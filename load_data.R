library(tidyverse)
library(stringr)
library(janitor)

#From janitor
clean_str <- function (dat) 
{
  dat %>% 
    gsub("'", "", .) %>% 
    gsub("\"","", .) %>% 
    gsub("%", "percent", .) %>% 
    gsub("^[ ]+","", .) %>%
    make.names(.) %>% 
    gsub("[.]+", "_", .) %>% 
    gsub("[_]+", "_", .) %>% 
    gsub("_$", "", .)
}


# https://gist.github.com/EmilHvitfeldt/482412fa4ea7246a11c7b7a8504b7997
one_hot <- function(data, var) {
  
  var_enquo <- enquo(var)
  items <- data %>% pull(!!var_enquo)
  items_unique <- items %>% unique()
  
  out <- matrix(0L, NROW(data), length(items_unique))
  colnames(out) <- items_unique
  
  for (i in items_unique) {
    out[, i] <- items == i
  }
  
  data %>%
    select(-!!var_enquo) %>%
    bind_cols(as.tibble(out))
}

one_hot_names <- function(data,var){
  var_enquo <- enquo(var)
  var_sym <- ensym(var)
  
  data %>% 
    mutate(!!var_sym := paste0(var_sym,"_",!!var_enquo %>% clean_str %>% str_to_upper)) %>% 
    one_hot(!!var_enquo)
}

spread_multiple <- function(df, key, value, ...) {
  # quote key
  keyq <- rlang::enquo(key)
  keysq <- rlang::quos(!!keyq)
  # break value vector into quotes
  valueq <- rlang::enquo(value)
  valuesq <- rlang::quos(!!valueq)
  if((df %>% ungroup %>% select(!!!valuesq) %>% ncol) > 1){
    df %>% gather(variable, value, !!!valuesq) %>%
      unite(temp, !!!keysq, variable) %>%
      spread(temp, value, ...)
  } else{
    df %>%
      unite(temp, !!!keysq) %>%
      spread(temp, !!valueq, ...)
  }
}

# 01 - Load data -----------------------
#_______________________________________

aplication_train <- read_csv("raw_data/application_train.csv.zip")
aplication_test <- read_csv("raw_data/application_test.csv.zip")

bureau <- read_csv("raw_data/bureau.csv.zip")
bureau_balance <- read_csv("raw_data/bureau_balance.csv.zip")

previous_application <- read_csv("raw_data/previous_application.csv.zip")
instalments_payments <- read_csv("raw_data/installments_payments.csv.zip")
pos_cash_balance <- read_csv("raw_data/POS_CASH_balance.csv.zip")
credit_card_balance <- read_csv("raw_data/credit_card_balance.csv.zip")

metadata <- read_csv("raw_data/HomeCredit_columns_description.csv")
  
# 02 - Bureau Balance ------------------
#_______________________________________

metadata %>% filter(Table %>% str_detect("bureau_balance"),Row == "STATUS") %>% pull(Description) 

# First EDA
bureau_balance %>% tabyl(STATUS)
bureau_balance %>% filter(SK_ID_BUREAU == 5230570) %>% print(n=100)

# We are interested in knowing how many DPD and how much time since then.
# We ignore status closed TODO
# EARLIEST_MONTHS_BALANCE TODO 

bureau_balance_df <- bureau_balance %>% 
  mutate(STATUS = ifelse(STATUS == "C",0,
                         ifelse(STATUS == "X",NA,STATUS))) %>% 
  group_by(SK_ID_BUREAU) %>% 
  summarise(MAX_STATUS = max(STATUS,na.rm=T),
            EARLIEST_MONTHS_BALANCE = max(ifelse(STATUS > 0, MONTHS_BALANCE,NA),na.rm=T),
            MONTHS_BAD_STATUS = sum(as.integer(as.logical(as.numeric(STATUS))),na.rm=T)) %>% 
  mutate(EARLIEST_MONTHS_BALANCE = ifelse(EARLIEST_MONTHS_BALANCE == -Inf,NA,EARLIEST_MONTHS_BALANCE))

bureau_balance_df <- bureau_balance %>% 
  mutate(STATUS = ifelse(STATUS == "C",0,
                         ifelse(STATUS == "X",NA,STATUS)),
        period = ifelse(MONTHS_BALANCE >= -12,"MONTHS_BAD_STATUS_SHORT",
                                ifelse(MONTHS_BALANCE >= -48,"MONTHS_BAD_STATUS_MID","MONTHS_BAD_STATUS_LONG"))) %>% 
  group_by(SK_ID_BUREAU,period) %>% 
  summarise(MONTHS_BAD_STATUS = sum(as.integer(as.logical(as.numeric(STATUS))),na.rm=T)) %>% 
    spread(period,MONTHS_BAD_STATUS,fill = 0)


# 77% of loans have been paid with no due
bureau_balance_df %>% tabyl(EARLIEST_MONTHS_BALANCE)


# 03 - Bureau -------------------------
#_______________________________________

metadata %>% filter(Table %>% str_detect("bureau.csv"))
# First EDA
bureau %>% tabyl(CREDIT_ACTIVE)
bureau %>% tabyl(CREDIT_CURRENCY)
# Study credit type and split into columns. Cluster types? TODO
bureau %>% tabyl(CREDIT_TYPE)

bureau %>% select(contains("AMT_CREDIT_SUM"),CREDIT_ACTIVE, SK_ID_BUREAU) %>% 
  filter(AMT_CREDIT_SUM_OVERDUE > 0)

bureau_balance %>% filter(SK_ID_BUREAU == 5714936)
# bureau_balance %>% distinct(SK_ID_BUREAU) %>% semi_join(bureau)


bureau %>% select(contains("AMT_CREDIT_SUM"),CREDIT_ACTIVE, SK_ID_BUREAU) %>% 
  # filter(AMT_CREDIT_SUM_DEBT > AMT_CREDIT_SUM)
  # filter(AMT_CREDIT_SUM_LIMIT > AMT_CREDIT_SUM)
  filter(AMT_CREDIT_SUM_DEBT < 0)

bureau %>% select(CREDIT_DAY_OVERDUE) %>% filter(CREDIT_DAY_OVERDUE >0)



bureau_summary_df <- bureau %>% 
  mutate(AMT_CREDIT_SUM_DEBT_GT_SUM = as.integer(AMT_CREDIT_SUM_DEBT > AMT_CREDIT_SUM),
         AMT_CREDIT_SUM_LIMIT_GT_SUM= as.integer(AMT_CREDIT_SUM_LIMIT > AMT_CREDIT_SUM),
         AMT_CREDIT_SUM_DEBT_LT_0 = as.integer(AMT_CREDIT_SUM_DEBT < 0),
         CREDIT_DAY_OVERDUE_GT_0 = as.integer(CREDIT_DAY_OVERDUE > 0)) %>% 
  left_join(bureau_balance_df) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise(NUMBER_CREDITS_BUREAU = n(),
            MONTHS_BAD_STATUS_MEAN_BUREAU = mean(MONTHS_BAD_STATUS,na.rm=T),
            MONTHS_BAD_STATUS_MAX_BUREAU = max(MONTHS_BAD_STATUS,na.rm=T),
            AMT_CREDIT_SUM_DEBT_GT_SUM_BUREAU = sum(AMT_CREDIT_SUM_DEBT_GT_SUM,na.rm = T),
            AMT_CREDIT_SUM_LIMIT_GT_SUM_BUREAU = sum(AMT_CREDIT_SUM_LIMIT_GT_SUM,na.rm = T),
            AMT_CREDIT_SUM_DEBT_LT_0_BUREAU = sum(AMT_CREDIT_SUM_DEBT_LT_0,na.rm = T),
            CREDIT_DAY_OVERDUE_GT_0_BUREAU = sum(CREDIT_DAY_OVERDUE_GT_0,na.rm = T),
            MAX_STATUS_BUREAU = max(MAX_STATUS,na.rm=T),
            EARLIEST_MONTHS_BALANCE_MIN_BUREAU = min(EARLIEST_MONTHS_BALANCE,na.rm=T),
            EARLIEST_MONTHS_BALANCE_MAX_BUREAU = max(EARLIEST_MONTHS_BALANCE,na.rm=T),
            DAYS_CREDIT_MIN_BUREAU = min(DAYS_CREDIT, na.rm = T),
            CREDIT_DAY_OVERDUE_MAX_BUREAU = max(CREDIT_DAY_OVERDUE, na.rm = T),
            DAYS_CREDIT_MIN_BUREAU = min(DAYS_CREDIT,na.rm=T),
            DAYS_CREDIT_MAX_BUREAU = max(DAYS_CREDIT,na.rm=T),
            DAYS_CREDIT_ENDDATE_MIN_BUREAU = min(DAYS_CREDIT_ENDDATE,na.rm=T),
            DAYS_CREDIT_ENDDATE_MAX_BUREAU = max(DAYS_CREDIT_ENDDATE,na.rm=T),
            DAYS_ENDDATE_FACT_MIN_BUREAU = min(DAYS_ENDDATE_FACT,na.rm=T),
            DAYS_ENDDATE_FACT_MAX_BUREAU = max(DAYS_ENDDATE_FACT,na.rm=T),
            DAYS_CREDIT_UPDATE_MIN_BUREAU = min(DAYS_CREDIT_UPDATE,na.rm=T),
            DAYS_CREDIT_UPDATE_MAX_BUREAU = max(DAYS_CREDIT_UPDATE,na.rm=T),
            AMT_CREDIT_MAX_OVERDUE_SUM_BUREAU = sum(AMT_CREDIT_MAX_OVERDUE, na.rm = T),
            CNT_CREDIT_PROLONG_BUREAU = sum(CNT_CREDIT_PROLONG, na.rm = T),
            AMT_CREDIT_SUM_SUM_BUREAU = sum(AMT_CREDIT_SUM,na.rm=T),
            AMT_CREDIT_SUM_DEBT_SUM_BUREAU = sum(AMT_CREDIT_SUM_DEBT, na.rm = T),
            AMT_CREDIT_SUM_LIMIT_SUM_BUREAU = sum(AMT_CREDIT_SUM_LIMIT, na.rm = T),
            AMT_CREDIT_SUM_OVERDUE_SUM_BUREAU = sum(AMT_CREDIT_SUM_OVERDUE, na.rm = T),
            AMT_ANNUITY_SUM_BUREAU = sum(AMT_ANNUITY, na.rm = T)) %>% 
  mutate(EARLIEST_MONTHS_BALANCE_MAX_BUREAU = ifelse(EARLIEST_MONTHS_BALANCE_MAX_BUREAU == -Inf,NA,EARLIEST_MONTHS_BALANCE_MAX_BUREAU),
         EARLIEST_MONTHS_BALANCE_MIN_BUREAU = ifelse(EARLIEST_MONTHS_BALANCE_MIN_BUREAU == Inf,NA,EARLIEST_MONTHS_BALANCE_MIN_BUREAU))


bureau_summary_df <- bureau %>% 
  mutate(AMT_CREDIT_SUM_DEBT_GT_SUM = as.integer(AMT_CREDIT_SUM_DEBT > AMT_CREDIT_SUM),
         AMT_CREDIT_SUM_LIMIT_GT_SUM= as.integer(AMT_CREDIT_SUM_LIMIT > AMT_CREDIT_SUM),
         AMT_CREDIT_SUM_DEBT_LT_0 = as.integer(AMT_CREDIT_SUM_DEBT < 0),
         CREDIT_DAY_OVERDUE_GT_0 = as.integer(CREDIT_DAY_OVERDUE > 0)) %>% 
  left_join(bureau_balance_df) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_if(is.numeric,
               funs(N =n(), 
                     MAX_BUREAU = max(.,na.rm = T),
                     MIN_BUREAU = min(.,na.rm = T),
                     SUM_BUREAU = sum(.,na.rm = T),
                     MEAN_BUREAU = mean(.,na.rm=T))) %>% 
  mutate_at(vars(ends_with("MAX_BUREAU")), funs(ifelse(. == -Inf,NA,.))) %>% 
  mutate_at(vars(ends_with("MIN_BUREAU")), funs(ifelse(. == Inf,NA,.)))



bureau_type_df <- bureau %>% 
  mutate(CREDIT_ACTIVE = paste0("CREDIT_ACTIVE_",CREDIT_ACTIVE %>% clean_str %>% str_to_upper)) %>% 
  one_hot(CREDIT_ACTIVE) %>% 
  mutate(CREDIT_TYPE = paste0("CREDIT_TYPE_",CREDIT_TYPE %>% clean_str %>% str_to_upper)) %>% 
  one_hot(CREDIT_TYPE) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_at(vars(starts_with("CREDIT_ACTIVE_"),
                    starts_with("CREDIT_TYPE")),
               funs(sum))
  


# 04 - POS Cash Balance ----------------
#_______________________________________
metadata %>% filter(Table %>% str_detect("POS"))


pos_cash_balance %>% filter(SK_ID_PREV == 2371489) %>% arrange(MONTHS_BALANCE) %>% print(n=100)
pos_cash_balance %>% filter(SK_ID_PREV == 2167174) %>% arrange(MONTHS_BALANCE) %>% print(n=100)

pos_cash_balance %>% filter(SK_DPD_DEF > 0)
pos_cash_balance %>% tabyl(NAME_CONTRACT_STATUS)

# First month with DPD TODO

pos_cash_balance_previous_df <- pos_cash_balance %>% 
  mutate(CONTRACT_COMPLETED = ifelse(NAME_CONTRACT_STATUS == "Completed",1L,0L)) %>% 
  mutate(period = ifelse(MONTHS_BALANCE >= -12,"SHORT",
                         ifelse(MONTHS_BALANCE >= -48,"MID","LONG"))) %>% 
  group_by(SK_ID_PREV,SK_ID_CURR,period) %>% 
summarise(LAST_MONTHS_BALANCE = max(MONTHS_BALANCE, na.rm = T),
          CNT_INSTALMENT_MAX = max(CNT_INSTALMENT, na.rm = T),
          LAST_CNT_INSTALMENT_FUTURE = min(CNT_INSTALMENT_FUTURE, na.rm = T),
          SK_DPD_MAX = max(SK_DPD, na.rm = T),
          SK_DPD_DEF_MAX = max(SK_DPD_DEF, na.rm = T),
          CONTRACT_COMPLETED = sum(CONTRACT_COMPLETED, na.rm = T)) %>% 
  gather("variable","valor", -SK_ID_PREV,-SK_ID_CURR,-period) %>% 
  unite("variable",variable,period,sep="_",remove = T) %>% 
  spread(variable,valor,fill= 0)


pos_cash_balance_df <- pos_cash_balance_previous_df %>% 
  ungroup() %>% 
  select(-SK_ID_PREV) %>% 
  mutate(CONTRACT_IS_COMPLETED = ifelse(CONTRACT_COMPLETED_SHORT == 1 | CONTRACT_COMPLETED_MID == 1 | CONTRACT_COMPLETED_LONG == 1,"CONTRACT_COMPLETED","CONTRACT_UNCOMPLETED")) %>% 
  # select(-contains("CONTRACT_COMPLETED")) %>% 
  one_hot(CONTRACT_IS_COMPLETED) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(sum(., na.rm=T),n()))
  # summarise(NUMBER_PREVIOUS_CONTRACTS = n(),
  #           NUMBER_PREVIOUS_CONTRACTS_COMPLETED = sum(CONTRACT_COMPLETED),
  #           NUMBER_PREVIOUS_CONTRACTS_UNCOMPLETED = sum(CONTRACT_UNCOMPLETED),
  #           CNT_INSTALMENT_PREVIOUS_CONTRACTS = sum(CNT_INSTALMENT_MAX, na.rm = T),
  #           SK_DPD_PREVIOUS_CONTRACTS= sum(SK_DPD_MAX, na.rm = T),
  #           SK_DPD_DEF_PREVIOUS_CONTRACTS= sum(SK_DPD_DEF_MAX, na.rm = T)
  #           )

# aplication_train %>% select(SK_ID_CURR,TARGET) %>% left_join(bureau) %>% 
#   ggplot(aes(x=NUMBER_PREVIOUS_CONTRACTS,fill=TARGET,group=TARGET)) +
#   geom_histogram()+
#   facet_grid(TARGET~.,scales = "free_y")



# 05 - Instalments payments -----------
#_______________________________________
metadata %>% filter(Table %>% str_detect("instal")) %>% View
instalments_payments %>% filter(SK_ID_PREV == 2234264) %>%  arrange(NUM_INSTALMENT_NUMBER)
instalments_payments %>% filter(NUM_INSTALMENT_VERSION > 2)

instalments_payments_previous_df <- instalments_payments %>% 
  group_by(SK_ID_CURR,SK_ID_PREV) %>% 
  summarise(CHANGES_INSTALMENT_VERSION = max(NUM_INSTALMENT_VERSION),
            AMT_INSTALMENT = mean(AMT_INSTALMENT),
            AMT_PAYMENT = mean(AMT_PAYMENT))

instalments_payments_df <- instalments_payments_previous_df %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_at(vars(CHANGES_INSTALMENT_VERSION,
                    AMT_INSTALMENT,
                    AMT_PAYMENT),
               funs(SUM = sum(., na.rm=T),MEAN = mean(., na.rm=T)))


# 06 - Credit card balance -----------
#_______________________________________

metadata %>% filter(Table %>% str_detect("credit")) %>% View

credit_card_balance %>% filter(SK_ID_PREV == 2582071) %>% arrange(MONTHS_BALANCE) %>% View

credit_card_balance %>% tabyl(NAME_CONTRACT_STATUS) %>% mutate(percent = round(percent,2))

# Filter only NAME_CONTRACT_STATUS == "Active" ?
credit_card_balance_previous_df <- credit_card_balance %>% 
  # group_by(SK_ID_PREV,SK_ID_CURR) %>% 
  mutate(period = ifelse(MONTHS_BALANCE >= -12,"SHORT",
                         ifelse(MONTHS_BALANCE >= -48,"MID","LONG"))) %>% 
  group_by(SK_ID_PREV,SK_ID_CURR,period) %>% 
  summarise(AMT_BALANCE = mean(AMT_BALANCE),
            AMT_CREDIT_LIMIT_ACTUAL = mean(AMT_CREDIT_LIMIT_ACTUAL),
            AMT_DRAWINGS_ATM_CURRENT = mean(AMT_DRAWINGS_ATM_CURRENT),
            AMT_DRAWINGS_CURRENT = mean(AMT_DRAWINGS_CURRENT),
            AMT_DRAWINGS_OTHER_CURRENT = mean(AMT_DRAWINGS_OTHER_CURRENT),
            AMT_DRAWINGS_POS_CURRENT = mean(AMT_DRAWINGS_POS_CURRENT),
            AMT_INST_MIN_REGULARITY = mean(AMT_INST_MIN_REGULARITY),
            AMT_PAYMENT_CURRENT = mean(AMT_PAYMENT_CURRENT),
            AMT_PAYMENT_TOTAL_CURRENT = mean(AMT_PAYMENT_TOTAL_CURRENT),
            AMT_RECEIVABLE_PRINCIPAL = mean(AMT_RECEIVABLE_PRINCIPAL),
            AMT_RECIVABLE = mean(AMT_RECIVABLE),
            AMT_TOTAL_RECEIVABLE = mean(AMT_TOTAL_RECEIVABLE),
            CNT_DRAWINGS_ATM_CURRENT = mean(CNT_DRAWINGS_ATM_CURRENT),
            CNT_DRAWINGS_CURRENT = mean(CNT_DRAWINGS_CURRENT),
            CNT_DRAWINGS_OTHER_CURRENT = mean(CNT_DRAWINGS_OTHER_CURRENT),
            CNT_DRAWINGS_POS_CURRENT = mean(CNT_DRAWINGS_POS_CURRENT),
            SK_DPD = mean(SK_DPD),
            SK_DPD_DEF = mean(SK_DPD_DEF)) %>% 
 gather("variable","valor", -SK_ID_PREV,-SK_ID_CURR,-period) %>% 
  unite("variable",variable,period,sep="_",remove = T) %>% 
  spread(variable,valor)
 
               
               
credit_card_balance_df <- credit_card_balance_previous_df %>% 
  group_by(SK_ID_CURR) %>% 
  select(-SK_ID_PREV) %>% 
  summarise_all(sum, na.rm = T)
  # summarise(AMT_BALANCE = sum(AMT_BALANCE),
  #           AMT_CREDIT_LIMIT_ACTUAL = sum(AMT_CREDIT_LIMIT_ACTUAL),
  #           AMT_DRAWINGS_ATM_CURRENT = sum(AMT_DRAWINGS_ATM_CURRENT),
  #           AMT_DRAWINGS_CURRENT = sum(AMT_DRAWINGS_CURRENT),
  #           AMT_DRAWINGS_OTHER_CURRENT = sum(AMT_DRAWINGS_OTHER_CURRENT),
  #           AMT_DRAWINGS_POS_CURRENT = sum(AMT_DRAWINGS_POS_CURRENT),
  #           AMT_INST_MIN_REGULARITY = sum(AMT_INST_MIN_REGULARITY),
  #           AMT_PAYMENT_CURRENT = sum(AMT_PAYMENT_CURRENT),
  #           AMT_PAYMENT_TOTAL_CURRENT = sum(AMT_PAYMENT_TOTAL_CURRENT),
  #           AMT_RECEIVABLE_PRINCIPAL = sum(AMT_RECEIVABLE_PRINCIPAL),
  #           AMT_RECIVABLE = sum(AMT_RECIVABLE),
  #           AMT_TOTAL_RECEIVABLE = sum(AMT_TOTAL_RECEIVABLE),
  #           CNT_DRAWINGS_ATM_CURRENT = sum(CNT_DRAWINGS_ATM_CURRENT),
  #           CNT_DRAWINGS_CURRENT = sum(CNT_DRAWINGS_CURRENT),
  #           CNT_DRAWINGS_OTHER_CURRENT = sum(CNT_DRAWINGS_OTHER_CURRENT),
  #           CNT_DRAWINGS_POS_CURRENT = sum(CNT_DRAWINGS_POS_CURRENT),
  #           SK_DPD = sum(SK_DPD),
  #           SK_DPD_DEF = sum(SK_DPD_DEF))
    
  

# 07 - Previous application -----------
#_______________________________________
previous_application %>% 
  select_if(is.character)

         

previous_application %>%   
  tabyl(PRODUCT_COMBINATION)

previous_application %>%   
  mutate(NAME_PAYMENT_TYPE = NAME_PAYMENT_TYPE %>% str_extract("^[\\w\\-]+")) %>% 
  tabyl(NAME_PAYMENT_TYPE)

previous_application_prev_df <- previous_application %>% 
  one_hot_names(NAME_CONTRACT_TYPE) %>% 
  one_hot_names(NAME_CASH_LOAN_PURPOSE) %>% 
  one_hot_names(NAME_CONTRACT_STATUS) %>% 
  mutate(NAME_PAYMENT_TYPE = NAME_PAYMENT_TYPE %>% str_extract("^[\\w\\-]+")) %>% 
  one_hot_names(NAME_PAYMENT_TYPE) %>% 
  one_hot_names(NAME_PORTFOLIO) %>% 
  one_hot_names(NAME_PRODUCT_TYPE) %>% 
  one_hot_names(NAME_SELLER_INDUSTRY) %>% 
  one_hot_names(NAME_YIELD_GROUP) %>% 
  one_hot_names(PRODUCT_COMBINATION) %>% 
  mutate(APP_CREDIT_PERC = AMT_APPLICATION / AMT_CREDIT) %>% 
  mutate_at(vars(starts_with("DAYS")),
            funs(ifelse(. == 365243, NA, .)))
  

previous_application_df <- previous_application_prev_df %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_if(is.numeric, funs(SUM = sum(.),N= n())) %>% 
  select(SK_ID_CURR,
         SK_ID_PREV_N,
         ends_with("_SUM"),
         -SK_ID_PREV_SUM
         ) %>% 
  rename(NUMBER_PREVIOUS_APPLICATIONS = SK_ID_PREV_N)




# 08 - Join data  ----------------------
#_______________________________________
aplication_train %>% tabyl(FONDKAPREMONT_MODE)

aplication <- aplication_train %>% 
  bind_rows(aplication_test)

aplication_train %>% select_if(is.character) %>% names

aplication <- aplication %>% 
  one_hot_names(NAME_CONTRACT_TYPE) %>% 
  one_hot_names(CODE_GENDER) %>% 
  one_hot_names(NAME_TYPE_SUITE) %>% 
  one_hot_names(NAME_INCOME_TYPE) %>% 
  one_hot_names(NAME_EDUCATION_TYPE) %>% 
  one_hot_names(NAME_FAMILY_STATUS) %>% 
  one_hot_names(NAME_HOUSING_TYPE) %>% 
  one_hot_names(OCCUPATION_TYPE)

ls() %>% str_subset("_df$")

# PAYMENT_RATE https://www.kaggle.com/jsaguiar/updated-0-792-lb-lightgbm-with-simple-features/comments

aplication_df <- aplication %>% 
  mutate_at(vars(starts_with("DAYS")), funs(ifelse(.==365243,NA,.))) %>% 
  mutate(PAYMENT_RATE = AMT_ANNUITY/AMT_CREDIT) %>% 
  left_join(previous_application_df) %>% 
  left_join(credit_card_balance_df) %>% 
  left_join(pos_cash_balance_df) %>% 
  left_join(instalments_payments_df) %>% 
  left_join(bureau_summary_df) %>% 
  left_join(bureau_type_df)

# 09 - Feature engineering -----------
#_______________________________________


aplication_df <- aplication_df %>% 
  mutate_at(vars(contains("_MID"), contains("_SHORT"),contains("_LONG")),
                            funs(as.integer(as.logical(.))))

aplication_df <- aplication_df %>% 
  select(-contains("FLAG_DOCUMENT")) %>%
  mutate(EXT_SOURCES_MEAN = (EXT_SOURCE_1 + EXT_SOURCE_2 + EXT_SOURCE_3)/3,
         EXT_SOURCES_PROD = (EXT_SOURCE_1 * EXT_SOURCE_2 * EXT_SOURCE_3),
         CREDIT_ANNUITY_RATIO = AMT_CREDIT_SUM / AMT_ANNUITY,
         CREDIT_GOODS_RATIO = AMT_CREDIT_SUM / AMT_GOODS_PRICE,
         DAYS_EMPLOYED_BIRTH_RATIO = DAYS_EMPLOYED /DAYS_BIRTH
         # MAX_STATUS_BUREAU = as.integer(MAX_STATUS_BUREAU)
         # MAX_STATUS_BAD_TIME = MAX_STATUS_BUREAU * (96 + EARLIEST_MONTHS_BALANCE_MAX_BUREAU)
         ) %>% 
  group_by(TARGET) %>% 
  # mutate_at(vars(starts_with("CREDIT_ACTIVE"),
  #                starts_with("CREDIT_TYPE"),
  #                starts_with("CNT_CHILDREN"),
  #                starts_with("CNT_FAM_MEMBERS")
  #                ), funs(as.integer(as.logical(.))))
  mutate_if(is.numeric,
            funs(ifelse(is.na(.),0,.))) %>% 
  ungroup()
  

# 09 - Application train/test -----------
#_______________________________________
aplication_test_df <- aplication_df %>% 
  filter(is.na(TARGET))

set.seed(42)

aplication_train_df <- aplication_df %>% 
  filter(!is.na(TARGET)) %>% 
  group_by(TARGET) %>% 
  sample_frac(0.8) %>% 
  ungroup()

aplication_validation_df <- aplication_df %>% 
  filter(!is.na(TARGET)) %>% 
  setdiff(aplication_train_df)

#UPSAMPLING TRAINING: 70/30:

target_1 <- aplication_train_df %>% 
  mutate(row_number = row_number()) %>% 
  filter(TARGET == 1) %>% 
  pull(row_number)

factor_sampling_1 <- proportion_sampling(aplication_train_df,0.3)

aplication_train_df <- aplication_train_df %>% 
  filter(TARGET == 0) %>% 
  bind_rows(
    aplication_train_df %>% 
      dplyr::slice(sample(target_1,round(length(target_1) * factor_sampling_1,0), replace = T))
  )

proportion_sampling <- function(df,x){
  df_n <- df %>% group_by(TARGET) %>% summarise(n = n())
  n_0 <- df_n %>% filter(TARGET == 0) %>% pull(n)
  n_1 <- df_n %>% filter(TARGET == 1) %>% pull(n)
  return(x*n_0 / (n_1 * (1-x)))
}

aplication_train_df %>% group_by(TARGET) %>% summarise(n = n())
