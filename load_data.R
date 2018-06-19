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
            EARLIEST_MONTHS_BALANCE = max(MONTHS_BALANCE,na.rm=T))

# 77% of loans have been paid with no due
bureau_balance_df %>% tabyl(MAX_STATUS)


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
  summarise(NUMBER_CREDITS = n(),
            AMT_CREDIT_SUM_DEBT_GT_SUM = sum(AMT_CREDIT_SUM_DEBT_GT_SUM,na.rm = T),
            AMT_CREDIT_SUM_LIMIT_GT_SUM = sum(AMT_CREDIT_SUM_LIMIT_GT_SUM,na.rm = T),
            AMT_CREDIT_SUM_DEBT_LT_0 = sum(AMT_CREDIT_SUM_DEBT_LT_0,na.rm = T),
            CREDIT_DAY_OVERDUE_GT_0 = sum(CREDIT_DAY_OVERDUE_GT_0,na.rm = T),
            
            MAX_STATUS = max(MAX_STATUS,na.rm=T),
            EARLIEST_MONTS_BALANCE = min(EARLIEST_MONTHS_BALANCE,na.rm=T),
            
            DAYS_CREDIT_MIN = min(DAYS_CREDIT, na.rm = T),
            CREDIT_DAY_OVERDUE_MAX = max(CREDIT_DAY_OVERDUE, na.rm = T),
            
            DAYS_CREDIT_MIN = min(DAYS_CREDIT,na.rm=T),
            DAYS_CREDIT_MAX = max(DAYS_CREDIT,na.rm=T),
            
            DAYS_CREDIT_ENDDATE_MIN = min(DAYS_CREDIT_ENDDATE,na.rm=T),
            DAYS_CREDIT_ENDDATE_MAX = max(DAYS_CREDIT_ENDDATE,na.rm=T),
            
            DAYS_ENDDATE_FACT_MIN = min(DAYS_ENDDATE_FACT,na.rm=T),
            DAYS_ENDDATE_FACT_MAX = max(DAYS_ENDDATE_FACT,na.rm=T),
            
            DAYS_CREDIT_UPDATE_MIN = min(DAYS_CREDIT_UPDATE,na.rm=T),
            DAYS_CREDIT_UPDATE_MAX = max(DAYS_CREDIT_UPDATE,na.rm=T),
            
            AMT_CREDIT_MAX_OVERDUE_SUM = sum(AMT_CREDIT_MAX_OVERDUE, na.rm = T),
            CNT_CREDIT_PROLONG = sum(CNT_CREDIT_PROLONG, na.rm = T),
            AMT_CREDIT_SUM_SUM = sum(AMT_CREDIT_SUM,na.rm=T),
            AMT_CREDIT_SUM_DEBT_SUM = sum(AMT_CREDIT_SUM_DEBT, na.rm = T),
            AMT_CREDIT_SUM_LIMIT_SUM = sum(AMT_CREDIT_SUM_LIMIT, na.rm = T),
            AMT_CREDIT_SUM_OVERDUE_SUM = sum(AMT_CREDIT_SUM_OVERDUE, na.rm = T),
            AMT_ANNUITY_SUM = sum(AMT_ANNUITY, na.rm = T))



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
  group_by(SK_ID_PREV,SK_ID_CURR) %>% 
summarise(LAST_MONTHS_BALANCE = max(MONTHS_BALANCE, na.rm = T),
          CNT_INSTALMENT_MAX = max(CNT_INSTALMENT, na.rm = T),
          LAST_CNT_INSTALMENT_FUTURE = min(CNT_INSTALMENT_FUTURE, na.rm = T),
          SK_DPD_MAX = max(SK_DPD, na.rm = T),
          SK_DPD_DEF_MAX = max(SK_DPD_DEF, na.rm = T),
          CONTRACT_COMPLETED = sum(CONTRACT_COMPLETED))


pos_cash_balance_df <- pos_cash_balance_previous_df %>% 
  mutate(CONTRACT_IS_COMPLETED = ifelse(CONTRACT_COMPLETED == 1,"CONTRACT_COMPLETED","CONTRACT_UNCOMPLETED")) %>% 
  select(-CONTRACT_COMPLETED) %>% 
  one_hot(CONTRACT_IS_COMPLETED) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise(NUMBER_PREVIOUS_CONTRACTS = n(),
            NUMBER_PREVIOUS_CONTRACTS_COMPLETED = sum(CONTRACT_COMPLETED),
            NUMBER_PREVIOUS_CONTRACTS_UNCOMPLETED = sum(CONTRACT_UNCOMPLETED),
            CNT_INSTALMENT_PREVIOUS_CONTRACTS = sum(CNT_INSTALMENT_MAX, na.rm = T),
            SK_DPD_PREVIOUS_CONTRACTS= sum(SK_DPD_MAX, na.rm = T),
            SK_DPD_DEF_PREVIOUS_CONTRACTS= sum(SK_DPD_DEF_MAX, na.rm = T)
            )

aplication_train %>% select(SK_ID_CURR,TARGET) %>% left_join(bureau) %>% 
  ggplot(aes(x=NUMBER_PREVIOUS_CONTRACTS,fill=TARGET,group=TARGET)) +
  geom_histogram()+
  facet_grid(TARGET~.,scales = "free_y")



# 05 - Instalments payments -----------
#_______________________________________

instalments_payments %>% filter(SK_ID_PREV == 2234264) %>%  arrange(NUM_INSTALMENT_NUMBER)
instalments_payments %>% filter(NUM_INSTALMENT_VERSION > 2)

credit_card_balance
aplication_train
