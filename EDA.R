library(fs)
is_dummy <- function(x) {
  all(x %in% c(0,1))
}

is_discrete <- function(x){
  # all(is.numeric(x)) & ((x %>% unique() %>% length) < max(10,length(x)/1000))
  all(is.numeric(x)) & ((x %>% unique() %>% length) < 100)
}

is_continuous <- function(x){
  all(is.numeric(x)) & !(is.discrete(x))
}

is_discrete(1:10)
aplication_train_df %>% select_if(is_dummy)
aplication_train_df %>% select_if(funs(is_discrete(.) & !is_dummy(.)))

aplication_train_df %>% pull(CREDIT_TYPE_MOBILE_OPERATOR_LOAN) %>% unique %>% length

proportion_dummy <- function(data,var_target,var){
  var_target_quo <- enquo(var_target)
  var_target_name <- ensym(var_target)
  var_quo <- enquo(var)
  var_name <- ensym(var)
  var_label <- sym("label")
  
  data %>% 
    mutate(prop = mean(!!var_target_quo)) %>% 
    group_by(!!var_quo) %>% 
    summarise(prop = first(prop),
              TARGET = sum(!!var_target_quo)/n()) %>% 
    mutate(!!var_name := as.factor(!!var_quo),
           label = round(TARGET *100,0)) %>%
    ggplot(aes_(group = var_name,fill=var_name,x = var_name,y = var_target_name))+
    geom_col() +
    # geom_text(aes_(label = round(var_target_name,2),color = var_name),vjust = -1) 
    geom_text(aes_(label = var_label,color = var_name),vjust = -1) +
    geom_hline(aes(yintercept = prop),linetype= "dashed", color = "darkgrey") +
    geom_text(aes(label = round(prop*100,0), y = prop),x = 0.5,color = "darkgrey",vjust = -1) +
    ylim(c(0,1))+
    theme(legend.position = "none",
          axis.title.x = ,
          axis.title.y = )
  
}

proportion_dummy(aplication_train_df,TARGET,FLAG_EMAIL)
proportion_dummy(aplication_train_df,"FLAG_EMAIL")
proportion_dummy(aplication_train_df,TARGET,FLAG_DOCUMENT_2)
proportion_dummy(aplication_train_df,"TARGET","FLAG_DOCUMENT_2")
proportion_dummy(aplication_train_df,"TARGET","FLAG_OWN_REALTY")

var_target <- "TARGET"
var <- "FLAG_DOCUMENT_2"

# One by one:
discrete_variable_plot <- function(df,var_target, var){
  
  var_sym <- sym(var)
  var_target_s <- sym(var_target)
  
df %>% 
  select(one_of(var_target,var)) %>% 
  mutate(prop = mean(!!var_target_s)) %>% 
  group_by(!!var_sym) %>% 
  summarise(prop = first(prop),
            COUNT = n(),
            PROPORTION = sum(!!var_target_s)/n()) %>% 
  mutate(var := as.factor(!!var_sym),
         label = round(PROPORTION *100,0)) %>% 
  gather("variable","valor",COUNT,PROPORTION) %>% 
  mutate(label = ifelse(variable == "COUNT",valor,label),
         prop = ifelse(variable == "COUNT",NA,prop)) %>% 
  ggplot(aes(group = var,fill = var,x = var,y = valor))+
  geom_col()+
  geom_text(aes(label = label ,color = var),vjust = -1) +
  facet_wrap(~variable,scales="free_y") +
  geom_hline(aes(yintercept = prop),linetype= "dashed", color = "darkgrey") +
  geom_text(aes(label = round(prop*100,0), y = prop),x = 0.5,color = "darkgrey",vjust = -1) +
  ggtitle(var) +
  theme(legend.position = "none",
        title = element_text(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
}

# One by one:
continous_variable_plot <- function(df,var_target, var){
  
  var_sym <- sym(var)
  var_target_s <- sym(var_target)
  
  # df %>% 
  #   select(one_of(var_target,var)) %>% 
  #   mutate(TARGET = as.factor(!!var_target_s)) %>% 
  #   gather("variable","valor",-TARGET) %>% 
  #   ggplot(aes(group = TARGET,color = TARGET,x = valor))+
  #   geom_density()+
  #   ggtitle(var) +
  #   theme(title = element_text(),
  #         axis.title.x = element_blank(),
  #         axis.title.y = element_blank())
  
  
  data <- aplication_train_df %>% 
    select(one_of(var_target,var)) %>% 
    mutate(TARGET = as.factor(TARGET)) %>% 
    mutate(IQR = remove_outliers_IQR(!!var_sym),
           LOG = log1p(!!var_sym)) %>% 
    gather("variable","valor",-TARGET) 
  
  var_original <- data %>% 
    filter(variable == var) %>% 
    pull(valor)
    na_prop <- sum(is.na(var_original))*100/length(var_original) 
  
  density <- data %>% 
    filter(variable == var) %>% 
    ggplot(aes(group = TARGET,color = TARGET,x = valor))+
    geom_density()+
    ggtitle(paste0(var,"   (NA proportion: ", round(na_prop,0),"%)")) +
    facet_wrap(~variable,scales="free",ncol = 2)+
    theme(title = element_text(),
          legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
  other <- data %>% 
    filter(variable != var) %>% 
    ggplot(aes(group = TARGET,color = TARGET,x = valor))+
    geom_density()+
    facet_wrap(~variable,scales="free",ncol = 2)+
    theme(title = element_text(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
  
  grid.arrange(density, other, ncol = 1) 
  
}

continous_variable_plot(aplication_train_df,"TARGET","AMT_ANNUITY")
continous_variable_plot(aplication_train_df,"TARGET","AMT_CREDIT")

library(gridExtra)
aplication_train_df %>% 
  select(TARGET,AMT_ANNUITY) %>% 
  mutate(TARGET = as.factor(TARGET)) %>% 
  mutate(IQR = remove_outliers_IQR(AMT_ANNUITY),
         LOG = log1p(AMT_ANNUITY)) %>% 
  gather("variable","valor",-TARGET) %>% 
  ggplot(aes(group = TARGET,color = TARGET,x = valor))+
  geom_density()+
  ggtitle(var) +
  facet_wrap(~variable,scales="free",ncol = 2)+
  theme(title = element_text(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())


  

remove_outliers_IQR <- function(x){
  lower <- max(min(x, na.rm=T),quantile(x,0.25,na.rm=T) - 1.5*IQR(x,na.rm=T))
  upper <- min(max(x, na.rm=T),quantile(x,0.25,na.rm=T) + 1.5*IQR(x,na.rm=T))
  
  x %>% map_dbl(~ifelse(between(.x,lower,upper),.x,NA))
  
}
remove_outliers_IQR(x)
#Dummy variables::
aplication_train_df %>% 
  select_if(is_dummy) %>% 
  mutate(prop = mean(TARGET)) %>% 
  gather("variable","valor", -TARGET,-prop) %>% 
  group_by(variable,valor) %>% 
  summarise(prop = first(prop),
            TARGET = sum(TARGET)/n()) %>% 
  mutate(valor = as.factor(valor),
         label = round(TARGET *100,0)) %>% 
  ggplot(aes(group = valor,fill=valor,x = valor,y = TARGET))+
  geom_col() +
  geom_text(aes(label = label,color = valor),vjust = -1) +
  geom_hline(aes(yintercept = prop),linetype= "dashed", color = "darkgrey") +
  geom_text(aes(label = round(prop*100,0), y = prop),x = 0.5,color = "darkgrey",vjust = -1) +
  facet_wrap(~variable) +
  ylim(c(0,1))

#Discrete variables::
aplication_train_df %>% 
  group_by(TARGET) %>% 
  select_if(funs(is_discrete(.) & !is_dummy(.))) %>% 
  ungroup() %>% 
  mutate(prop = mean(TARGET)) %>% 
  gather("variable","valor", -TARGET,-prop) %>% 
  group_by(variable,valor) %>% 
  summarise(prop = first(prop),
            TARGET = sum(TARGET)/n()) %>% 
  mutate(valor = as.factor(valor),
         label = round(TARGET *100,0)) %>% 
  ggplot(aes(group = valor,fill=valor,x = valor,y = TARGET))+
  geom_col() +
  geom_text(aes(label = label,color = valor),vjust = -1) +
  geom_hline(aes(yintercept = prop),linetype= "dashed", color = "darkgrey") +
  geom_text(aes(label = round(prop*100,0), y = prop),x = 0.5,color = "darkgrey",vjust = -1) +
  facet_wrap(~variable,scales= "free_x") +
  theme(legend.position = "none")+
  ylim(c(0,1))


#Continuous variables::
aplication_train_df %>% 
  group_by(TARGET) %>% 
  select_if(funs(is.numeric(.) & !is_discrete(.))) %>% 
  ungroup() %>% 
  select(-SK_ID_CURR) %>% 
  mutate(TARGET = as.factor(TARGET)) %>% 
  gather("variable","valor", -TARGET) %>% 
  ggplot(aes(group = TARGET,color=TARGET,x = valor))+
  geom_density() +
  facet_wrap(~variable,scales= "free")


variables_df <- data_frame(variable_name = aplication_train_df %>% names) %>% 
  rowwise %>% 
  mutate(id = (variable_name == "SK_ID_CURR"),
         target = (variable_name == "TARGET"),
         dummy = map_lgl(variable_name, ~is_dummy(aplication_train_df %>% pull(.x))),
         discrete = map_lgl(variable_name, ~is_discrete(aplication_train_df %>% pull(.x))),
         character = map_lgl(variable_name, ~all(is_character(aplication_train_df %>% pull(.x)))),
         continuous = !id & !target & !dummy & !discrete & !character
  )



dir_create("EDA")         

variables_df %>% 
  filter(dummy | discrete | character) %>% 
  pull(variable_name) %>% 
  walk(function(x){
    discrete_variable_plot(aplication_train_df,"TARGET",x)
    ggsave(filename = paste0("EDA/",x,".png"))
  })

variables_df %>% 
  filter(continuous) %>% 
  pull(variable_name) %>% 
  walk(function(x){
    g <- continous_variable_plot(aplication_train_df,"TARGET",x)
    ggsave(filename = paste0("EDA/",x,".png"),g)
  })

aplication_train_df %>% tabyl(CREDIT_TYPE_MICROLOAN)
