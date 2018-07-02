library(xgboost)
library(plotROC)


train_matrix <- xgb.DMatrix(data = aplication_train_df %>% select(-TARGET,-SK_ID_CURR) %>% data.matrix(), 
                            label = aplication_train_df %>% pull(TARGET))
validation_matrix <- xgb.DMatrix(data = aplication_validation_df %>% select(-TARGET,-SK_ID_CURR) %>% data.matrix(), 
                            label = aplication_validation_df %>% pull(TARGET))
test_matrix <- xgb.DMatrix(data = aplication_test_df %>% select(-TARGET,-SK_ID_CURR) %>% data.matrix())

cols <- aplication_validation_df %>% select(-TARGET,-SK_ID_CURR) %>% names



p <- list(objective = "binary:logistic",
          booster = "gbtree",
          eval_metric = "auc",
          nthread = 15,
          eta = 0.025,
          max_depth = 6,
          min_child_weight = 19,
          gamma = 0,
          subsample = 0.8,
          colsample_bytree = 0.632,
          alpha = 0,
          lambda = 0.05,
          nrounds = 3000)
m_xgb <- xgb.train(p, train_matrix, p$nrounds, list(val = validation_matrix), print_every_n = 50, early_stopping_rounds = 200)

xgb.importance(cols, model=m_xgb) %>% 
  xgb.plot.importance(top_n = 30)

xgb.importance(cols, model=m_xgb) %>% 
  mutate(rank = row_number()) %>% 
  filter(Feature %>% str_detect("MONTHS"))

# data_frame(d=aplication_validation_df %>% pull(TARGET),m=predict(m_xgb, validation_matrix)) %>%
#   ggplot(aes(d=d,m=m)) %>%
#   geom_roc

pROC::roc(aplication_validation_df %>% pull(TARGET),
               predict(m_xgb, validation_matrix)) %>% pROC::ci.thresholds(c(seq(0.1,0.9,by=0.1)))

aplication_test_df %>% 
  select(SK_ID_CURR) %>% 
  mutate(SK_ID_CURR = as.integer(SK_ID_CURR),
         TARGET = predict(m_xgb, test_matrix)) %>%
  write_csv(paste0("predictions/xgb_parcial_", round(m_xgb$best_score, 4), ".csv"))
