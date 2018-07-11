library(reticulate)

train_x <- aplication_train_df %>% select(-TARGET,-SK_ID_CURR) 
train_y <- aplication_train_df %>% pull(TARGET)

validation_x <- aplication_validation_df %>% select(-TARGET,-SK_ID_CURR)
validation_y <- aplication_validation_df %>% pull(TARGET)

test_x <- aplication_test_df %>% select(-SK_ID_CURR)


source_python("lightGBM.py")
fit_gbm <- lightGBM(train_x,train_y,validation_x,validation_y)
