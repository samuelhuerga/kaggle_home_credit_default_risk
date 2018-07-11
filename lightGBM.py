from lightgbm import LGBMClassifier

def lightGBM(train_x,train_y,validation_x,validation_y):
  
  # fit_params={"early_stopping_rounds":100, 
  #           "eval_metric" : 'auc', 
  #           "eval_set" : [(validation_x,validation_y)],
  #           'eval_names': ['valid'],
  #           'verbose': 100,
  #           'feature_name': 'auto', # that's actually the default
  #           'categorical_feature': 'auto' # that's actually the default
  #          }
  
  # clf = LGBMClassifier(
  #       n_estimators=4000,
  #       learning_rate=0.03,
  #       num_leaves=30,
  #       colsample_bytree=.8,
  #       subsample=.9,
  #       max_depth=7,
  #       reg_alpha=.1,
  #       reg_lambda=.1,
  #       min_split_gain=.01,
  #       min_child_weight=2,
  #       silent=-1,
  #       verbose=-1,
  #   )
  
  clf = LGBMClassifier(num_leaves= 15, max_depth=-1, 
                         random_state=314, 
                         silent=True, 
                         metric='auc', 
                         n_jobs=8, 
                         n_estimators=4000,
                         colsample_bytree=0.9,
                         subsample=0.9,
                         learning_rate=0.1)
    
  # clf.fit(train_x, train_y, **fit_params)
  clf.fit(train_x,
  train_y,
  early_stopping_rounds=100,
  eval_metric= 'auc', 
            eval_set = [(validation_x,validation_y)],
            eval_names = ['valid'],
            verbose= 100
  )
         
  return clf
