# xgboost specs

    Code
      boost_tree(learn_rate = 0.1, trees = 1000) %>% set_engine("h2o") %>% set_mode(
        "regression") %>% translate()
    Output
      Boosted Tree Model Specification (regression)
      
      Main Arguments:
        trees = 1000
        learn_rate = 0.1
      
      Computational engine: h2o 
      
      Model fit template:
      agua::h2o_train_xgboost(x = missing_arg(), y = missing_arg(), 
          ntrees = 1000, learn_rate = 0.1)

---

    Code
      boost_tree(learn_rate = 0.1, trees = 1000) %>% set_engine("h2o", gamma = 1 / 3) %>%
        set_mode("regression") %>% translate()
    Output
      Boosted Tree Model Specification (regression)
      
      Main Arguments:
        trees = 1000
        learn_rate = 0.1
      
      Engine-Specific Arguments:
        gamma = 1/3
      
      Computational engine: h2o 
      
      Model fit template:
      agua::h2o_train_xgboost(x = missing_arg(), y = missing_arg(), 
          ntrees = 1000, learn_rate = 0.1, gamma = 1/3)

# xgboost execution

    Code
      fit_reg
    Output
      parsnip model object
      
      Model Details:
      ==============
      
      H2ORegressionModel: xgboost
      Model ID:  fit_1 
      Model Summary: 
        number_of_trees
      1               5
      
      
      H2ORegressionMetrics: xgboost
      ** Reported on training data. **
      
      MSE:  163.8029
      RMSE:  12.79855
      MAE:  12.00612
      RMSLE:  0.8332365
      Mean Residual Deviance :  163.8029
      
      
      
      

