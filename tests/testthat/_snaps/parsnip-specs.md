# random forest specs

    Code
      rand_forest(mtry = 2, trees = 1000) %>% set_engine("h2o") %>% set_mode(
        "regression") %>% translate()
    Output
      Random Forest Model Specification (regression)
      
      Main Arguments:
        mtry = 2
        trees = 1000
      
      Computational engine: h2o 
      
      Model fit template:
      agua::h2o_train_rf(x = missing_arg(), y = missing_arg(), mtries = 2, 
          ntrees = 1000)

---

    Code
      rand_forest(mtry = 2, trees = 1000) %>% set_engine("h2o", sample_rate = 1 / 3,
      distribution = "quantile") %>% set_mode("regression") %>% translate()
    Output
      Random Forest Model Specification (regression)
      
      Main Arguments:
        mtry = 2
        trees = 1000
      
      Engine-Specific Arguments:
        sample_rate = 1/3
        distribution = quantile
      
      Computational engine: h2o 
      
      Model fit template:
      agua::h2o_train_rf(x = missing_arg(), y = missing_arg(), mtries = 2, 
          ntrees = 1000, sample_rate = 1/3, distribution = "quantile")

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

