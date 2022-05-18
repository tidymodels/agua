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

