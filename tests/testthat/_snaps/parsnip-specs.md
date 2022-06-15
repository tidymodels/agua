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
      agua::h2o_train_rf(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
          mtries = 2, ntrees = 1000)

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
      agua::h2o_train_rf(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
          mtries = 2, ntrees = 1000, sample_rate = 1/3, distribution = "quantile")

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
          weights = missing_arg(), ntrees = 1000, learn_rate = 0.1)

---

    Code
      boost_tree(learn_rate = 0.1, trees = 1000) %>% set_engine("h2o", gamma = 1 / 3,
      validation = 0.1) %>% set_mode("regression") %>% translate()
    Output
      Boosted Tree Model Specification (regression)
      
      Main Arguments:
        trees = 1000
        learn_rate = 0.1
      
      Engine-Specific Arguments:
        gamma = 1/3
        validation = 0.1
      
      Computational engine: h2o 
      
      Model fit template:
      agua::h2o_train_xgboost(x = missing_arg(), y = missing_arg(), 
          weights = missing_arg(), ntrees = 1000, learn_rate = 0.1, 
          gamma = 1/3, validation = 0.1)

# linear regression specs

    Code
      linear_reg(mixture = 0.5, penalty = 0.01) %>% set_engine("h2o") %>% set_mode(
        "regression") %>% translate()
    Output
      Linear Regression Model Specification (regression)
      
      Main Arguments:
        penalty = 0.01
        mixture = 0.5
      
      Computational engine: h2o 
      
      Model fit template:
      agua::h2o_train_glm(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
          lambda = 0.01, alpha = 0.5, family = "gaussian")

---

    Code
      linear_reg(mixture = 0.5, penalty = 0.01) %>% set_engine("h2o", solver = "IRLSM") %>%
        set_mode("regression") %>% translate()
    Output
      Linear Regression Model Specification (regression)
      
      Main Arguments:
        penalty = 0.01
        mixture = 0.5
      
      Engine-Specific Arguments:
        solver = IRLSM
      
      Computational engine: h2o 
      
      Model fit template:
      agua::h2o_train_glm(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
          lambda = 0.01, alpha = 0.5, solver = "IRLSM", family = "gaussian")

# logistic regression specs

    Code
      logistic_reg(mixture = 0.5, penalty = 0.01) %>% set_engine("h2o") %>% set_mode(
        "classification") %>% translate()
    Output
      Logistic Regression Model Specification (classification)
      
      Main Arguments:
        penalty = 0.01
        mixture = 0.5
      
      Computational engine: h2o 
      
      Model fit template:
      agua::h2o_train_glm(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
          lambda = 0.01, alpha = 0.5, family = "binomial")

---

    Code
      logistic_reg(mixture = 0.5, penalty = 0.01) %>% set_engine("h2o", theta = 1e-05) %>%
        set_mode("classification") %>% translate()
    Output
      Logistic Regression Model Specification (classification)
      
      Main Arguments:
        penalty = 0.01
        mixture = 0.5
      
      Engine-Specific Arguments:
        theta = 1e-05
      
      Computational engine: h2o 
      
      Model fit template:
      agua::h2o_train_glm(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
          lambda = 0.01, alpha = 0.5, theta = 1e-05, family = "binomial")

# poisson regression specs

    Code
      poisson_reg(engine = "h2o", mixture = 0.5, penalty = 0.01) %>% set_engine("h2o") %>%
        set_mode("regression") %>% translate()
    Output
      Poisson Regression Model Specification (regression)
      
      Main Arguments:
        penalty = 0.01
        mixture = 0.5
      
      Computational engine: h2o 
      
      Model fit template:
      agua::h2o_train_glm(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
          lambda = 0.01, alpha = 0.5, family = "poisson")

---

    Code
      poisson_reg(engine = "h2o", mixture = 0.5, penalty = 0.01) %>% set_engine("h2o",
        solver = "L_BFGS") %>% set_mode("regression") %>% translate()
    Output
      Poisson Regression Model Specification (regression)
      
      Main Arguments:
        penalty = 0.01
        mixture = 0.5
      
      Engine-Specific Arguments:
        solver = L_BFGS
      
      Computational engine: h2o 
      
      Model fit template:
      agua::h2o_train_glm(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
          lambda = 0.01, alpha = 0.5, solver = "L_BFGS", family = "poisson")

# multinomial regression specs

    Code
      multinom_reg(mixture = 0.5, penalty = 0.01) %>% set_engine("h2o") %>% set_mode(
        "classification") %>% translate()
    Output
      Multinomial Regression Model Specification (classification)
      
      Main Arguments:
        penalty = 0.01
        mixture = 0.5
      
      Computational engine: h2o 
      
      Model fit template:
      agua::h2o_train_glm(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
          lambda = 0.01, alpha = 0.5, family = "multinomial")

---

    Code
      multinom_reg(mixture = 0.5, penalty = 0.01) %>% set_engine("h2o", theta = 1e-05) %>%
        set_mode("classification") %>% translate()
    Output
      Multinomial Regression Model Specification (classification)
      
      Main Arguments:
        penalty = 0.01
        mixture = 0.5
      
      Engine-Specific Arguments:
        theta = 1e-05
      
      Computational engine: h2o 
      
      Model fit template:
      agua::h2o_train_glm(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
          lambda = 0.01, alpha = 0.5, theta = 1e-05, family = "multinomial")

# naive bayes specs

    Code
      naive_Bayes(engine = "h2o", Laplace = 1) %>% set_mode("classification") %>%
        translate()
    Output
      Naive Bayes Model Specification (classification)
      
      Main Arguments:
        Laplace = 1
      
      Computational engine: h2o 
      
      Model fit template:
      agua::h2o_train_nb(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
          laplace = 1)

---

    Code
      naive_Bayes(engine = "h2o", Laplace = 1) %>% set_engine("h2o", min_sdev = 1e-10,
        min_prob = 1e-05) %>% set_mode("classification") %>% translate()
    Output
      Naive Bayes Model Specification (classification)
      
      Main Arguments:
        Laplace = 1
      
      Engine-Specific Arguments:
        min_sdev = 1e-10
        min_prob = 1e-05
      
      Computational engine: h2o 
      
      Model fit template:
      agua::h2o_train_nb(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
          laplace = 1, min_sdev = 1e-10, min_prob = 1e-05)

# mlp specs

    Code
      mlp(hidden_units = 100, penalty = 0.5, activation = "relu") %>% set_engine(
        "h2o") %>% set_mode("regression") %>% translate()
    Output
      Single Layer Neural Network Model Specification (regression)
      
      Main Arguments:
        hidden_units = 100
        penalty = 0.5
        activation = relu
      
      Computational engine: h2o 
      
      Model fit template:
      agua::h2o_train_mlp(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
          hidden = 100, l2 = 0.5, activation = "relu")

---

    Code
      mlp(hidden_units = 100, penalty = 0.5, activation = "relu") %>% set_engine(
        "h2o", standarize = FALSE) %>% set_mode("regression") %>% translate()
    Output
      Single Layer Neural Network Model Specification (regression)
      
      Main Arguments:
        hidden_units = 100
        penalty = 0.5
        activation = relu
      
      Engine-Specific Arguments:
        standarize = FALSE
      
      Computational engine: h2o 
      
      Model fit template:
      agua::h2o_train_mlp(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
          hidden = 100, l2 = 0.5, activation = "relu", standarize = FALSE)

---

    Code
      rule_fit(engine = "h2o", trees = 100, tree_depth = 5) %>% set_mode("regression") %>%
        translate()
    Output
      RuleFit Model Specification (regression)
      
      Main Arguments:
        trees = 100
        tree_depth = 5
      
      Computational engine: h2o 
      
      Model fit template:
      agua::h2o_train_rule(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
          rule_generation_ntrees = 100, max_rule_length = 5)

---

    Code
      rule_fit(engine = "h2o", trees = 100, tree_depth = 5) %>% set_engine("h2o",
        algorithm = "DRF") %>% set_mode("regression") %>% translate()
    Output
      RuleFit Model Specification (regression)
      
      Main Arguments:
        trees = 100
        tree_depth = 5
      
      Engine-Specific Arguments:
        algorithm = DRF
      
      Computational engine: h2o 
      
      Model fit template:
      agua::h2o_train_rule(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
          rule_generation_ntrees = 100, max_rule_length = 5, algorithm = "DRF")

