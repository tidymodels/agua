test_that("random forest execution", {
  skip_if(!interactive())
  h2o_start()

  expect_h2o_fit(rand_forest(mtry = 2, trees = 5) %>%
    set_mode("regression"))

  expect_h2o_fit(rand_forest(mtry = 2, trees = 20) %>%
    set_mode("classification"))
})


test_that("linear regression execution", {
  skip_if(!interactive())
  h2o_start()

  expect_h2o_fit(linear_reg(penalty = 0.1))
})

test_that("logistic regression execution", {
  skip_if(!interactive())
  h2o_start()

  expect_h2o_fit(logistic_reg(mixture = 1))
})

test_that("poisson regression execution", {
  skip_if(!interactive())
  h2o_start()

  expect_h2o_fit(poisson_reg(engine = "h2o"),
    data = as.data.frame(Titanic),
    formula = Freq ~ .
  )
})

test_that("multinomial regression execution", {
  skip_if(!interactive())
  h2o_start()

  expect_h2o_fit(multinom_reg(),
    data = iris,
    formula = Species ~ .
  )
})

test_that("naive bayes execution", {
  skip_if(!interactive())
  h2o_start()

  expect_h2o_fit(naive_Bayes(engine = "h2o", Laplace = 1))
})

test_that("mlp execution", {
  skip_if(!interactive())
  h2o_start()

  expect_h2o_fit(mlp(hidden_units = 100) %>%
    set_mode("regression"))
  expect_h2o_fit(mlp(hidden_units = 100) %>%
    set_mode("classification"))
})

test_that("rule fit execution", {
  skip_if(!interactive())
  h2o_start()

  expect_h2o_fit(rule_fit(engine = "h2o", trees = 10, tree_depth = 3) %>%
    set_mode("regression"))
  expect_h2o_fit(rule_fit(engine = "h2o", trees = 10, tree_depth = 3) %>%
    set_mode("classification"))
})

test_that("xgboost execution", {
  skip_if(!interactive())
  h2o_start()

  skip_if(!h2o_xgboost_available())
  expect_h2o_fit(boost_tree(learn_rate = .1, trees = 5) %>%
    set_mode("regression"))
  expect_h2o_fit(boost_tree(learn_rate = .1, trees = 5) %>%
    set_mode("classification"))
})

test_that("gbm execution", {
  skip_if(!interactive())
  h2o_start()

  expect_h2o_fit(boost_tree(learn_rate = .1, trees = 5) %>%
    set_mode("regression"), engine = "h2o_gbm")
  expect_h2o_fit(boost_tree(learn_rate = .1, trees = 5) %>%
    set_mode("classification"), engine = "h2o_gbm")
})

test_that("automl execution", {
  skip_if(!interactive())
  h2o_start()

  data(two_class_dat, package = "modeldata")
  set.seed(1)
  spec <- auto_ml() %>% set_engine("h2o",
    max_runtime_secs = 10
  )
  spec_reg <- spec %>% set_mode("regression")
  spec_cls <- spec %>% set_mode("classification")

  fit_reg <- spec_reg %>% fit(mpg ~ ., data = mtcars)
  fit_cls <- spec_cls %>% fit(Class ~ ., data = two_class_dat)
  pred_reg <- predict(fit_reg, head(mtcars))
  pred_cls <- predict(fit_cls, head(two_class_dat))

  expect_s3_class(fit_reg, "_H2OAutoML")
  expect_s3_class(fit_cls, "_H2OAutoML")
  expect_type(pred_reg[[1]], "double")
  expect_s3_class(pred_cls[[1]], "factor")
})

test_that("automl tools", {
  skip_if(!interactive())
  h2o_start()
  set.seed(1)

  spec <- auto_ml() %>%
    set_engine("h2o",
      max_runtime_secs = 10
    ) %>%
    set_mode("regression")
  mod <- spec %>% fit(mpg ~ ., data = mtcars)
  ranks <- rank_results(mod)
  mod_tidy <- tidy(mod, n = 10)
  leader <- extract_fit_parsnip(mod)

  expect_equal(nrow(mod_tidy), 10)
  expect_s3_class(ranks, "tbl_df")
  expect_s3_class(mod_tidy, "tbl_df")
  expect_warning(print(leader))
  expect_s3_class(leader, c("h2o_fit", "model_fit"))
})
