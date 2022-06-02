library(testthat)

test_that("random forest specs", {
  expect_snapshot(
    rand_forest(mtry = 2, trees = 1000) %>%
      set_engine("h2o") %>%
      set_mode("regression") %>%
      translate()
  )

  expect_snapshot(
    rand_forest(mtry = 2, trees = 1000) %>%
      set_engine("h2o", sample_rate = 1 / 3, distribution = "quantile") %>%
      set_mode("regression") %>%
      translate()
  )
})

test_that("xgboost specs", {
  expect_snapshot(
    boost_tree(learn_rate = .1, trees = 1000) %>%
      set_engine("h2o") %>%
      set_mode("regression") %>%
      translate()
  )

  expect_snapshot(
    boost_tree(learn_rate = .1, trees = 1000) %>%
      set_engine("h2o", gamma = 1 / 3) %>%
      set_mode("regression") %>%
      translate()
  )
})

test_that("linear regression specs", {
  expect_snapshot(
    linear_reg(mixture = 0.5, penalty = 1e-2) %>%
      set_engine("h2o") %>%
      set_mode("regression") %>%
      translate()
  )

  expect_snapshot(
    linear_reg(mixture = 0.5, penalty = 1e-2) %>%
      set_engine("h2o", solver = "IRLSM") %>%
      set_mode("regression") %>%
      translate()
  )
})

test_that("logistic regression specs", {
  expect_snapshot(
    logistic_reg(mixture = 0.5, penalty = 1e-2) %>%
      set_engine("h2o") %>%
      set_mode("classification") %>%
      translate()
  )

  expect_snapshot(
    logistic_reg(mixture = 0.5, penalty = 1e-2) %>%
      set_engine("h2o", theta = 1e-5) %>%
      set_mode("classification") %>%
      translate()
  )
})

test_that("multinomial regression specs", {
  expect_snapshot(
    multinom_reg(mixture = 0.5, penalty = 1e-2) %>%
      set_engine("h2o") %>%
      set_mode("classification") %>%
      translate()
  )

  expect_snapshot(
    multinom_reg(mixture = 0.5, penalty = 1e-2) %>%
      set_engine("h2o", theta = 1e-5) %>%
      set_mode("classification") %>%
      translate()
  )
})

test_that("naive bayes specs", {
  expect_snapshot(
    naive_Bayes(engine = "h2o", Laplace = 1) %>%
      set_mode("classification") %>%
      translate()
  )

  expect_snapshot(
    naive_Bayes(engine = "h2o", Laplace = 1) %>%
      set_engine("h2o", min_sdev = 1e-10, min_prob = 1e-5) %>%
      set_mode("classification") %>%
      translate()
  )
})

test_that("mlp specs", {
  # TODO mlp activation
  expect_snapshot(
    mlp(hidden_units = 100, penalty = 0.5, activation = "Rectifier") %>%
      set_engine("h2o") %>%
      set_mode("regression") %>%
      translate()
  )

  expect_snapshot(
    mlp(hidden_units = 100, penalty = 0.5, activation = "Rectifier") %>%
      set_engine("h2o", standarize = FALSE) %>%
      set_mode("regression") %>%
      translate()
  )
})

test_that("mlp specs", {
  expect_snapshot(
    rule_fit(engine = "h2o", trees = 100, tree_depth = 5) %>%
      set_mode("regression") %>%
      translate()
  )

  expect_snapshot(
    rule_fit(engine = "h2o", trees = 100, tree_depth = 5) %>%
      set_engine("h2o", algorithm = "DRF") %>%
      set_mode("regression") %>%
      translate()
  )
})



