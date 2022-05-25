library(testthat)

test_that('random forest specs', {
  expect_snapshot(
    rand_forest(mtry = 2, trees = 1000) %>%
      set_engine("h2o") %>%
      set_mode("regression") %>%
      translate()
  )

  expect_snapshot(
    rand_forest(mtry = 2, trees = 1000) %>%
      set_engine("h2o", sample_rate = 1/3, distribution = "quantile") %>%
      set_mode("regression") %>%
      translate()
  )
})

test_that('xgboost specs', {
  expect_snapshot(
    boost_tree(learn_rate = .1, trees = 1000) %>%
      set_engine("h2o") %>%
      set_mode("regression") %>%
      translate()
  )

  expect_snapshot(
    boost_tree(learn_rate = .1, trees = 1000) %>%
      set_engine("h2o", gamma = 1/3) %>%
      set_mode("regression") %>%
      translate()
  )
})
