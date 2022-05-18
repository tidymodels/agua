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

test_that('random forest execution', {
  agua:::h2o_start()
  on.exit(try(h2o::h2o.shutdown(prompt = FALSE), silent = TRUE))
  skip_if(!h2o_running())

  set.seed(1)
  fit_reg <-
    rand_forest(mtry = 2, trees = 5) %>%
    set_engine("h2o", model_id = "fit_1") %>%
    set_mode("regression") %>%
    fit(mpg ~ ., data = mtcars)
  expect_equal(class(fit_reg), c("_H2ORegressionModel", "model_fit"))
  expect_snapshot(
    fit_reg
  )

  pred_reg <- predict(fit_reg, head(mtcars))
  expect_equal(
    pred_reg$.pred,
    c(19.0999998092651, 19.0999998092651, 22.4399993896484, 19.7699998855591,
      17.8650003433228, 19.1633333206177)
  )
})

