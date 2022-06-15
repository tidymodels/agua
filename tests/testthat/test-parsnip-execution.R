test_that("random forest execution", {
  h2o_start()

  expect_h2o_fit(rand_forest(mtry = 2, trees = 5) %>%
               set_mode("regression"))

  expect_h2o_fit(rand_forest(mtry = 2, trees = 20) %>%
                   set_mode("classification"))
})

test_that("xgboost execution", {
  h2o_start()

  skip_if_not(h2o::h2o.xgboost.available())
  expect_h2o_fit(boost_tree(learn_rate = .1, trees = 5) %>%
                   set_mode("regression"))
  expect_h2o_fit(boost_tree(learn_rate = .1, trees = 5) %>%
                   set_mode("classification"))
})

test_that("linear regression execution", {
  h2o_start()

  expect_h2o_fit(linear_reg())
})

test_that("logistic regression execution", {
  h2o_start()

  expect_h2o_fit(logistic_reg())
})

test_that("poisson regression execution", {
  h2o_start()

  expect_h2o_fit(poisson_reg(engine = "h2o"),
             .data = as.data.frame(Titanic),
             .formula = Freq ~ .)
})

test_that("multinomial regression execution", {
  h2o_start()

  expect_h2o_fit(multinom_reg(),
               .data = iris,
               .formula = Species ~ .
  )
})

test_that("naive bayes execution", {
  h2o_start()

  expect_h2o_fit(naive_Bayes(engine = "h2o", Laplace = 1))
})

test_that("mlp execution", {
  h2o_start()
  expect_h2o_fit(mlp(hidden_units = 100) %>%
                   set_mode("regression"))
  expect_h2o_fit(mlp(hidden_units = 100) %>%
                   set_mode("classification"))
})

test_that("rule fit execution", {
  h2o_start()

  expect_h2o_fit(rule_fit(engine = "h2o", trees = 10, tree_depth = 3) %>%
                   set_mode("regression"))
  expect_h2o_fit(rule_fit(engine = "h2o", trees = 10, tree_depth = 3) %>%
                   set_mode("classification"))
})
