test_that('parsnip model execution', {
  skip("h2o server issues with testthat")

  agua:::h2o_start()
  on.exit(try(h2o::h2o.shutdown(prompt = FALSE), silent = TRUE))
  skip_if(!h2o_running())

  data("two_class_dat", package = "modeldata")

  expect_reg <- function(spec, expected_pred, ...) {
    set.seed(1)
    fit_reg <- spec %>%
      set_engine("h2o", ...) %>%
      set_mode("regression") %>%
      fit(mpg ~ ., data = mtcars)
    pred_reg <- predict(fit_reg, head(mtcars))

    eval(bquote(expect_snapshot(fit_reg)))
    eval(bquote(expect_equal(class(fit_reg), c("_H2ORegressionModel", "model_fit"))))
    eval(bquote(expect_equal(pred_reg$.pred, expected_pred)))
  }

  expect_class <- function(spec, expected_pred, ...) {
    set.seed(1)
    fit_class <- spec %>%
      set_engine("h2o", ...) %>%
      set_mode("classification") %>%
      fit(Class ~ ., data = two_class_dat)
    pred_class <- predict(fit_class, head(two_class_dat), type = "prob")

    eval(bquote(expect_snapshot(fit_class)))
    eval(bquote(expect_equal(class(fit_class), c("_H2OBinomialModel", "model_fit"))))
    eval(bquote(expect_equal(pred_class$.pred_Class2, expected_pred)))
  }

  expect_multi_class <- function(spec, expected_pred1, expected_pred2, ...) {
    set.seed(1)
    fit_class <- spec %>%
      set_engine("h2o", ...) %>%
      set_mode("classification") %>%
      fit(Species ~ ., data = iris)
    pred_class <- predict(fit_class, head(iris), type = "prob")

    eval(bquote(expect_snapshot(fit_class)))
    eval(bquote(expect_equal(class(fit_class), c("_H2OMultinomialModel", "model_fit"))))
    eval(bquote(expect_equal(pred_class$.pred_setosa, expected_pred1)))
    eval(bquote(expect_equal(pred_class$.pred_versicolor, expected_pred2)))
  }

  # random forest regression
  expect_reg(rand_forest(mtry = 2, trees = 5),
             c(19.100000, 19.100000, 22.439999, 19.770000, 17.865000, 19.163333)
  )
  # random forest classification
  expect_class(rand_forest(mtry = 2, trees = 20),
             c(0.3, 0, 0.85, 0.8, 0.4, 1)
  )

    # boost tree regression
  if (h2o::h2o.xgboost.available()) {
    expect_reg(boost_tree(learn_rate = .1, trees = 5),
               c( 8.96138000488281, 8.96138000488281, 10.2200937271118,
                  8.28253364562988, 6.63353729248047, 8.28253364562988))
  }

  # boost tree classification
  if (h2o::h2o.xgboost.available()) {
    expect_class(boost_tree(learn_rate = .1, trees = 20),
                 c( 8.96138000488281, 8.96138000488281, 10.2200937271118,
                    8.28253364562988, 6.63353729248047, 8.28253364562988))
  }

  # linear regression
  expect_reg(linear_reg(),
             c(21.948257, 21.646053, 25.345465, 20.448832, 17.044916, 20.125847)
  )

  # logistic classification
  expect_class(logistic_reg(),
               c(0.486674849, 0.097683696, 0.355450942,
                 0.408943488, 0.567220135, 0.798129245)
  )

  # multinomial classification
  expect_multi_class(multinom_reg(),
                     c(0.99831868, 0.99091443, 0.99807912, 0.99691936, 0.99917055, 0.99824032),
                     c(0.0016813171, 0.0090855667, 0.0019208824, 0.0030806428, 0.0008294532, 0.0017596838))

  # naive bayes classification
  expect_class(naive_Bayes(engine = "h2o", Laplace = 1),
               c(0.47724811, 0.15598391, 0.24309786, 0.87980213, 0.82694825, 0.96210559)
  )


  # mlp regression
  # skip for seeding issue
  # expect_reg(mlp(hidden_units = 100),
  #            c(21.591594, 20.779010, 26.678451, 20.876708, 17.381320, 19.551599)
  # )

  # mlp classification
  # skip for seeding issue
  # expect_class(mlp(hidden_units = 100),
  #              c(0.556647684, 0.096701481, 0.371913185, 0.594010773, 0.709933951, 0.908649196)
  # )

  # rule fit regression
  expect_reg(rule_fit(engine = "h2o", trees = 100, tree_depth = 5),
               c(22.201423, 21.536628, 24.981005, 20.558594, 17.320593, 19.990024)
  )

  # rule fit classification
  expect_class(rule_fit(engine = "h2o", trees = 100, tree_depth = 5),
               c(0.56616535, 0.20167720, 0.32106085, 0.60254444, 0.60077550, 0.77305489)
  )

})
