data(two_class_dat, package = "modeldata")

test_that("tune model only (with id)", {
  # skip("h2o server issues with testthat")

  agua:::h2o_start()
  on.exit(h2o::h2o.shutdown(prompt = FALSE))
  skip_if(!h2o_running())

  helper_objects <- helper_objects_agua()
  wflow <- workflows::workflow() %>%
    workflows::add_model(helper_objects$glm_spec_tune_label) %>%
    workflows::add_formula(Class ~ A + B)

  control <- tune::control_grid(save_pred = TRUE)
  res <- tune::tune_grid(wflow,
                         resamples = helper_objects$folds,
                         control = control,
                         grid = 5)
  expect_snapshot(res)
})

test_that("tune model only (without id)", {
  # skip("h2o server issues with testthat")

  agua:::h2o_start()
  on.exit(h2o::h2o.shutdown(prompt = FALSE))
  skip_if(!h2o_running())

  helper_objects <- helper_objects_agua()
  wflow <- workflows::workflow() %>%
    workflows::add_model(helper_objects$glm_spec_tune_no_label) %>%
    workflows::add_formula(Class ~ A + B)

  control <- tune::control_grid(save_pred = TRUE)
  res <- tune::tune_grid(wflow,
                         resamples = helper_objects$folds,
                         control = control,
                         grid = 5)
  expect_snapshot(res)
})

test_that("tune model only (with id and recipe)", {
  # skip("h2o server issues with testthat")

  agua:::h2o_start()
  on.exit(h2o::h2o.shutdown(prompt = FALSE))
  skip_if(!h2o_running())

  helper_objects <- helper_objects_agua()
  wflow <- workflows::workflow() %>%
    workflows::add_model(helper_objects$glm_spec_tune_no_label) %>%
    workflows::add_recipe(helper_objects$rec_no_tune)

  control <- tune::control_grid(save_pred = TRUE)
  res <- tune::tune_grid(wflow,
                         resamples = helper_objects$folds,
                         control = control,
                         grid = 5)
  expect_snapshot(res)
})



test_that("tune model and recipe", {
  # skip("h2o server issues with testthat")

  agua:::h2o_start()
  on.exit(h2o::h2o.shutdown(prompt = FALSE))
  skip_if(!h2o_running())

  helper_objects <- helper_objects_agua()
  wflow <- workflows::workflow() %>%
    workflows::add_model(helper_objects$glm_spec_tune_no_label) %>%
    workflows::add_recipe(helper_objects$rec_tune)
  param_grid <- expand.grid(
    penalty = 10^seq(-10, 1, length = 5),
    deg_free = c(3, 4, 5)
  )
  control <- tune::control_grid(save_pred = TRUE)
  res <- tune::tune_grid(wflow,
                         resamples = helper_objects$folds,
                         control = control,
                         grid = param_grid)
  expect_snapshot(res)
})



