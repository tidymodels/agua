data(two_class_dat, package = "modeldata")

test_that("tune model only (with id)", {
  skip_if(!interactive())
  h2o_start()

  helper_objects <- helper_objects_tune()
  wflow <- workflows::workflow() %>%
    workflows::add_model(helper_objects$glm_spec_tune_label) %>%
    workflows::add_formula(Class ~ A + B)

  control <- tune::control_grid(save_pred = TRUE)
  res <- tune::tune_grid(wflow,
    resamples = helper_objects$folds,
    control = control,
    grid = 5
  )
  expect_snapshot(res)
})

test_that("tune model only (without id)", {
  skip_if(!interactive())
  h2o_start()

  helper_objects <- helper_objects_tune()
  wflow <- workflows::workflow() %>%
    workflows::add_model(helper_objects$glm_spec_tune_no_label) %>%
    workflows::add_formula(Class ~ A + B)

  control <- tune::control_grid(save_pred = TRUE)
  res <- tune::tune_grid(wflow,
    resamples = helper_objects$folds,
    control = control,
    grid = 5
  )
  expect_snapshot(res)
})

test_that("tune model only (with id and recipe)", {
  skip_if(!interactive())
  h2o_start()

  helper_objects <- helper_objects_tune()
  wflow <- workflows::workflow() %>%
    workflows::add_model(helper_objects$glm_spec_tune_no_label) %>%
    workflows::add_recipe(helper_objects$rec_no_tune)

  control <- tune::control_grid(save_pred = TRUE)
  res <- tune::tune_grid(wflow,
    resamples = helper_objects$folds,
    control = control,
    grid = 5
  )
  expect_snapshot(res)
})



test_that("tune model and recipe", {
  skip_if(!interactive())
  h2o_start()

  helper_objects <- helper_objects_tune()
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
    grid = param_grid
  )
  expect_snapshot(res)
})

test_that("tune with backend options parallelism", {
  skip_if(!interactive())
  h2o_start()

  helper_objects <- helper_objects_tune()
  wflow <- workflows::workflow() %>%
    workflows::add_model(helper_objects$glm_spec_tune_no_label) %>%
    workflows::add_recipe(helper_objects$rec_tune)
  param_grid <- expand.grid(
    penalty = 10^seq(-10, 1, length = 5),
    deg_free = c(3, 4, 5)
  )
  control <- tune::control_grid(
    save_pred = TRUE,
    backend_options = agua_backend_options(parallelism = 20)
  )
  res <- tune::tune_grid(
    wflow,
    resamples = helper_objects$folds,
    control = control,
    grid = param_grid
  )
  expect_snapshot(res)
})
