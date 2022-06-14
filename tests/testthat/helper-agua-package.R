data(two_class_dat, package = "modeldata")

helper_objects_agua <- function() {
  set.seed(1)
  folds <- rsample::vfold_cv(two_class_dat, v = 5)

  rec_no_tune <-
    recipes::recipe(Class ~ A + B, data = two_class_dat) %>%
    recipes::step_normalize(recipes::all_predictors())

  rec_tune <-
    recipes::recipe(Class ~ A + B, data = two_class_dat) %>%
    recipes::step_normalize(recipes::all_predictors()) %>%
    recipes::step_ns(A, deg_free = tune())

  glm_spec_no_tune <- logistic_reg() %>%
    set_engine("h2o") %>%
    set_mode("classification")

  glm_spec_tune_label <- logistic_reg(penalty = tune("lambda")) %>%
    set_engine("h2o")

  glm_spec_tune_no_label <- logistic_reg(penalty = tune()) %>%
    set_engine("h2o")

  list(
    folds = folds,
    rec_tune = rec_tune,
    rec_no_tune = rec_no_tune,
    glm_spec_no_tune = glm_spec_no_tune,
    glm_spec_tune_label = glm_spec_tune_label,
    glm_spec_tune_no_label = glm_spec_tune_no_label
  )
}

build_model_info <- function(fit) {
  model <- fit$fit@model
  # metrics <- model$training_metrics@metrics
  summary <- as.data.frame(model$model_summary)
  if ("model_size_in_bytes" %in% names(summary)) {
    summary[["model_size_in_bytes"]] <- NULL
  }
  model_info <- list(
    summary = summary
    # metrics = metrics
  )

  model_info
}

expect_reg <- function(spec,
                       expected_pred,
                       formula = mpg ~ .,
                       data = mtcars,
                       ...) {
  set.seed(1)
  fit_reg <- spec %>%
    set_engine("h2o", ...) %>%
    set_mode("regression") %>%
    fit(formula, data = data)
  pred_reg <- predict(fit_reg, head(data))

  # model_info <- build_model_info(fit_reg)
  # eval(bquote(expect_snapshot_value(model_info, tolerance = 1e-5)))
  eval(bquote(expect_equal(class(fit_reg), c("_H2ORegressionModel", "model_fit"))))
  eval(bquote(expect_equal(pred_reg$.pred, expected_pred)))
}


expect_class <- function(spec,
                         expected_pred,
                         formula = Class ~ .,
                         data = two_class_dat,
                         ...) {
  set.seed(1)
  mod <- class(spec)[1]
  fit_class <- spec %>%
    set_engine("h2o", ...) %>%
    set_mode("classification") %>%
    fit(formula, data = data)
  pred_class <- predict(fit_class, head(data), type = "prob")

  # model_info <- build_model_info(fit_class)
  # eval(bquote(expect_snapshot_value(model_info, tolerance = 1e-5)))
  if (!identical(mod, "multinom_reg")) {
    eval(bquote(expect_equal(class(fit_class),
                             c("_H2OBinomialModel", "model_fit"))))
    eval(bquote(expect_equal(pred_class[[2]], expected_pred)))
  } else {
    eval(bquote(expect_equal(class(fit_class),
                             c("_H2OMultinomialModel", "model_fit"))))
    eval(bquote(expect_equal(pred_class[[1]], expected_pred[[1]])))
    eval(bquote(expect_equal(pred_class[[2]], expected_pred[[2]])))
  }
}

