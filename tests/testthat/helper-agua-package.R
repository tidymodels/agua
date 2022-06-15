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

expect_h2o_fit <- function(spec, ..., .data = NULL, .formula = NULL) {
  spec <- spec %>% set_engine("h2o", ...)
  if (spec$mode == "regression") {
    data <- if (is.null(.data)) mtcars else .data
    formula <- if (is.null(.formula)) (mpg ~ .) else .formula
    mod <- spec %>%
      fit(formula, data = data)
    preds <- predict(mod, head(data))
    eval(bquote(expect_s3_class(mod, "_H2ORegressionModel")))
    eval(bquote(expect_type(preds[[1]], "double")))
  }

  else if (spec$mode == "classification") {
    data <- if (is.null(.data)) two_class_dat else .data
    formula <- if (is.null(.formula)) (Class ~ .) else .formula
    mod <- spec %>%
      fit(formula, data = data)
    spec_class <- class(spec)[1]
    mod_class <- if (spec_class == "multinom_reg") "_H2OMultinomialModel" else "_H2OBinomialModel"
    preds <- predict(mod, head(data))
    eval(bquote(expect_s3_class(mod, mod_class)))
    eval(bquote(expect_s3_class(preds[[1]], "factor")))
  }
}
