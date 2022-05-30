helper_objects_agua <- function() {
  data(two_class_dat, package = "modeldata")
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
