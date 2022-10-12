test_that("case weights works", {
  skip_if(!interactive())
  skip_if_not_installed("modeldata")

  h2o_start()
  data("two_class_dat", package = "modeldata")

  wts <- runif(nrow(two_class_dat))
  wts <- ifelse(wts < 1 / 5, 0, 1)
  two_class_subset <- two_class_dat[wts != 0, ]
  wts <- importance_weights(wts)


  spec <- rand_forest(trees = 10) %>%
    set_engine("h2o") %>%
    set_mode("classification")

  set.seed(1)
  fit_res <- spec %>%
    fit(Class ~ ., data = two_class_dat, case_weights = wts)
  wf_res <- workflows::workflow() %>%
    workflows::add_model(spec) %>%
    workflows::add_formula(Class ~ .) %>%
    workflows::add_case_weights(wts) %>%
    fit(data = two_class_dat %>% dplyr::mutate(wts = wts))


  expect_equal(
    fit_res$fit@parameters$weights_column$column_name,
    ".weights"
  )
  expect_equal(
    wf_res$fit$fit$fit@parameters$weights_column$column_name,
    ".weights"
  )
})
