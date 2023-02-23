#' Prediction wrappers for h2o
#'
#' Prediction wrappers for fitted models with h2o engine that include data conversion,
#' h2o server cleanup, and so on.
#' @inheritParams parsnip::predict.model_fit
#' @inheritParams h2o::h2o.predict
#' @param id Model id in AutoML results.
#' @param ... Other options passed to [h2o::h2o.predict()]
#' @return For type != "raw", a prediction data frame with the same number of
#'   rows as `new_data`. For type == "raw", return the result of
#'   [h2o::h2o.predict()].
#' @export
#' @details
#' For AutoML, prediction is based on the best performing model.
#' @examplesIf agua:::should_run_examples()
#' if (h2o_running()) {
#'   spec <-
#'     rand_forest(mtry = 3, trees = 100) %>%
#'     set_engine("h2o") %>%
#'     set_mode("regression")
#'
#'   set.seed(1)
#'   mod <- fit(spec, mpg ~ ., data = mtcars)
#'   h2o_predict_regression(mod$fit, new_data = head(mtcars), type = "numeric")
#'
#'   # using parsnip
#'   predict(mod, new_data = head(mtcars))
#' }
h2o_predict <- function(object, new_data, ...) {
  new_data <- as_h2o(new_data)
  on.exit(h2o::h2o.rm(new_data$id))
  opts <- list(...)

  cl <- rlang::call2(
    "h2o.predict",
    .ns = "h2o",
    object = object,
    newdata = new_data$data,
    !!!opts
  )
  res <- h2o::h2o.no_progress(rlang::eval_tidy(cl))
  tibble::as_tibble(res)
}

#' @export
#' @rdname h2o_predict
h2o_predict_classification <- function(object, new_data, type = "class", ...) {
  res <- h2o_predict(object, new_data, ...)
  all_types <- c("class", "prob")

  msg <- glue::glue(
    paste0(
      "Prediction type `{type}` is not supported by the h2o engine. ",
      "Possible values are {toString(all_types)}."
    )
  )

  switch(type,
    "class" = res$predict,
    "prob" = res[, 2:ncol(res)],
    # TODO: type "raw", can h2o.predict return raw values?
    rlang::abort(msg)
  )
}

#' @export
#' @rdname h2o_predict
h2o_predict_regression <- function(object, new_data, type = "numeric", ...) {
  res <- h2o_predict(object, new_data, ...)
  all_types <- c("numeric", "raw")

  msg <- glue::glue(
    paste0(
      "Prediction type `{type}` is not supported by the h2o engine. ",
      "Possible values are {toString(all_types)}."
    )
  )

  switch(type,
    "numeric" = res$predict,
    "raw" = res,
    rlang::abort(msg)
  )
}

#' @export
#' @rdname h2o_predict
predict._H2OAutoML <- function(object, new_data, id = NULL, ...) {
  check_automl_fit(object)
  object <- extract_fit_parsnip(object, id)
  predict(object, new_data, ...)
}
