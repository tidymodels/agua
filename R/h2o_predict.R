#' Prediction wrappers for h2o
#'
#' Prediction wrappers for fitted models with h2o engine that include data conversion,
#' h2o server cleanup, and so on.
#' @inheritParams parsnip::predict.model_fit
#' @inheritParams h2o::h2o.predict
#' @param ... Other options passed to [h2o::h2o.predict()]
#' @return For type != "raw", a prediction data frame with the same number of
#'   rows as `new_data`. For type == "raw", return original [h2o::h2o.predict()]
#'   output
#' @export
#' @examples
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
#'   # using pasrsnip
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
  res <- h2o:::with_no_h2o_progress(rlang::eval_tidy(cl))
  tibble::as_tibble(res)
}

#' @export
#' @rdname h2o_predict
h2o_predict_classification <- function(object, new_data, type = "class", ...) {
  res <- h2o_predict(object, new_data, ...)
  switch(type,
    "class" = res$predict,
    "prob" = res[, 2:ncol(res)],
    # TODO: type "raw", can h2o.predict return raw values?
    rlang::abort(glue::glue("Prediction type `{type}` is not supported by
                            the h2o engine."))
  )
}

#' @export
#' @rdname h2o_predict
h2o_predict_regression <- function(object, new_data, type = "numeric", ...) {
  res <- h2o_predict(object, new_data, ...)
  switch(type,
    "numeric" = res$predict,
    "raw" = res,
    rlang::abort(glue::glue("Prediction type `{type}` is not supported by
                            the h2o engine."))
  )
}
