#' Printing wrappers for h2o models
#' @rdname h2o-prints
#' @export
print.h2o_fit <- function(object, ...) {
  msg <- paste0(
    "This is not a real parsnip `model_fit` object ",
    "and is only meant to be used for prediction with predict(). ",
    "Specifications are borrowed directly from the parent `auto_ml()` model."
  )
  rlang::warn(msg)

  NextMethod()
}
