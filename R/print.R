#' Printing wrappers for h2o models
#' @rdname print-h2o
#' @export
#' @keywords internal
print.h2o_fit <- function(object, ...) {
  msg <- paste0(
    "This is not a real parsnip `model_fit` object ",
    "and is only meant to be used for prediction with predict()."
  )
  rlang::warn(msg)

  NextMethod()
}

#' @rdname print-h2o
#' @export
#' @keywords internal
print.H2OAutoML_fit <- function(object, ...) {
  cat("parsnip model object\n\n")
  print_automl_fit(object$fit, rank = attributes(object)$automl_rank, ...)

  invisible(object)
}

#' @rdname print-h2o
#' @export
#' @keywords internal
print.H2OAutoML <- function(object, ...) {
  cat("H2O AutoML Summary\n")
  cat("==============\n")
  cat("Leader Algorithm:", object@leader@algorithm, "\n")
  cat("Leader ID:", object@leader@model_id, "\n\n")

  leaderboard <- get_leaderboard(object, ...)
  cat("Leaderboard of", nrow(leaderboard), "models\n")
  print(leaderboard)
}

print_automl_fit <- function(object, rank, ...) {
  cat("H2O AutoML Candidate Model\n")
  cat("==============\n")
  cat("Model Algorithm:", object@algorithm, "\n")
  cat("Model ID:", object@model_id, "\n")
  cat("Model Ranking: ", rank, "\n\n")

  cat("Model Details", "\n")
  cat("==============\n\n")

  model_summary <- object@model$model_summary
  cv_summary <- object@model$cross_validation_metrics
  print(model_summary)
  cat("\n")
  print(cv_summary)
}
