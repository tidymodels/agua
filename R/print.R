#' Print wrappers for h2o models
#' @rdname h2o-print
#' @export
#' @keywords internal
print.h2o_fit <- function(x, ...) {
  msg <- paste0(
    "'x' is not a real parsnip `model_fit` object ",
    "and is only meant to be used for prediction with predict()."
  )
  rlang::warn(msg)

  NextMethod()
}

#' @rdname h2o-print
#' @export
#' @keywords internal
print.H2OAutoML_fit <- function(x, ...) {
  cat("parsnip model object\n\n")
  print_automl_fit(x$fit, rank = attributes(x)$automl_rank, ...)
}

#' @rdname h2o-print
#' @export
#' @keywords internal
print.H2OAutoML <- function(x, ...) {
  leaderboard <- x@leaderboard

  cat("H2O AutoML Summary:", nrow(leaderboard), "models\n")
  cat("==============\n")
  cat("Leader Algorithm:", x@leader@algorithm, "\n")
  cat("Leader ID:", x@leader@model_id, "\n\n")
  cat("Leaderboard Preview\n")
  print(head(leaderboard))
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
