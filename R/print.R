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

  title <- paste("H2O AutoML Summary:", nrow(leaderboard), "models")
  cat(cli::rule(center = title, line = 2), "\n")
  cat(cli::cli_text(
    paste("{.strong Leader Algorithm}:", x@leader@algorithm)
  ), "\n")
  cat(cli::cli_text(
    paste("{.strong Leader ID}:", x@leader@model_id)
  ), "\n")

  cat(cli::rule(center = "Leaderboard", line = 2), "\n")
  print(head(leaderboard))
}

print_automl_fit <- function(object, rank = NULL, ...) {
  title <- "H2O AutoML Candidate Model"
  cat(cli::rule(center = title, line = 2), "\n")

  cat(cli::cli_text(
    paste("{.strong Algorithm}:", object@algorithm)
  ), "\n")
  cat(cli::cli_text(
    paste("{.strong Ranking}:", rank)
  ), "\n")
  cat(cli::cli_text(
    paste("{.strong ID}:", object@model_id)
  ), "\n")

  cat(cli::rule(center = "Details", line = 2), "\n")

  model_summary <- object@model$model_summary
  cv_summary <- object@model$cross_validation_metrics
  print(model_summary)
  cat("\n")
  print(cv_summary)
}
