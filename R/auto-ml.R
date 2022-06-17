#' Tools for working with h2o auto_ml results
#' @param object A `model_fit` or fitted `workflow` object.
#' @param n The number of individual models to extract from `auto_ml` results,
#'  ranked descendingly by performance. Default to all.
#' @param id Model id.
#' @return A [tibble::tibble()] of model's cross validation performances.
#' @rdname autml-tools
#' @export
rank_automl <- function(object, ...) {
  UseMethod("rank_automl")
}

#' @export
rank_automl.default <- function(object, ...) {
  msg <- paste0(
    "The first argument to [rank_automl()] should be either ",
    "a fitted `auto_ml()` model or workflow."
  )
  rlang::abort(msg)
}

#' @export
rank_automl.workflow <- function(object, n = NULL) {
  fit <- object$fit$fit
  if (!("_H2OAutoML" %in% class(fit))) {
    msg <- paste0(
      "The first argument to [rank_automl()] should be ",
      "fitted by `auto_ml()`."
    )
    rlang::abort(msg)
  }

  rank_automl.model_fit(fit, n = n)
}

#' @export
rank_automl.model_fit <- function(object, n = NULL) {
  if (!("_H2OAutoML" %in% class(object))) {
    msg <- paste0(
      "The first argument to [rank_automl()] should be ",
      "fitted by `auto_ml()`."
    )
    rlang::abort(msg)
  }
  leaderboard <- object$fit@leaderboard
  n_models <- nrow(leaderboard)
  if (!is.null(n)) {
    if (n > n_models) {
      msg <- paste0(
        "`n` must be smaller or equal than the number of models (",
        glue::glue({n_models}),
        ")."
      )
      rlang::abort(msg)
    }
  } else {
    n <- n_models
  }

  idx <- seq_len(n)
  board <- tibble::as_tibble(leaderboard[idx, , drop = FALSE])
  model_ids <- board$model_id
  models <- purrr::map(model_ids, h2o::h2o.getModel)
  models_summary <- purrr::map_dfr(models, summarize_cv)

  models_summary %>%
    dplyr::group_by(.metric) %>%
    dplyr::mutate(rank = rank(mean, ties.method = "random")) %>%
    dplyr::arrange(rank)
}

summarize_cv <- function(x) {
  cv_summary <- x@model$cross_validation_metrics_summary
  df <- tibble::as_tibble(cv_summary) %>%
    dplyr::mutate(algorithm = x@algorithm,
                  model_id = x@model_id,
                  .metric = rownames(cv_summary),
                  .before = 1) %>%
    tidyr::pivot_longer(dplyr::starts_with("cv"),
                        names_to = "cv_id") %>%
    tidyr::nest(cv_details = c(cv_id, value))

  df
}
