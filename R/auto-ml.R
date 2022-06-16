#' Tools for working with h2o auto_ml results
#' @param object A `model_fit` or fitted `workflow` object.
#' @param n The number of individual models to extract from `auto_ml` results,
#'  ranked descendingly by performance.
#' @return A [tibble::tibble()] of model's cross validation performances
#' @rdname autml-tools
#' @export
h2o_rank <- function(object, ...) {
  UseMethod("h2o_rank")
}

#' @export
h2o_rank.default <- function(object, ...) {
  msg <- paste0(
    "The first argument to [h2o_rank()] should be either ",
    "a fitted model or workflow."
  )
  rlang::abort(msg)
}

#' @export
h2o_rank.workflow <- function(object, n = 10) {
  h2o_rank.model_fit(object$fit$fit, n = n)
}

#' @export
h2o_rank.model_fit <- function(object, n = 10) {
  idx <- seq_len(n)
  board <- (object$fit@leaderboard)[idx, , drop = FALSE]
  board <- tibble::as_tibble(board)
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
