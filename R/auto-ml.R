#' Tools for working with h2o auto_ml results
#'
#' @description
#' `rank_automl()` returns a tibble ranking cross validation performances
#' of different algorithms on a metric. `tidy()` returns the leaderboard
#' in tidy format.
#'
#' @details
#' The names of algorithms comes from h2o, see the details section in [h2o::h2o.automl()] for
#' more information.
#'
#' When `keep_model` is `TRUE`, `tidy()` adds a list column where each
#' component is a "fake" parsnip `model_fit` object constructed
#' from the h2o model. These objects are meant to be used for prediction only,
#' i.e., `predict(object, new_data = data)`, and should not be used as a
#' regular parsnip model.
#'
#' @param object A `model_fit` or fitted `workflow` object.
#' @param n The number of individual models to extract from `auto_ml` results,
#'  ranked descendingly by performance. Default to all.
#' @param ... Not used.
#' @return A [tibble::tibble()] cross validation performances.
#' @rdname automl-tools
#' @examples
#' if (h2o_running()) {
#'   mod <- auto_ml() %>%
#'     set_engine("h2o", max_runtime_secs = 10) %>%
#'     set_mode("regression") %>%
#'     fit(mpg ~ ., data = mtcars)
#'
#'   tidy(mod)
#'   rank_automl(mod)
#' }
#'
#' @export
rank_automl <- function(object, ...) {
  UseMethod("rank_automl")
}

#' @rdname automl-tools
#' @export
rank_automl.default <- function(object, ...) {
  msg <- paste0(
    "The first argument to [rank_automl()] should be either ",
    "a fitted `auto_ml()` model or workflow."
  )
  rlang::abort(msg)
}

#' @rdname automl-tools
#' @export
rank_automl.workflow <- function(object, n = NULL, ...) {
  object <- object$fit$fit$fit
  if (!("H2OAutoML" %in% class(object))) {
    msg <- paste0(
      "The first argument to [rank_automl()] should be ",
      "a fitted workflow object with `auto_ml()` specs."
    )
    rlang::abort(msg)
  }

  rank_automl.H2OAutoML(object, n = n, ...)
}

#' @rdname automl-tools
#' @export
rank_automl.model_fit <- function(object, n = NULL, ...) {
  if (!("H2OAutoML" %in% class(object$fit))) {
    msg <- paste0(
      "The first argument to [rank_automl()] should be ",
      "a `model_fit` object with `auto_ml()` specs."
    )
    rlang::abort(msg)
  }

  rank_automl.H2OAutoML(object$fit, n = n, ...)
}

#' @rdname automl-tools
#' @export
rank_automl.H2OAutoML <- function(object, n = NULL, ...) {
  leaderboard <- object@leaderboard
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
    dplyr::left_join(metric_info, by = ".metric") %>%
    dplyr::group_by(.metric) %>%
    dplyr::mutate(rank = rank(mean * direction, ties.method = "random")) %>%
    dplyr::select(-direction) %>%
    dplyr::ungroup()
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

metric_info <- tibble::tribble(
  ~ .metric, ~ direction,
  "mae", 1,
  "mean_residual_deviance", 1,
  "mse", 1,
  "residual_deviance", 1,
  "rmse", 1,
  "rmsle", 1,
  "null_deviance", 1,
  "r2", -1,
  "logloss", 1,
  "err", 1,
  "err_count", 1,
  "max_per_class_error", 1,
  "mean_per_class_error", 1,
  "recall", -1,
  "accuracy", -1,
  "auc", -1,
  "f0point5", -1,
  "f1", -1,
  "f2", -1,
  "lift_top_group", -1,
  "mcc", -1,
  "mean_per_class_accuracy", -1,
  "pr_auc", -1,
  "precision", -1,
  "specificity", -1
)

#' @rdname automl-tools
#' @param keep_model A logical value for whether individual models by
#'  `auto_ml()` should be retrieved from the server. Defaults to `TRUE`.
#'
#' @export
tidy._H2OAutoML <- function(object, keep_model = TRUE, ...) {
  leaderboard <- tibble::as_tibble(object$fit@leaderboard)
  res <- leaderboard %>%
    tidyr::pivot_longer(-c(model_id),
                        names_to = ".metric",
                        values_to = "mean")
  if (!keep_model) {
    return(res)
  }

  res %>%
    dplyr::mutate(.model = purrr::map(model_id, convert_model,
                                      spec = object$spec))
}

convert_model <- function(model_id, spec) {
  h2o_model <- h2o::h2o.getModel(model_id)
  res <- list(fit = h2o_model,
              spec = spec,
              elapsed = list(elapsed = NA_real_))
  class(res) <- c("automl_fit", paste0("_", class(h2o_model)[1]), "model_fit")
  res
}

#' @rdname automl-tools
#' @export
print.automl_fit <- function(object, ...) {
  msg <- paste0(
    "This is not a real parsnip `model_fit` object ",
    "and is only meant to be used for prediction with predict(). ",
    "Specifications are borrowed directly from the parent `auto_ml()` model."
  )
  rlang::warn(msg)

  NextMethod()
}

#' @rdname automl-tools
#' @param type A single character of choices of "rank" (plotting average ranking
#'  of algorithms within metrics) or "metric" (plotting average value of metrics).
#' @param metric A character vector or NULL for which metric to plot.
#'  By default, all metrics will be shown via facets.
#' @export
autoplot.H2OAutoML <- function(object, type = c("rank", "metric"), metric = NULL, ...) {
  type <- match.arg(type)
  results <- rank_automl(object) %>%
    dplyr::group_by(algorithm, .metric)

  if (!is.null(metric)) {
    results <- results %>% dplyr::filter(.metric %in% metric)
  }

  if (type == "rank") {
    df <- results %>% dplyr::summarise(value = mean(rank))
  }

  if (type == "metric") {
    df <- results %>% dplyr::summarise(value = mean(mean))
  }

  df %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(ggplot2::aes(value, algorithm)) +
    ggplot2::facet_wrap(~ .metric, scales = "free")
}
