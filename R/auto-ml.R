#' Tools for working with h2o auto_ml results
#'
#' @description
#' `rank_automl()` ranks cross validation performances of different candidate models
#' and algorithms on each metric. `autoplot()` uses this ranking table to plot performances
#' via facets.
#'
#' `tidy()` returns a tibble with average performance for each candidate model. When
#' `keep_model` is `TRUE`, `tidy()` adds a list column where each
#' component is a "fake" parsnip `model_fit` object constructed
#' from the h2o model. These objects are meant to be used for prediction only,
#' i.e., `predict(object, new_data = data)`, and should not be used as a
#' regular parsnip model.
#'
#' `model_importance()` computes variable importance for each stacked ensemble,
#' i.e., the relative importance of base models in the meta-learner. This is
#' typically the coefficient magnitude in the second-level GLM model.
#'
#' `extract_automl_fit_parsnip()` builds a fake parsnip fit object given a model
#' id.
#'
#' @details
#' Typical algorithms in h2o's automatic machine learning includes xgboost,
#' gradient boosting (`"GBM"`), random forest and variants (`"DRF"`, `"XRT"`),
#' generalized linear model (`"GLM"`), and neural network (`"deeplearning"`).
#' See the details section in [h2o::h2o.automl()] for more information.
#' @param object A fitted `auto_ml()` model.
#' @param ... Not used.
#' @return A [tibble::tibble()].
#' @rdname automl-tools
#' @examples
#' if (h2o_running()) {
#'   mod <- auto_ml() %>%
#'     set_engine("h2o", max_runtime_secs = 10) %>%
#'     set_mode("regression") %>%
#'     fit(mpg ~ ., data = mtcars)
#'
#'   rank_automl(mod)
#'   tidy(mod)
#'   model_importance(mod)
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
    "The first argument should be a fitted `auto_ml()` model."
  )
  rlang::abort(msg)
}

#' @rdname automl-tools
#' @param n The number of models to extract from `auto_ml()` results,
#'  ranked descendingly by performance. Default to all.
#' @export
rank_automl.model_fit <- function(object, n = NULL, model_id = NULL, ...) {
  check_automl_fit(object)
  rank_automl.H2OAutoML(object$fit, n = n, model_id = model_id, ...)
}

#' @rdname automl-tools
#' @export
rank_automl.H2OAutoML <- function(object, n = NULL, model_id = NULL, ...) {
  leaderboard <- get_leaderboard(object, n, model_id)
  model_ids <- leaderboard$model_id
  models <- purrr::map(model_ids, get_model)
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
    dplyr::mutate(
      model_id = x@model_id,
      algorithm = x@algorithm,
      .metric = rownames(cv_summary),
      .before = 1
    ) %>%
    tidyr::pivot_longer(dplyr::starts_with("cv"),
      names_to = "cv_id"
    ) %>%
    tidyr::nest(cv_details = c(cv_id, value))

  df
}

metric_info <- tibble::tribble(
  ~.metric, ~direction,
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

check_automl_fit <- function(object) {
  if (!inherits(object, "_H2OAutoML")) {
    rlang::abort("The first argument should be a fitted `auto_ml()` model.")
  }
  invisible(object)
}

#' @rdname automl-tools
#' @param keep_model A logical value for if the actual model object
#'  should be retrieved from the server. Defaults to `TRUE`.
#' @param model_id A character vector of model ids to retrieve.
#' @export
tidy._H2OAutoML <- function(object,
                            n = NULL,
                            model_id = NULL,
                            keep_model = TRUE,
                            ...) {
  leaderboard <- get_leaderboard(object, n, model_id)
  leaderboard <- leaderboard %>%
    tidyr::pivot_longer(-c(model_id),
      names_to = ".metric",
      values_to = "mean"
    ) %>%
    dplyr::nest_by(model_id, .key = ".metric") %>%
    dplyr::ungroup()

  if (!keep_model) {
    return(leaderboard)
  }

  leaderboard %>%
    dplyr::mutate(.model = purrr::map(
      model_id,
      ~ extract_automl_fit_parsnip(object, .x),
    )) %>%
    dplyr::mutate(
      algorithm = purrr::map_chr(.model, ~ .x$fit@algorithm),
      .after = 1
    )
}

#' @rdname automl-tools
#' @param type A single character of choices of "rank" (plotting average ranking
#'  of algorithms within metrics) or "metric" (plotting average value of metrics).
#' @param metric A character vector or NULL for which metric to plot.
#'  By default, all metrics will be shown via facets.
#' @export
autoplot.H2OAutoML <- function(object,
                               type = c("rank", "metric"),
                               metric = NULL,
                               ...) {
  type <- match.arg(type)
  results <- rank_automl(object, ...)
  if (!is.null(metric)) {
    results <- results %>% dplyr::filter(.metric %in% metric)
  }
  results <- results %>% dplyr::group_by(.metric, algorithm)

  if (type == "rank") {
    results <- results %>% dplyr::summarise(value = mean(rank))
  } else if (type == "metric") {
    results <- results %>% dplyr::summarise(value = mean(mean))
  }

  p <- results %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(ggplot2::aes(value, algorithm))



  num_metrics <- length(unique(results$.metric))
  if (num_metrics > 1) {
    xlab <- if (type == "rank") "Ranking" else "Metric"
    results$.metric <- factor(results$.metric)
    p <- p +
      ggplot2::facet_wrap(~.metric, scales = "free_y", as.table = FALSE) +
      ggplot2::labs(x = xlab, y = "Algorithm")
  } else {
    metric_name <- results$.metric[[1]]
    xlab <- if (type == "rank") paste0("Ranking on ", metric_name) else metric_name
    p <- p + ggplot2::labs(x = xlab, y = "Algorithm")
  }

  p
}

get_leaderboard <- function(object, n = NULL, model_id = NULL) {
  if (inherits(object, "_H2OAutoML")) {
    object <- object$fit
  }
  leaderboard <- tibble::as_tibble(object@leaderboard)
  if (!is.null(model_id) && is.character(model_id)) {
    n <- NULL
    leaderboard <- leaderboard %>%
      dplyr::filter(.data[["model_id"]] %in% .env[["model_id"]])
  }
  if (!is.null(n)) {
    n <- check_leaderboard_n(leaderboard, n)
    leaderboard <- leaderboard[seq_len(n), ]
  }

  leaderboard
}

#' @export
#' @rdname automl-tools
model_importance <- function(object, ...) {
  check_automl_fit(object)
  leaderboard <- get_leaderboard(object)
  model_id <- leaderboard[grep("StackedEnsemble", leaderboard$model_id), ]$model_id

  tibble::tibble(
    stacked_model_id = model_id,
    importance = purrr::map(model_id, get_meta_learner_imp)
  )
}

get_meta_learner_imp <- function(model_id) {
  mod <- get_model(model_id)
  meta_learner <- get_model(mod@model$metalearner$name)
  tibble::as_tibble(h2o::h2o.varimp(meta_learner))
}

check_leaderboard_n <- function(leaderboard, n) {
  n_models <- nrow(leaderboard)
  if (!is.null(n) && n > n_models) {
    msg <- paste0(
      "`n` is larger than the number of models, ",
      "returning all."
    )
    rlang::warn(msg)
  }
  min(n, n_models)
}

#' @export
#' @rdname automl-tools
extract_automl_fit_parsnip <- function(object, model_id) {
  check_automl_fit(object)
  mod <- get_model(model_id)
  mod <- convert_h2o_parsnip(mod, object$spec, object$lvl)
  mod
}
