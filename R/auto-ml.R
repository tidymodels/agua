#' Tools for working with H2O AutoML results
#'
#' @description
#' `rank_results_automl()` ranks average cross validation performances of different
#' candidate models and algorithms on each metric.
#'
#' `tidy()` returns a tibble with average performance for each candidate model.
#' When `keep_model` is `TRUE`, `tidy()` adds a list column where each
#' component is a "fake" parsnip `model_fit` object constructed
#' from the h2o model. These objects are meant to be used for prediction only,
#' i.e., `predict(object, new_data = data)`, and should not be used as a
#' regular parsnip model.
#'
#' `member_weights()` computes variable importance for all stacked ensemble
#' models, i.e., the relative importance of base models in the meta-learner.
#' This is typically the coefficient magnitude in the second-level GLM model.
#'
#' `extract_fit_parsnip()` is a s3 method to extract candidate model from
#' `auto_ml()` results. When `id` is null, it returns the leader model.
#'
#' @details
#' Algorithms in h2o's automatic machine learning include xgboost,
#' gradient boosting (`"GBM"`), random forest and variants (`"DRF"`, `"XRT"`),
#' generalized linear model (`"GLM"`), and neural network (`"deeplearning"`).
#' See the details section in [h2o::h2o.automl()] for more information.
#' @param object A fitted `auto_ml()` model.
#' @param n The number of models to extract from `auto_ml()` results,
#'  default to all.
#' @param summarize A logical value for should metrics be summarized over
#' resamples. Rankings are always based on average performance even if
#' `summarize` is `FALSE`.
#'
#' @param ... Not used.
#' @return A [tibble::tibble()].
#' @examples
#' if (h2o_running()) {
#'   mod <- auto_ml() %>%
#'     set_engine("h2o", max_runtime_secs = 10) %>%
#'     set_mode("regression") %>%
#'     fit(mpg ~ ., data = mtcars)
#'
#'   rank_results_automl(mod)
#'   tidy(mod)
#'   imp_stacking(mod)
#' }
#'
#' @export
#' @rdname automl-tools
rank_results_automl <- function(object, ...) {
  UseMethod("rank_results_automl")
}

#' @rdname automl-tools
#' @export
rank_results_automl.default <- function(object, ...) {
  msg <- paste0(
    "The first argument should be a fitted `auto_ml()` model."
  )
  rlang::abort(msg)
}

#' @rdname automl-tools
#' @export
rank_results_automl.model_fit <- function(object, n = NULL, id = NULL, ...) {
  check_automl_fit(object)
  rank_results_automl.H2OAutoML(object$fit, n = n, id = id, ...)
}


#' @rdname automl-tools
#' @export
rank_results_automl.H2OAutoML <- function(object,
                                          n = NULL,
                                          id = NULL,
                                          summarize = TRUE,
                                          ...) {
  leaderboard <- get_leaderboard(object, n, id)
  id <- leaderboard$model_id
  models <- purrr::map(id, get_model)
  cv_metrics <- purrr::map_dfr(models, get_cv_metrics, summarize = summarize)

  if (summarize) {
    res <- cv_metrics %>%
      dplyr::left_join(metric_info, by = ".metric") %>%
      dplyr::group_by(.metric) %>%
      dplyr::mutate(rank = rank(mean * direction, ties.method = "random"))

  } else {
    res_rank <- cv_metrics %>%
      dplyr::group_by(.metric, id) %>%
      dplyr::summarize(mean = mean(value)) %>%
      dplyr::left_join(metric_info, by = c(".metric")) %>%
      dplyr::mutate(rank = rank(mean * direction, ties.method = "random")) %>%
      dplyr::select(-mean)

    res <- cv_metrics %>% dplyr::left_join(res_rank, by = c("id", ".metric"))
  }

  res %>%
    dplyr::select(-direction) %>%
    dplyr::ungroup()
}


get_cv_metrics <- function(x, summarize) {
  cv_summary <- x@model$cross_validation_metrics_summary
  cv_summary[["sd"]] <- NULL
  cv_summary[["mean"]] <- NULL
  res <- tibble::as_tibble(cv_summary) %>%
    dplyr::mutate(
      id = x@model_id,
      algorithm = x@algorithm,
      .metric = rownames(cv_summary),
      .before = 1
    ) %>%
    tidyr::pivot_longer(dplyr::starts_with("cv"),
      names_to = "cv_id",
      values_to = "value"
    )

  if (summarize) {
    res <- res %>%
      dplyr::group_by(id, algorithm, .metric) %>%
      dplyr::summarize(
        std_err = sd(value, na.rm = TRUE) / sqrt(dplyr::n()),
        mean = mean(value, na.rm = TRUE),
        .groups = "drop"
      )
  }

  res
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
#' @param id A character vector of model ids to retrieve.
#' @export
tidy._H2OAutoML <- function(object,
                            n = NULL,
                            id = NULL,
                            keep_model = TRUE,
                            ...) {
  leaderboard <- get_leaderboard(object, n, id)
  leaderboard <- leaderboard %>%
    tidyr::pivot_longer(-c(model_id),
      names_to = ".metric",
      values_to = "mean"
    ) %>%
    dplyr::rename(id = model_id) %>%
    tidyr::nest(.metric = c(.metric, mean)) %>%
    dplyr::ungroup()

  if (!keep_model) {
    return(leaderboard)
  }

  leaderboard %>%
    dplyr::mutate(.model = purrr::map(
      id,
      ~ extract_fit_parsnip(object, .x),
    )) %>%
    dplyr::mutate(
      algorithm = purrr::map_chr(.model, ~ .x$fit@algorithm),
      .after = 1
    )
}

#' @rdname automl-tools
#' @export
get_leaderboard <- function(object, n = NULL, id = NULL) {
  if (inherits(object, "_H2OAutoML")) {
    object <- object$fit
  }
  leaderboard <- tibble::as_tibble(object@leaderboard)
  if (!is.null(id) && is.character(id)) {
    n <- NULL
    leaderboard <- leaderboard %>% dplyr::filter(model_id %in% id)
  }
  if (!is.null(n)) {
    n <- check_leaderboard_n(leaderboard, n)
    leaderboard <- leaderboard[seq_len(n), ]
  }

  leaderboard
}

#' @rdname automl-tools
#' @export
member_weights <- function(object, ...) {
  check_automl_fit(object)
  leaderboard <- get_leaderboard(object)
  model_id <- leaderboard[grep("StackedEnsemble", leaderboard$model_id), ]$model_id
  ranks <- match(model_id, leaderboard$model_id)


  tibble::tibble(
    ensemble_id = model_id,
    rank = ranks,
    importance = purrr::map(ensemble_id, get_stacking_imp)
  )
}

get_stacking_imp <- function(id) {
  mod <- get_model(id)
  meta_learner <- get_model(mod@model$metalearner$name)
  res <- tibble::as_tibble(h2o::h2o.varimp(meta_learner))

  res %>%
    dplyr::rename(member = variable) %>%
    dplyr::mutate(algorithm = id_to_algorithm(member)) %>%
    tidyr::pivot_longer(-c(member, algorithm), names_to = "type", values_to = "value")
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
extract_fit_parsnip._H2OAutoML <- function(object, id = NULL, ...) {
  if (is.null(id)) {
    id <- object$fit@leader@model_id
  }
  mod <- get_model(id)
  leaderboard <- get_leaderboard(object)
  automl_rank <- match(id, leaderboard$model_id)
  mod <- convert_h2o_parsnip(mod, object$spec, object$lvl, extra_class = NULL)
  class(mod) <- c("h2o_fit", "H2OAutoML_fit", class(mod))
  attr(mod, "automl_rank") <- automl_rank
  mod
}
