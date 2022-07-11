#' Tools for working with H2O AutoML results
#'
#' @description
#' `rank_results()` ranks average cross validation performances of
#' candidate models on each metric.
#'
#' `collect_metrics()` returns average statistics of performance metrics
#'  (summarized) per model, or raw value in each resample (unsummarized).
#'
#' `tidy()` returns a tibble with average performance for each model.
#'
#' `member_weights()` computes member importance for stacked ensemble
#' models, i.e., the relative importance of base models in the meta-learner.
#' This is typically the coefficient magnitude in the second-level GLM model.
#'
#' `extract_fit_engine()` extracts single candidate model from `auto_ml()`
#' results. When `id` is null, it returns the leader model.
#'
#' `refit()` re-fits an existing AutoML model to add more candidates. The model
#'  to be re-fitted needs to have engine argument `save_data = TRUE`, and
#' `keep_cross_validation_predictions = TRUE` if stacked ensembles is needed for
#' later models.
#'
#' @details
#' H2O associates with each model in AutoML an unique id. This can be used for
#' model extraction and prediction, i.e., `extract_fit_engine(x, id = id)`
#' returns the model and `predict(x, id = id)` will predict for that model.
#' `extract_fit_parsnip(x, id = id)` wraps the h2o model with parsnip
#  classes to enable predict and print methods, other usage of this "fake"
#' parsnip model object is discouraged.
#'
#' The `algorithm` column corresponds to the model family H2O use for a
#' particular model, including xgboost (`"XGBOOST"`),
#' gradient boosting (`"GBM"`), random forest and variants (`"DRF"`, `"XRT"`),
#' generalized linear model (`"GLM"`), and neural network (`"deeplearning"`).
#' See the details section in [h2o::h2o.automl()] for more information.
#'
#' @param object,x A fitted `auto_ml()` model or workflow.
#' @param n An integer for the number of top models to extract from AutoML
#'  results, default to all.
#' @param id A character vector of model ids to retrieve.
#' @param ... Not used.
#' @return A [tibble::tibble()].
#' @examples
#' if (h2o_running()) {
#'   auto_fit <- auto_ml() %>%
#'     set_engine("h2o", max_runtime_secs = 10) %>%
#'     set_mode("regression") %>%
#'     fit(mpg ~ ., data = mtcars)
#'
#'   rank_results(auto_fit, n = 5)
#'   collect_metrics(auto_fit, summarize = FALSE)
#'   tidy(auto_fit)
#'   member_weights(auto_fit)
#' }
#'
#' @export
#' @rdname automl-tools
rank_results.workflow <- function(x, ...) {
  rank_results(hardhat::extract_fit_parsnip(x), ...)
}


#' @rdname automl-tools
#' @export
rank_results._H2OAutoML <- function(x, ...) {
  rank_results(x$fit, ...)
}


#' @rdname automl-tools
#' @export
rank_results.H2OAutoML <- function(x,
                                   n = NULL,
                                   id = NULL,
                                   ...) {
  leaderboard <- get_leaderboard(x, n, id)
  models <- purrr::map(leaderboard$model_id, h2o_get_model)
  cv_metrics <- purrr::map_dfr(models, get_cv_metrics, summarize = TRUE)

  res <- cv_metrics %>%
    dplyr::left_join(metric_info, by = ".metric") %>%
    dplyr::group_by(.metric) %>%
    dplyr::mutate(rank = rank(mean * direction, ties.method = "random")) %>%
    dplyr::select(-direction) %>%
    dplyr::ungroup()

  res
}

get_cv_metrics <- function(x, summarize = TRUE) {
  cv_summary <- x@model$cross_validation_metrics_summary
  cv_summary[["sd"]] <- NULL
  cv_summary[["mean"]] <- NULL
  res <- tibble::as_tibble(cv_summary) %>%
    dplyr::mutate(
      id = x@model_id,
      algorithm = id_to_algorithm(id),
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

check_automl_fit <- function(x) {
  if (!inherits(x, "_H2OAutoML")) {
    msg <- paste0(
      "The first argument should be a fitted ",
      "`auto_ml()` model or workflow."
    )
    rlang::abort(msg)
  }
  invisible(x)
}

#' @rdname automl-tools
#' @export
collect_metrics.workflow <- function(x, ...) {
  collect_metrics(extract_fit_parsnip(x), ...)
}

#' @rdname automl-tools
#' @export
collect_metrics._H2OAutoML <- function(x, ...) {
  collect_metrics(x$fit, ...)
}

#' @param summarize A logical; should metrics be summarized over resamples
#'  (TRUE) or return the values for each individual resample.
#' @rdname automl-tools
#' @export
collect_metrics.H2OAutoML <- function(x,
                                      summarize = TRUE,
                                      n = NULL,
                                      id = NULL,
                                      ...) {
  leaderboard <- get_leaderboard(x, n = n, id = id)
  lvl <- leaderboard$model_id
  models <- purrr::map(leaderboard$model_id, h2o_get_model)
  cv_metrics <- purrr::map_dfr(models, get_cv_metrics, summarize = FALSE)

  if (summarize) {
    res <- cv_metrics %>%
      dplyr::mutate(id = factor(id, levels = lvl)) %>%
      dplyr::group_by(id, algorithm, .metric) %>%
      dplyr::summarize(
        mean = mean(value, na.rm = TRUE),
        std_err = sd(value) / sqrt(sum(!is.na(value))),
        n = sum(!is.na(value)),
        .groups = "drop"
      ) %>%
      dplyr::mutate(id = as.character(id))
  } else {
    res <- cv_metrics %>%
      dplyr::rename(.estimate = value)
  }

  res
}

#' @rdname automl-tools
#' @export
tidy.workflow <- function(x, ...) {
  tidy(extract_fit_parsnip(x), ...)
}

#' @rdname automl-tools
#' @param keep_model A logical value for if the actual model object
#'  should be retrieved from the server. Defaults to `TRUE`.
#' @export
tidy._H2OAutoML <- function(x,
                            n = NULL,
                            id = NULL,
                            keep_model = TRUE,
                            ...) {
  leaderboard <- get_leaderboard(x, n, id)
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
      ~ extract_fit_parsnip(x, .x),
    )) %>%
    dplyr::mutate(
      algorithm = purrr::map_chr(id, id_to_algorithm),
      .after = 1
    )
}

#' @rdname automl-tools
#' @export
get_leaderboard <- function(x, n = NULL, id = NULL) {
  if (inherits(x, "_H2OAutoML")) {
    x <- x$fit
  }
  leaderboard <- as.data.frame(x@leaderboard)
  if (!is.null(id) && is.character(id)) {
    n <- NULL
    leaderboard <- leaderboard %>% dplyr::filter(model_id %in% id)
  }
  if (!is.null(n)) {
    n <- check_leaderboard_n(leaderboard, n)
    leaderboard <- leaderboard[seq_len(n), ]
  }

  tibble::as_tibble(leaderboard)
}

#' @rdname automl-tools
#' @export
member_weights <- function(x, ...) {
  check_automl_fit(x)
  leaderboard <- get_leaderboard(x)
  model_id <- leaderboard[grep("StackedEnsemble", leaderboard$model_id), ]$model_id
  ranks <- match(model_id, leaderboard$model_id)

  tibble::tibble(
    ensemble_id = model_id,
    rank = ranks,
    importance = purrr::map(ensemble_id, get_stacking_imp)
  )
}

get_stacking_imp <- function(id) {
  mod <- h2o_get_model(id)
  meta_learner <- h2o_get_model(mod@model$metalearner$name)
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
extract_fit_parsnip._H2OAutoML <- function(x, id = NULL, ...) {
  leader <- try(slot(x$fit, "leader"), silent = TRUE)
  # for bundled objects, leaders are already extracted
  if (inherits(leader, "try-error")) {
    return(x$fit)
  }

  if (is.null(id)) {
    id <- x$fit@leader@model_id
  }
  mod <- h2o_get_model(id)
  leaderboard <- get_leaderboard(x)
  automl_rank <- match(id, leaderboard$model_id)
  mod <- convert_h2o_parsnip(mod, x$spec, x$lvl, extra_class = NULL)
  class(mod) <- c("h2o_fit", "H2OAutoML_fit", class(mod))
  attr(mod, "automl_rank") <- automl_rank
  mod
}

#' @export
#' @rdname automl-tools
extract_fit_engine._H2OAutoML <- function(x, id = NULL, ...) {
  leader <- try(slot(x$fit, "leader"), silent = TRUE)
  # for bundled objects, leaders are already extracted
  if (inherits(leader, "try-error")) {
    return(x$fit)
  }

  if (is.null(id)) {
    id <- x$fit@leader@model_id
  }
  mod <- h2o_get_model(id)
  mod
}

#' @export
#' @rdname automl-tools
refit.workflow <- function(object, ...) {
  refit(extract_fit_parsnip(object), ...)
}

#' @export
#' @param verbosity Verbosity of the backend messages printed during training;
#' Must be one of NULL (live log disabled), "debug", "info", "warn", "error".
#' Defaults to NULL.
#' @rdname automl-tools
refit._H2OAutoML <- function(object, verbosity = NULL, ...) {
  x <- object$fit
  params <- x@leader@allparameters
  project_name <- x@project_name
  training_frame <- h2o_get_frame(params$training_frame)
  if (is.null(training_frame)) {
    msg <- paste0(
      "The model needs to be trained with `save_data = TRUE` to ",
      "enable re-fitting, also set `keep_cross_validation_predictions = TRUE` ",
      "if you need stacked ensembles in later models."
    )
    rlang::abort(msg)
  }
  x_names <- params$x
  y <- params$y

  cl <- rlang::call2(
    "h2o.automl",
    .ns = "h2o",
    x = quote(x_names),
    y = y,
    training_frame = quote(training_frame),
    project_name = project_name,
    verbosity = verbosity,
    ...
  )
  res <- h2o:::with_no_h2o_progress(rlang::eval_tidy(cl))
  object$fit <- res
  object
}
