#' A helper function for processing a grid for a specific model.
#'
#' This is an internal function that is used internal by the \pkg{tune}
#' package.
#'
#' @param static A list object produced by \pkg{tune}.
#' @param grid A tibble of candidate tuning parameters (if any)
#' @param resample_label A single row tibble of resampling `id` columns.
#' @return A list of tibbles containing the out-of-sample predictions for each
#' row of `grid`.
#' @keywords internal
#' @export
agua_train_predict <- function(static, grid, resample_label) {
  model_mode <-
    static$wflow |>
    hardhat::extract_spec_parsnip() |>
    purrr::pluck("mode")

  parallelism <- check_parallelism(static$control)

  # ----------------------------------------------------------------------------
  # Grid things

  orig_names <- names(grid)
  model_param_names_h2o <- extract_model_param_names_h2o(
    orig_names,
    static$wflow
  )
  parsnip_to_h2o <- orig_names
  names(parsnip_to_h2o) <- model_param_names_h2o
  h2o_to_parsnip <- model_param_names_h2o
  names(h2o_to_parsnip) <- orig_names

  grid_by_row <- vec_list_rowwise(grid)

  h2o_hyper_params <-
    grid |>
    dplyr::rename(!!!parsnip_to_h2o) |>
    as.list()

  # ----------------------------------------------------------------------------
  # Data things

  has_cal_data <- !is.null(static$data$cal)

  # extract outcome and predictor names (used by h2o.grid)
  predictor_names <- colnames(static$data$fit$data$predictors)

  # These data sets should be the results of any preprocessor used by the
  # workflow (e.g., a recipe).
  h2o_training_frame <- bind_frames(static, "fit") |> as_h2o("training_frame")
  h2o_pred_frame <- bind_frames(static, "pred") |> as_h2o("pred_frame")

  # For garbage collection later
  h2o_ids <- c(h2o_training_frame$id, h2o_pred_frame$id)

  if (has_cal_data) {
    h2o_cal_frame <- bind_frames(static, "cal") |> as_h2o("cal_frame")
    h2o_ids <- c(h2o_ids, h2o_cal_frame$id)
  }

  # remove objects from h2o server
  on.exit(h2o::h2o.rm(h2o_ids))

  # ----------------------------------------------------------------------------
  # Run grid/resampling

  h2o_algo <- extract_h2o_algorithm(static$wflow)

  if (length(h2o_hyper_params) > 1) {
    h2o_search_criteria <- list(strategy = "Sequential")
  } else {
    h2o_search_criteria <- NULL
  }

  h2o_res <- h2o::h2o.grid(
    h2o_algo,
    x = predictor_names,
    y = static$y_name,
    training_frame = h2o_training_frame$data,
    hyper_params = h2o_hyper_params,
    parallelism = parallelism,
    search_criteria = h2o_search_criteria
  )

  # ----------------------------------------------------------------------------
  # Make predictions on the out-of-sample data and the calibration set (if any)

  h2o_model_ids <- as.character(h2o_res@model_ids)
  h2o_ids <- c(h2o_ids, h2o_model_ids)
  h2o_models <- purrr::map(h2o_model_ids, h2o_get_model)

  h2o_pred <- purrr::map(
    h2o_models,
    pull_h2o_predictions,
    val_frame = h2o_pred_frame$data,
    val_truth = static$data$pred$data[static$y_name],
    fold_id = resample_label,
    orig_rows = static$data$pred$ind,
    mode = model_mode
  ) |>
    purrr::map2(grid_by_row, ~ vctrs::vec_cbind(.y, .x))

  if (has_cal_data) {
    h2o_cal <- purrr::map(
      h2o_models,
      pull_h2o_predictions,
      val_frame = h2o_cal_frame$data,
      val_truth = static$data$cal$data[static$y_name],
      fold_id = resample_label,
      orig_rows = static$data$cal$ind,
      mode = model_mode
    ) |>
      purrr::map2(grid_by_row, ~ vctrs::vec_cbind(.y, .x))
  }

  list(pred = h2o_pred, cal = h2o_cal)
}

# ------------------------------------------------------------------------------

vec_list_rowwise <- function(x) {
  vctrs::vec_split(x, by = 1:nrow(x))$val
}

bind_frames <- function(x, slot = "fit") {
  dplyr::bind_cols(x$data[[slot]]$data$outcomes, x$data[[slot]]$data$predictors)
}

# ------------------------------------------------------------------------------

check_parallelism <- function(control) {
  backend_options <- control$backend_options
  if (is.null(backend_options)) {
    return(1L)
  }

  if (!inherits(backend_options, "agua_backend_options")) {
    cli::cli_abort(
      c(
        "{.arg backend_options} should be created by {.fn agua_backend_options}.",
        "i" = "For example: {.code control_grid(backend_options = agua_backend_options(parallelism = 5))}."
      )
    )
  }

  parallelism <- as.integer(backend_options$parallelism)
  if (is.na(parallelism)) {
    cli::cli_abort(
      "{.arg parallelism} should be an integer for the number of threads."
    )
  }

  parallelism
}

append_h2o_predictions <- function(collection, predictions, control) {
  if (!control$save_pred) {
    return(NULL)
  }
  if (inherits(predictions, "try-error")) {
    return(collection)
  }

  dplyr::bind_rows(collection, predictions)
}

pull_h2o_predictions <- function(
  h2o_model,
  val_frame,
  val_truth,
  fold_id,
  control,
  orig_rows,
  mode
) {
  outcome_name <- names(val_truth)
  h2o_preds <- h2o::h2o.predict(h2o_model, val_frame) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(predict = vctrs::vec_cast(predict, val_truth[[outcome_name]]))

  if (mode == "classification") {
    h2o_preds <- parsnip::format_classprobs(
      h2o_preds %>%
        dplyr::select(-predict)
    ) %>%
      dplyr::mutate(.row = orig_rows) %>%
      dplyr::bind_cols(
        parsnip::format_class(h2o_preds %>% purrr::pluck("predict"))
      )
  } else {
    h2o_preds <- parsnip::format_num(
      h2o_preds %>%
        purrr::pluck("predict")
    ) %>%
      dplyr::mutate(.row = orig_rows)
  }
  h2o_preds %>% dplyr::bind_cols(val_truth, fold_id)
}

#' Control model tuning via [h2o::h2o.grid()]
#' @inheritParams h2o::h2o.grid
#' @rdname h2o_tune
#' @export
agua_backend_options <- function(parallelism = 1) {
  rlang::check_installed("tune")
  tune::new_backend_options(
    parallelism = parallelism,
    class = "agua_backend_options"
  )
}
