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

  # maybe use data slots instead of static so that they are processed

  parallelism <- check_parallelism(static$control)

  # ----------------------------------------------------------------------------
  # grid things

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
  # data things

  # extract outcome and predictor names (used by h2o.grid)
  predictor_names <- colnames(static$fit$data)
  predictor_names <- predictor_names[predictor_names != static$y_name]

  # These data sets should be the results of any preprocessor used by the
  # workflow (e.g., a recipe).
  h2o_training_frame <- as_h2o(static$fit$data, "training_frame")
  h2o_val_frame <- as_h2o(static$pred$data, "val_frame")

  # ----------------------------------------------------------------------------

  h2o_algo <- extract_h2o_algorithm(static$wflow)

  if (length(h2o_hyper_params) > 1) {
    h2o_search_criteria <- list(strategy = "Sequential")
  } else {
    h2o_search_criteria <-NULL
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

  # remove objects from h2o server
  on.exit(h2o::h2o.rm(c(
    h2o_model_ids,
    h2o_training_frame$id,
    h2o_val_frame$id
  )))

  # ----------------------------------------------------------------------------

  h2o_model_ids <- as.character(h2o_res@model_ids)
  h2o_models <- purrr::map(h2o_model_ids, h2o_get_model)

  val_truth <- val_info$data[static$y_name]
  h2o_predictions <- purrr::map(
    h2o_models,
    pull_h2o_predictions,
    val_frame = h2o_val_frame$data,
    val_truth = val_truth,
    fold_id = resample_label,
    orig_rows = static$pred$ind,
    mode = model_mode
  ) |>
    purrr::map2(grid_by_row, ~ vctrs::vec_cbind(.y, .x))

  h2o_predictions
}

# ------------------------------------------------------------------------------

vec_list_rowwise <- function(x) {
  vctrs::vec_split(x, by = 1:nrow(x))$val
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
    cli::cli_abort("{.arg parallelism} should be an integer for the number of threads.")
  }

  parallelism
}

#' Control model tuning via [h2o::h2o.grid()]
#' @inheritParams h2o::h2o.grid
#' @rdname h2o_tune
#' @export
agua_backend_options <- function(parallelism = 1) {
  tune::new_backend_options(parallelism = parallelism, class = "agua_backend_options")
}

