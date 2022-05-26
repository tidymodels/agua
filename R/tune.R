tune_grid_loop_iter_h2o <- function(split,
                                    grid_info,
                                    workflow,
                                    metrics,
                                    control,
                                    seed) {
  h2o::h2o.no_progress()
  on.exit(h2o::h2o.show_progress())

  tune::load_pkgs(workflow)
  tune:::load_namespace(control$pkgs)

  training_frame <- rsample::analysis(split)
  val_frame <- rsample::assessment(split)
  mode <- hardhat::extract_spec_parsnip(wf)$mode
  workflow_original <- wf

  fold_id <- labels(split)
  pset <- hardhat::extract_parameter_set_dials(workflow_original)
  param_names <- dplyr::pull(pset, "id")
  model_params <- dplyr::filter(pset, source == "model_spec")
  preprocessor_params <- dplyr::filter(pset, source == "recipe")
  model_param_names <- dplyr::pull(model_params, "id")
  preprocessor_param_names <- dplyr::pull(preprocessor_params, "id")

  out_metrics <- NULL
  out_extracts <- NULL
  out_predictions <- NULL
  out_all_outcome_names <- list()
  out_notes <-
    tibble::tibble(location = character(0), type = character(0), note = character(0))
  event_level <- control$event_level
  orig_rows <- as.integer(split, data = "assessment")

  cols <- rlang::expr(
    c(
      .iter_model,
      .iter_config,
      .msg_model,
      dplyr::all_of(model_param_names)
    )
  )
  grid_info <- tidyr::nest(grid_info, data = !!cols)


  iter_preprocessors <- grid_info[[".iter_preprocessor"]]

  # preprocessor loop
  for (iter_preprocessor in iter_preprocessors) {
    workflow <- workflow_original
    iter_grid_info <- dplyr::filter(
      .data = grid_info,
      .iter_preprocessor == iter_preprocessor
    )

    iter_grid_preprocessor <- dplyr::select(
      .data = iter_grid_info,
      dplyr::all_of(preprocessor_param_names)
    )


    iter_msg_preprocessor <- iter_grid_info[[".msg_preprocessor"]]

    # finalize and extract preprocessor
    workflow <- tune:::finalize_workflow_preprocessor(
      workflow = workflow,
      grid_preprocessor = iter_grid_preprocessor
    )
    workflow <- tune:::catch_and_log(
      .expr = workflows::.fit_pre(workflow, training_frame),
      control,
      split,
      iter_msg_preprocessor,
      notes = out_notes
    )

    preprocessor <- hardhat::extract_recipe(workflow)

    # prep training and validation data
    training_frame_processed <- recipes::bake(preprocessor, new_data = training_frame)
    val_frame_processed <- recipes::bake(preprocessor, new_data = val_frame)

    iter_grid_info_models <- iter_grid_info[["data"]][[1L]] %>%
      tidyr::unnest(.iter_config) %>%
      dplyr::select(dplyr::all_of(model_param_names), .iter_config)

    outcome_name <- tune:::outcome_names(workflow)
    out_all_outcome_names <- c(
      out_all_outcome_names,
      outcome_name
    )

    iter_grid <- dplyr::bind_cols(
      iter_grid_preprocessor,
      iter_grid_info_models
    )

    # extract outcome and predictor names (used by h2o.grid)
    outcome <- preprocessor$term_info %>%
      dplyr::filter(role == "outcome") %>%
      dplyr::pull("variable")

    predictors <- preprocessor$term_info %>%
      dplyr::filter(role == "predictor") %>%
      dplyr::pull("variable")

    # extract hyper params into list
    h2o_hyper_params <- purrr::map(
      model_param_names,
      ~ dplyr::pull(iter_grid_info_models, .)
      %>% unique()
    ) %>%
      purrr::set_names(model_param_names)

    h2o_training_frame <- as_h2o(training_frame_processed, "training_frame")
    h2o_val_frame <- as_h2o(val_frame_processed, "val_frame")

    h2o_algo <- extract_h2o_algorithm(workflow)
    h2o_res <- h2o::h2o.grid(
      h2o_algo,
      x = predictors,
      y = outcome,
      training_frame = h2o_training_frame$data,
      hyper_params = h2o_hyper_params
    )

    h2o_model_ids <- as.character(h2o_res@model_ids)
    h2o_models <- purrr::map(h2o_model_ids, h2o.getModel)
    # remove objects from h2o server
    on.exit(h2o::h2o.rm(c(
      h2o_model_ids,
      h2o_training_frame$id,
      h2o_val_frame$id
    )))

    val_truth <- val_frame_processed[outcome_name]
    h2o_predictions <- purrr::map(
      h2o_models,
      pull_h2o_predictions,
      val_frame = h2o_val_frame$data,
      val_truth = val_truth,
      fold_id = fold_id,
      orig_rows = orig_rows,
      mode = mode
    ) %>%
      purrr::imap(~ bind_prediction_iter_grid(
        predictions = .x,
        iter_grid = iter_grid[.y, ],
        param_names = param_names
      ))
    iter_predictions <- dplyr::bind_rows(!!!h2o_predictions)

    out_predictions <- append_h2o_predictions(
      out_predictions,
      predictions = iter_predictions,
      control = control
    )

    # yardstick metrics
    h2o_metrics <- purrr::map(
      h2o_predictions,
      pull_h2o_metrics,
      metrics = metrics,
      fold_id = fold_id,
      param_names = param_names,
      outcome_name = outcome_name,
      event_level = event_level
    )
    iter_metrics <- dplyr::bind_rows(!!!h2o_metrics)
    out_metrics <- dplyr::bind_rows(out_metrics, iter_metrics)

    iter_extracts <- iter_grid_info %>%
      tidyr::unnest(cols = data) %>%
      dplyr::select(
        dplyr::all_of(preprocessor_param_names),
        dplyr::all_of(model_param_names),
        .iter_config
      ) %>%
      tidyr::unnest(.iter_config) %>%
      bind_cols(fold_id)
    out_extracts <- dplyr::bind_rows(out_extracts, iter_extracts)
  }

  list(
    .extracts = out_extracts,
    .metrics = out_metrics,
    .predictions = out_predictions,
    .all_outcome_names = out_all_outcome_names,
    .notes = out_notes
  )
}

bind_prediction_iter_grid <- function(predictions, iter_grid, param_names) {
  predictions %>%
    dplyr::bind_cols(iter_grid) %>%
    # relocate and rename to be consistent with tune_grid output
    dplyr::relocate(dplyr::all_of(param_names), .after = .row) %>%
    dplyr::rename(.config = .iter_config)
}

pull_h2o_predictions <- function(h2o_model,
                                 val_frame,
                                 val_truth,
                                 fold_id,
                                 control,
                                 orig_rows,
                                 mode) {
  h2o_preds <- h2o::h2o.predict(h2o_model, val_frame) %>%
    tibble::as_tibble()

  if (mode == "classification") {
    h2o_preds <- parsnip:::format_classprobs(h2o_preds %>%
      dplyr::select(-predict)) %>%
      dplyr::mutate(.row = orig_rows) %>%
      dplyr::bind_cols(
        parsnip:::format_class(h2o_preds %>% purrr::pluck("predict"))
      )
  } else {
    h2o_preds <- parsnip:::format_num(h2o_preds %>%
      purrr::pluck("predict")) %>%
      dplyr::mutate(.row = orig_rows)
  }
  h2o_preds %>% dplyr::bind_cols(val_truth, fold_id)
}


append_h2o_predictions <- function(collection,
                                   predictions,
                                   control) {
  if (!control$save_pred) {
    return(NULL)
  }
  if (inherits(predictions, "try-error")) {
    return(collection)
  }

  dplyr::bind_rows(collection, predictions)
}



pull_h2o_metrics <- function(predictions,
                             metrics,
                             fold_id,
                             param_names,
                             outcome_name,
                             event_level) {
  metrics <- tune:::estimate_metrics(
    predictions,
    metrics,
    param_names,
    outcome_name,
    event_level
  )
  metrics %>% bind_cols(fold_id)
}

