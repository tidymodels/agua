tune_grid_loop_iter_h2o <- function(split,
                                    grid_info,
                                    workflow,
                                    metrics,
                                    control,
                                    seed) {
  tune:::load_pkgs(workflow)
  tune:::load_namespace(control$pkgs)

  training_frame <- rsample::analysis(split)
  val_frame <- rsample::assessment(split)
  mode <- hardhat::extract_spec_parsnip(wf)$mode
  workflow_original <- wf

  pset <- hardhat::extract_parameter_set_dials(workflow_original)
  param_names <- dplyr::pull(pset, "id")
  model_params <- dplyr::filter(pset, source == "model_spec")
  preprocessor_params <- dplyr::filter(pset, source == "recipe")
  model_param_names <- dplyr::pull(model_params, "id")
  preprocessor_param_names <- dplyr::pull(preprocessor_params, "id")


  outcome_name <- tune:::outcome_names(workflow)
  event_level <- control$event_level
  orig_rows <- as.integer(split, data = "assessment")


  iter_preprocessors <- grid_info[[".iter_preprocessor"]]
  out <- vector("list", length(iter_preprocessors))
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

    val_truth <- val_frame_processed[outcome_name]
    h2o_preds <- purrr::map(h2o_models, pull_h2o_predictions,
                            val_frame = h2o_val_frame$data,
                            val_truth = val_truth,
                            orig_rows = orig_rows,
                            mode = mode) %>%
      purrr::imap(~ bind_prediction_iter_grid(prediction = .x,
                                              iter_grid = iter_grid[.y, ],
                                              param_names = param_names))
    # yardstick metrics
    h2o_metrics <- purrr::map(h2o_preds, pull_h2o_metrics,
                              metrics = metrics,
                              param_names = param_names,
                              outcome_name = outcome_name,
                              event_level = event_level)

    grid_out <- iter_grid_info %>%
      tidyr::unnest(cols = data) %>%
      dplyr::select(
        dplyr::all_of(preprocessor_param_names),
        dplyr::all_of(model_param_names),
        .iter_config
      ) %>%
      tidyr::unnest(.iter_config) %>%
      dplyr::mutate(.metrics = h2o_metrics)

    if (control$save_pred) {
      grid_out <- grid_out %>% dplyr::mutate(.predictions = h2o_preds)
    }

    out[[iter_preprocessor]] <- grid_out

    # remove objects from h2o server
    h2o::h2o.rm(c(h2o_model_ids, h2o_training_frame$id, h2o_val_frame$id))
  }

  out <- vctrs::vec_rbind(!!!out) %>%
    dplyr::bind_cols(labels(split))

  out
}

bind_prediction_iter_grid <- function(prediction, iter_grid, param_names) {
  prediction %>%
    dplyr::bind_cols(iter_grid) %>%
    # relocate and rename to be consistent with tune_grid output
    dplyr::relocate(dplyr::all_of(param_names), .after = .row) %>%
    dplyr::rename(.config = .iter_config)
}


pull_h2o_predictions <- function(h2o_model, val_frame, val_truth, orig_rows, mode) {
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

  h2o_preds %>%
    dplyr::bind_cols(val_truth)
}



pull_h2o_metrics <- function(prediction,
                             metrics,
                             param_names,
                             outcome_name,
                             event_level) {

  estimate_metrics_safely <- purrr::safely(tune:::estimate_metrics)
  metrics <- estimate_metrics_safely(prediction,
                                     metrics,
                                     param_names,
                                     outcome_name,
                                     event_level)
  if (is.null(metrics$error)) {
    metrics$result
  } else {
    NULL
  }
}
