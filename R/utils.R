all_algos <- c(
  "boost_tree", "rand_forest", "linear_reg", "logistic_reg",
  "multinom_reg", "mlp", "naive_Bayes"
)

extract_h2o_algorithm <- function(workflow, ...) {
  model_spec <- hardhat::extract_spec_parsnip(workflow)
  model_class <- class(model_spec)[1]
  algo <- switch(model_class,
    boost_tree = "gbm",
    rand_forest = "randomForest",
    linear_reg = "glm",
    logistic_reg = "glm",
    multinom_reg = "glm",
    mlp = "deeplearning",
    naive_Bayes = "naive_bayes",
    rlang::abort(
      glue::glue("Model `{model_class}` is not supported by the h2o engine, use one of { toString(all_algos) }")
    )
  )
  algo
}

as_h2o <- function(df, destination_frame_prefix) {
  id <- paste(destination_frame_prefix, runif(1), sep = "_")
  list(
    data = as.h2o(df, destination_frame = id),
    id = id
  )
}

is_h2o <- function(workflow, ...) {
  model_spec <- hardhat::extract_spec_parsnip(object)
  identical(model_spec$engine, "h2o")
}




rename_grid_h2o <- function(grid, workflow) {
  model_spec <- hardhat::extract_spec_parsnip(workflow)
  # For translate from given names/ids in grid to parsnip names:
  params <- model_spec %>% extract_parameter_set_dials()
  params <- tibble::as_tibble(params)
  pset <- params$id
  names(pset) <- params$name
  grid_parsnip <- dplyr::rename(grid, !!!pset)

  # Go from parsnip names to h2o names
  arg_key <- get_from_env(paste0(class(model_spec)[1], "_args")) %>%
    dplyr::filter(engine == "h2o")
  # rename again
  pset <- arg_key$parsnip %>% purrr::set_names(arg_key$original)
  grid_h2o <- dplyr::rename(grid_parsnip, !!!pset)
  grid_h2o
}
