# tools for tuning
all_algos <- c(
  "boost_tree", "rand_forest", "linear_reg", "logistic_reg",
  "multinom_reg", "mlp", "naive_Bayes", "auto_ml"
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
    auto_ml = "automl",
    rlang::abort(
      glue::glue("Model `{model_class}` is not supported by the h2o engine, use one of { toString(all_algos) }")
    )
  )
  algo
}

# ------------------------------------------------------------------------------
# Data conversions

#' Data conversion tools
#' @inheritParams tibble::as_tibble
#' @param df A R data frame.
#' @param destination_frame_prefix A character string to use as the base name.
#' @param x An H2OFrame.
#' @return A tibble or, for `as_h2o()`, a list with `data` (an H2OFrame) and
#' `id` (the id on the h2o server).
#' @examples
#'
#' # start with h2o::h2o.init()
#' if (h2o_running()) {
#'   cars2 <- as_h2o(mtcars)
#'   cars2
#'   class(cars2$data)
#'
#'   cars0 <- as_tibble(cars2$data)
#'   cars0
#' }
#' @export
as_h2o <- function(df, destination_frame_prefix = "object") {
  suffix <- paste0(sample(letters, size = 10, replace = TRUE), collapse = "")
  id <- paste(destination_frame_prefix, suffix, sep = "_")
  # fix when h2o exports
  data <- h2o:::with_no_h2o_progress(h2o::as.h2o(df, destination_frame = id))
  list(
    data = data,
    id = id
  )
}

#' @export
#' @rdname as_h2o
as_tibble.H2OFrame <-
  function(x,
           ...,
           .rows = NULL,
           .name_repair = c("check_unique", "unique", "universal", "minimal"),
           rownames = pkgconfig::get_config("tibble::rownames", NULL)) {
    x <- as.data.frame(x)
    tibble::as_tibble(x,
      ...,
      .rows = .rows,
      .name_repair = .name_repair,
      rownames = rownames
    )
  }

# ------------------------------------------------------------------------------
# translate parsnip labels into original h2o parameter names
extract_model_param_names_h2o <- function(model_param_names, workflow) {
  model_spec <- hardhat::extract_spec_parsnip(workflow)
  param <- hardhat::extract_parameter_set_dials(workflow) %>%
    tibble::as_tibble()

  arg_key <- parsnip::get_from_env(paste0(class(model_spec)[1], "_args")) %>%
    dplyr::filter(engine == "h2o")

  dplyr::inner_join(
    arg_key %>% dplyr::select(name = parsnip, original),
    param,
    by = "name"
  ) %>%
    purrr::pluck("original")
}

rename_grid_h2o <- function(grid, workflow) {
  rn <- parsnip::.model_param_name_key(workflow, as_tibble = FALSE)
  grid %>%
    dplyr::rename(!!!rn$user_to_parsnip) %>%
    dplyr::rename(!!!rn$parsnip_to_engine)
}

workflow_uses_automl <- function(x) {
  model_spec <- hardhat::extract_spec_parsnip(x)
  identical(class(model_spec)[1], "auto_ml")
}

eval_silently <- function(expr, envir = NULL) {
  junk <- capture.output(res <- try(rlang::eval_tidy(expr), silent = TRUE))
  if (length(junk) == 0) {
    return(res)
  }
}

# extract algorithm from model id
id_to_algorithm <- function(id, recode = TRUE) {
  algo <- tolower(sub("_.+", "", id))
  if (recode) {
    algo[algo == "xrt" | algo == "drf"] <- "random forests"
    algo[algo == "deeplearning"] <- "neural nets"
    algo[algo == "gbm"] <- "gradient boosting"
    algo[algo == "stackedensemble"] <- "stacking"
  }
  algo
}

# convert a h2o model to parsnip `model_fit` object
convert_h2o_parsnip <- function(x, spec, lvl = NULL, extra_class = "h2o_fit", ...) {
  res <- list(
    fit = x,
    spec = spec,
    elapsed = list(elapsed = NA_real_),
    lvl = lvl
  )
  class(res) <- c(
    extra_class,
    paste0("_", class(x)[1]),
    "model_fit"
  )
  res
}
