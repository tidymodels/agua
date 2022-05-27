# ------------------------------------------------------------------------------
# Tools for using the h2o model functions

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

# ------------------------------------------------------------------------------
# Data conversions

#' Data conversion tools
#' @inheritParams tibble::as_tibble
#' @param df A R data frame (for `r_h2o`).
#' @param destination_frame_prefix A character string to use as the base name.
#' @param x An H2OFrame.
#' @return A tibble or, for `as_h2o()`, a list with `data` (an H2OFrame) and
#' `id` (the id on the h2o server).
#' @examples
#'
#' # start with h2o::h2o.init()
#' if (h2o_running()) {
#'  cars2 <- as_h2o(mtcars)
#'  cars2
#'  class(cars2$data)
#'
#'  cars0 <- as_tibble(cars2$data)
#'  cars0
#' }
#' @export
as_h2o <- function(df, destination_frame_prefix = "object") {
  suffix <- paste0(sample(letters, size = 10, replace = TRUE), collapse = "")
  id <- paste(destination_frame_prefix, suffix, sep = "_")
  # Once h2o exports it, we should use the function with_no_h2o_progress
  res <- quiet_convert(df, destination_frame = id)
  list(
    data = res$result,
    id = id
  )
}
quiet_convert <- purrr::quietly(h2o::as.h2o)


#' @export
#' @rdname as_h2o
as_tibble.H2OFrame <-
  function (x,
            ...,
            .rows = NULL,
            .name_repair = c("check_unique", "unique", "universal", "minimal"),
            rownames = pkgconfig::get_config("tibble::rownames", NULL)) {
    x <- as.data.frame(x)
    tibble::as_tibble(x,
                      ...,
                      .rows = .rows,
                      .name_repair = .name_repair,
                      rownames = rownames)
  }

# ------------------------------------------------------------------------------

rename_grid_h2o <- function(grid, workflow) {
  model_spec <- hardhat::extract_spec_parsnip(workflow)
  # For translate from given names/ids in grid to parsnip names:
  params <- model_spec %>% extract_parameter_set_dials()
  params <- tibble::as_tibble(params)
  pset <- params$id
  names(pset) <- params$name
  grid_parsnip <- dplyr::rename(grid, !!!pset)

  # Go from parsnip names to h2o names
  arg_key <- parsnip::get_from_env(paste0(class(model_spec)[1], "_args")) %>%
    dplyr::filter(engine == "h2o")
  # rename again
  pset <- arg_key$parsnip
  names(pset) <- arg_key$original
  grid_h2o <- dplyr::rename(grid_parsnip, !!!pset)
  grid_h2o
}


quiet_start <- purrr::quietly(h2o::h2o.init)
h2o_start <- function() {
  res <- utils::capture.output(quiet_start(), "output")
}

# ------------------------------------------------------------------------------

#' Check if h2o cluster is initialized
#'
#' @param verbose Print out the message if no cluster is available.
#' @return A logical.
#' @examples
#' h2o_running()
#' h2o_running(verbose = TRUE)
#' @export
h2o_running <- function(verbose = FALSE) {
  res <- try(h2o::h2o.clusterIsUp(), silent = TRUE)
  if (inherits(res, "try-error")) {
    if (verbose) {
      msg <- as.character(res)
      rlang::inform(msg)
    }
    res <- FALSE
  }
  res
}


