
# ------------------------------------------------------------------------------
# Data conversions

r_h2o <- function(x) {
  h2o::as.h2o(x)
  # TODO find a way to do this quietly withr::with_options and purrr::quietly
}

h2o_r <- function(x) {
  as.data.frame(x)
  # TODO to tibble
}

# ------------------------------------------------------------------------------

get_fit_opts <- function(...) {
  opts <- list(...)
  if (!any(names(opts) == "seed")) {
    opts$seed <- sample.int(10^5, 1)
  }
  opts
}


