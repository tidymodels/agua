
# ------------------------------------------------------------------------------
# Data conversions

#' Data conversion tools
#' @param x A R data frame (for `r_h2o`) or an H2OFrame (`h2o_r`).
#' @return A tibble or H2OFrame.
#' @examples
#'
#' # start with h2o::h2o.init()
#' if (h2o_running()) {
#'   cars2 <- r_h2o(mtcars)
#'   cars2
#'
#'   cars0 <- h2o_r(cars2)
#'   cars0
#'
#' }
#' @export
r_h2o <- function(x) {
  quiet_convert(x)$result
}

quiet_convert <- purrr::quietly(h2o::as.h2o)

#' @export
#' @rdname r_h2o
h2o_r <- function(x) {
  x <- as.data.frame(x)
  tibble::as_tibble(x)
}

# ------------------------------------------------------------------------------

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



# ------------------------------------------------------------------------------

get_fit_opts <- function(...) {
  opts <- list(...)
  if (!any(names(opts) == "seed")) {
    opts$seed <- sample.int(10^5, 1)
  }
  opts
}


