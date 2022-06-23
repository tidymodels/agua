#' Utility functions for working with the h2o server
#'
#' @param verbose Print out the message if no cluster is available.
#' @param id Model or frame id.
#' @examples
#' if (!h2o_running()) {
#'   h2o_start()
#' }
#' @rdname h2o-server
#' @export
h2o_start <- function() {
  res <- utils::capture.output(h2o:::with_no_h2o_progress(
    h2o::h2o.init()
  ), "output")
  invisible(res)
}

#' @rdname h2o-server
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

#' @rdname h2o-server
#' @export
h2o_remove <- function(id) {
  h2o::h2o.rm(id)
}

#' @rdname h2o-server
#' @export
h2o_remove_all <- function() {
  h2o::h2o.removeAll()
}

#' @rdname h2o-server
#' @export
h2o_get_model <- function(id) {
  if (!h2o_running()) {
    msg <- "No active connection to an H2O cluster. Run `h2o::h2o.init()` first."
    rlang::abort(msg)
  }
  res <- eval_silent(h2o:::with_no_h2o_progress(h2o::h2o.getModel(id)))
  if (is.null(res)) {
    rlang::abort("Model id does not exist on the h2o server.")
  }
  res
}

#' @rdname h2o-server
#' @export
h2o_get_frame <- function(id) {
  if (!h2o_running()) {
    msg <- "No active connection to an H2O cluster. Run `h2o::h2o.init()` first."
    rlang::abort(msg)
  }
  res <- eval_silent(h2o:::with_no_h2o_progress(h2o::h2o.getFrame(id)))
  if (!is.null(res)) {
    res
  }
}

#' @rdname h2o-server
#' @export
h2o_xgboost_available <- function() {
  "XGBoost" %in% h2o::h2o.list_core_extensions()
}
