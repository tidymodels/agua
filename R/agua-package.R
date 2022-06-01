#' tidymodels integration with h2o
#'
#' agua has tools that allows users to fit and tune models using the h2o
#' platform. Most functions in this package are not user facing; agua enables
#' new parsnip model engines and sets up additional infrastructure for tune.
#'
#' The package uses code initially written by Steven Pawley in his h2oparsnip
#' package. Addition work was done by Qiushi Yan as an RStudio summer intern.
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom parsnip set_model_arg
#' @importFrom tune tune
#' @importFrom stats predict
## usethis namespace: end
NULL

utils::globalVariables(
  c(
    ".iter_config", ".iter_preprocessor", ".iter_model", "data", "engine",
    "out_notes", "role", "parsnip", ".msg_model", "original", "predict"
  )
)
