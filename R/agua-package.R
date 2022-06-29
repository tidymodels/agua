#' tidymodels integration with h2o
#'
#' agua allows users to fit and tune models using the H2O
#' platform with tidymodels syntax. The package provides a new parsnip
#' computational engine 'h2o' for various models and sets up additional
#' infrastructure for tune.
#'
#' The package uses code initially written by Steven Pawley in his h2oparsnip
#' package. Addition work was done by Qiushi Yan as an RStudio summer intern.
#'
#' @includeRmd man/rmd/agua-package-details.md details
#'
#' @docType package
#' @aliases agua agua-package
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom parsnip set_model_arg
#' @importFrom tune tune
#' @importFrom stats predict qnorm
#' @importFrom utils capture.output head
## usethis namespace: end
NULL

utils::globalVariables(
  c(
    ".iter_config", ".iter_preprocessor", ".iter_model", "data", "engine",
    "out_notes", "role", "parsnip", ".msg_model", "original", "predict", "name",
    ".metric", "algorithm", "value", "direction", "cv_id", "model_id", "id",
    "variable", ".model", "sd", "std_err", "mean", "member", "ensemble_id",
    ".data"
  )
)
