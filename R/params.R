values_h2o_activation <- c(
  "Rectifier", "RectifierWithDropout", "Tanh",
  "TanhWithDropout", "Maxout", "MaxoutWithDropout"
)

values_h2o_split <- c(
  "AUTO", "UniformAdaptive", "Random", "QuantilesGlobal",
  "RoundRobin", "UniformRobust"
)

#' Tuning parameters in h2o
#' @rdname h2o-tuning-params
#' @keywords internal
#' @export
#' @examples
#' h2o_activation()
h2o_activation <- function(values = values_h2o_activation) {
  dials::new_qual_param(
    type = "character",
    values = values,
    default = "none",
    label = c(h2o_activation = "Activation function")
  )
}


#' @rdname h2o-tuning-params
#' @keywords internal
#' @export
h2o_split <- function(values = values_h2o_split) {
  dials::new_qual_param(
    type = "character",
    values = values,
    default = "none",
    label = c(h2o_split = "Type of histogram to find optimal splits")
  )
}
