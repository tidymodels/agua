#' Tuning parameters in h2o
#' @rdname h2o-tuning-params
#' @keywords internal
#' @export
#' @examples
#' h2o_activation()
h2o_activation <- function(values = c(
                             "Rectifier",
                             "RectifierWithDropout",
                             "Tanh",
                             "TanhWithDropout",
                             "Maxout",
                             "MaxoutWithDropout"
                           )) {
  dials::new_qual_param(
    type = "character",
    values = values,
    default = "none",
    label = c(h2o_activation = "Activation function")
  )
}
