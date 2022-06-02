#' @export
#' @rdname h2o-tuning-parameters
h2o_activation <- function(values = c("Rectifier",
                                      "RectifierWithDropout",
                                      "Tanh",
                                      "TanhWithDropout",
                                      "Maxout",
                                      "MaxoutWithDropout")) {
  dials::new_qual_param(
    type = "character",
    values = values,
    default = "none",
    label = c(h2o_activation = "Activation function")
  )
}
