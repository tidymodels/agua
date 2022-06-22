#' Plot the results of H2o AutoML
#'
#' This `autoplot()` method plots cross validation performances of candidate
#' models in H2O AutoML output via facets on each metric.
#'
#' @param object A fitted `auto_ml()` model.
#' @param type A single character of choices of "rank" (plotting average ranking
#'  of algorithms within metrics) or "metric" (plotting average value of metrics).
#' @param metric A character vector or NULL for which metric to plot.
#'  By default, all metrics will be shown via facets.
#' @param std_errs The number of standard errors to plot.
#' @param ... Other options to pass to `autoplot()`.
#' @return A ggplot object.
#' @export
#' @examples
#' if (h2o_running()) {
#'   m <- auto_ml() %>%
#'     set_engine("h2o", max_runtime_secs = 10) %>%
#'     set_mode("regression") %>%
#'     fit(mpg ~ ., data = mtcars)
#'
#'   autoplot(m)
#' }
autoplot.H2OAutoML <- function(object,
                               type = c("rank", "metric"),
                               metric = NULL,
                               std_errs = qnorm(0.95),
                               ...) {
  type <- match.arg(type)


  if (type == "rank") {
    results <- rank_results_automl(object, summarize = TRUE, ...)
    if (!is.null(metric)) {
      results <- results %>% dplyr::filter(.metric %in% metric)
    }
    res <- results %>%
      dplyr::group_by(algorithm, .metric) %>%
      dplyr::summarize(
        mean = mean(rank, na.rm = TRUE),
        std_err = sd(rank, na.rm = TRUE) / sqrt(dplyr::n()),
        .groups = "drop"
      )
    facet_scales <- "free_y"
  } else if (type == "metric") {
    results <- rank_results_automl(object, summarize = FALSE, ...)
    if (!is.null(metric)) {
      results <- results %>% dplyr::filter(.metric %in% metric)
    }
    res <- results %>%
      dplyr::group_by(algorithm, .metric) %>%
      dplyr::summarize(
        mean = mean(value, na.rm = TRUE),
        std_err = sd(value, na.rm = TRUE) / sqrt(dplyr::n()),
        .groups = "drop"
      )
    facet_scales <- "free"
  }

  p <- res %>%
    ggplot2::ggplot(ggplot2::aes(mean, algorithm, color = algorithm)) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        xmin = mean - std_errs * std_err,
        xmax = mean + std_errs * std_err,
        width = diff(range(results$rank)) / 100
      )
    )

  num_metrics <- length(unique(res$.metric))
  if (num_metrics > 1) {
    xlab <- if (type == "rank") "Ranking" else "Metric"
    res$.metric <- factor(res$.metric)
    p <- p +
      ggplot2::facet_wrap(~.metric, scales = facet_scales, as.table = FALSE) +
      ggplot2::labs(x = xlab, y = "Algorithm")
  } else {
    metric_name <- res$.metric[[1]]
    xlab <- if (type == "rank") paste0("Ranking on ", metric_name) else metric_name
    p <- p + ggplot2::labs(x = xlab, y = "Algorithm")
  }

  p
}
