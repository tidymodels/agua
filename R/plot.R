#' Plot rankings and metrics of H2O AutoML results
#'
#' The `autoplot()` method plots cross validation performances of candidate
#' models in H2O AutoML output via facets on each metric.
#'
#' @param object A fitted `auto_ml()` model.
#' @param type A character value for whether to plot average ranking ("rank")
#' or metrics ("metric").
#' @param metric A character vector or NULL for which metric to plot.
#'  By default, all metrics will be shown via facets.
#' @param std_errs The number of standard errors to plot.
#' @param ... Other options to pass to `autoplot()`.
#' @return A ggplot object.
#' @examples
#' if (h2o_running()) {
#'   auto_fit <- auto_ml() %>%
#'     set_engine("h2o", max_runtime_secs = 10) %>%
#'     set_mode("regression") %>%
#'     fit(mpg ~ ., data = mtcars)
#'
#'   autoplot(auto_fit)
#' }
#' @rdname automl-autoplot
#' @export
autoplot.workflow <- function(object, ...) {
  autoplot(extract_fit_parsnip(object), ...)
}

# no need for autoplot._H2oAutoML
# parsnip:::autoplot.model_fit will dispatch
#' @rdname automl-autoplot
#' @export
autoplot.H2OAutoML <- function(object,
                               type = c("rank", "metric"),
                               metric = NULL,
                               std_errs = qnorm(0.95),
                               ...) {
  type <- match.arg(type)
  if (type == "rank") {
    res <- rank_results_automl(object, ...)
    if (!is.null(metric)) {
      res <- res %>% dplyr::filter(.metric %in% metric)
    }
    y_var <- "rank"
    facet_scales <- "free_y"
  } else if (type == "metric") {
    res <- collect_metrics(object, summarize = FALSE, ...)
    if (!is.null(metric)) {
      res <- res %>% dplyr::filter(.metric %in% metric)
    }
    y_var <- ".estimate"
    facet_scales <- "free"
  }

  res <- res %>%
    dplyr::group_by(algorithm, .metric) %>%
    dplyr::summarize(
      mean = mean(.data[[y_var]], na.rm = TRUE),
      std_err = sd(.data[[y_var]], na.rm = TRUE) / sqrt(dplyr::n()),
      .groups = "drop"
    )

  p <- res %>%
    ggplot2::ggplot(ggplot2::aes(mean, algorithm, color = algorithm)) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        xmin = mean - std_errs * std_err,
        xmax = mean + std_errs * std_err,
        width = 0.3
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
