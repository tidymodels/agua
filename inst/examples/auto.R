library(agua)
library(ggplot2)
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
h2o_start()
mod <- auto_ml() %>%
  set_engine("h2o", max_runtime_secs = 10,
             verbosity = NULL,
             seed = 1) %>%
  set_mode("regression")

m <- mod %>%
  fit(mpg ~ ., data = mtcars)

# rank all algorithms by cross validation performance
rank_results_automl(m)
# metrics
collect_metrics(m)
collect_metrics(m, summarize = FALSE)

# autoplot methods for plotting cross validation performances
# plot ranking
autoplot(m, type = "rank",
         metric = c("r2", "mae", "rmse", "mean_residual_deviance")) +
  theme(legend.position = "none")
# plot metric value
autoplot(m, type = "metric") +
  theme(legend.position = "none")

# tidy methods, returns leaderboard in tidy format
m_tidy <- tidy(m, n = 5)
m_tidy
# extract single candidate model, default to leader
extract_fit_parsnip(m)
extract_fit_parsnip(m, m_tidy$id[5])

# variable importance in metalearner, i.e. model importance of base learner
weights <- member_weights(m) %>%
  tidyr::unnest(importance)
weights
ggplot(weights, aes(algorithm, value)) +
  geom_boxplot() +
  facet_wrap(~ type)

# can join with tibbles from other functions
member_weights(m) %>%
  left_join(
    rank_results_automl(m) %>%
      select(id, .metric, mean, rank),
    by = c("ensemble_id" = "id")
  )
