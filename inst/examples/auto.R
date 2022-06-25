library(agua)
library(ggplot2)
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
h2o_start()

mod <- auto_ml() %>%
  set_engine("h2o",
             max_runtime_secs = 10,
             save_data = TRUE,
             keep_cross_validation_predictions = TRUE,
             seed = 1) %>%
  set_mode("regression")

m <- mod %>%
  fit(mpg ~ ., data = mtcars)

# rank all algorithms by cross validation performance
# workflowsets::rank_results
rank_results_automl(m)

# tune::collect_metrics
collect_metrics(m)
collect_metrics(m, summarize = FALSE)

# autoplot methods for plotting cross validation performances
# plot ranking
autoplot(m, type = "rank",
         metric = c("mae", "rmse")) +
  theme(legend.position = "none")

# plot metric value
autoplot(m, type = "metric") +
  theme(legend.position = "none")

# tidy methods, returns leaderboard in tidy format
m_tidy <- tidy(m, n = 5)
m_tidy %>% mutate(
  .predictions = purrr::map(.model, predict, new_data = head(mtcars))
)
# extract single candidate model, default to leader
leader <- extract_fit_parsnip(m)
extract_fit_engine(m, m_tidy$id[[2]])

predict(leader, head(mtcars))

# variable importance in metalearner, i.e. model importance of base learner
weights <- member_weights(m) %>%
  tidyr::unnest(importance)

weights

ggplot(weights, aes(value, algorithm)) +
  geom_boxplot() +
  facet_wrap(~ type)

# can join with tibbles from other functions
member_weights(m) %>%
  left_join(
    rank_results_automl(m) %>%
      select(id, .metric, mean, rank),
    by = c("ensemble_id" = "id")
  )

# refit with additional 30s of training time
m2 <- refit(m, max_runtime_secs = 30)
m2
