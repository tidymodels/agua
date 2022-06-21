library(agua)
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
h2o_start()
mod <- auto_ml() %>%
  set_engine("h2o", max_runtime_secs = 10,
             verbosity = NULL) %>%
  set_mode("regression")

m <- mod %>%
  fit(mpg ~ ., data = mtcars)
m

# rank all algorithms by cross validation performance
rank_results_automl(m)

# autoplot methdos
# average ranking position of algorithms in each metric
ggplot2::autoplot(m, type = "rank",
                  metric = c("r2", "mae", "rmse", "mean_residual_deviance"))

# tidy methods
m_tidy <- tidy(m, n = 5)
m_tidy

# extract single candidate model, default to leader
extract_fit_parsnip(m)
extract_fit_parsnip(m, "GBM_5_AutoML_18_20220621_145456")


# variable importance in metalearner, i.e. model importance of base learner
imp_stacking(m) %>% tidyr::unnest(importance)

# can join with tibbles from other functions
imp_stacking(m) %>%
  left_join(
    rank_results_automl(m) %>%
      select(id, .metric, mean, rank),
    by = c("stacked_model_id" = "id")
  )
