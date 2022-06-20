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
rank_automl(m)

# autoplot methdos
# average ranking position of algorithms in each metric
ggplot2::autoplot(m, type = "rank",
                  metric = c("r2", "mae", "rmse", "mean_residual_deviance"))

# tidy methods
m_tidy <- tidy(m, n = 5)
m_tidy

# helper to extract single candidate model
extract_automl_fit_parsnip(m, m_tidy[["model_id"]][[1]])

# varibale importance in metalearner, i.e. model importance of base learner
model_importance(m)

# can join with tibbles from other functions
model_importance(m) %>%
  left_join(
    rank_automl(m) %>%
      select(model_id, .metric, mean, rank),
    by = c("stacked_model_id" = "model_id")
  )
