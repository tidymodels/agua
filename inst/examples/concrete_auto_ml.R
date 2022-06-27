library(agua)
library(tidymodels)
library(doMC)
# ------------------------------------------------------------------------------

registerDoMC(cores = 10)
tidymodels_prefer()
h2o::h2o.init()
theme_set(theme_bw())

# ------------------------------------------------------------------------------
# used in a similar way in
# https://www.tmwr.org/ensembles.html
# https://www.tmwr.org/workflow-sets.html

data(concrete, package = "modeldata")

concrete <-
  concrete %>%
  group_by(across(-compressive_strength)) %>%
  summarize(compressive_strength = mean(compressive_strength),
            .groups = "drop")

# ------------------------------------------------------------------------------

set.seed(1501)
concrete_split <- initial_split(concrete, strata = compressive_strength)
concrete_train <- training(concrete_split)
concrete_test  <- testing(concrete_split)

set.seed(1502)
concrete_folds <-
  vfold_cv(concrete_train, strata = compressive_strength, repeats = 5)

normalized_rec <-
  recipe(compressive_strength ~ ., data = concrete_train) %>%
  step_normalize(all_predictors())

# ------------------------------------------------------------------------------

h2o_auto_spec <-
  auto_ml() %>%
  set_engine("h2o", max_runtime_secs = 60, seed = 1) %>%
  set_mode("regression")

h2o_auto_wflow <-
  workflow() %>%
  add_model(h2o_auto_spec) %>%
  add_recipe(normalized_rec)

# ------------------------------------------------------------------------------
h2o_auto_fit <- fit(h2o_auto_wflow, data = concrete_train)

ctrl_fit <- control_workflow(control_parsnip = control_parsnip(verbosity = 2))
h2o_auto_fit <- fit(h2o_auto_wflow, data = concrete_train,
                    control = ctrl_fit)

# Links to the ?rank_results_automl docs (and wherever we put an example article)
# What happens with parallel processing?
# Need rank_results_automl.workflow (and other methods like collect metrics)
# Note that recipes will not be resampled if given via a workflow. The engine doc might say something like:
# "When using autoML, h2o does internal cross-validation of the model fits (for
# the leaderboard and ranking). When using agua’s `auto_ml()` function with a
# workflow, please be aware that the internal cross-validation is not able to
# resample the recipe appropriately. For this reason, the statistics produced
# by the internal resampling may be optimistic."
stack_summary <-
  h2o_auto_fit %>%
  rank_results_automl() %>%
  filter(.metric == "mse")
stack_summary %>%
  filter(rank <= 20) %>%
  ggplot(aes(x = rank, y = mean, col = algorithm)) +
  geom_point()
h2o_auto_fit %>%
  extract_fit_parsnip() %>%
  member_weights() %>%
  filter(rank == 1) %>%
  unnest(importance) %>%
  filter(type == "scaled_importance" & value > 0) %>%
  mutate(
    member = factor(member),
    member = reorder(member, value)
  ) %>%
  ggplot(aes(x = value, y = member, col = algorithm)) +
  geom_point() +
  labs(x = "scaled importance", y = NULL)

test_pred <-
  predict(h2o_auto_fit, concrete_test) %>%
  bind_cols(concrete_test)

test_pred %>% rmse(compressive_strength, .pred)

test_pred %>%
  ggplot(aes(x = compressive_strength, y = .pred)) +
  geom_abline(color = "gray50", lty = 2) +
  geom_point(alpha = 0.5) +
  coord_obs_pred() +
  labs(x = "observed", y = "predicted")

# ------------------------------------------------------------------------------
# this fails:
h2o_auto_res <-
  h2o_auto_wflow %>%
  fit_resamples(resamples = concrete_folds)
collect_notes(h2o_auto_res)$note[[1]] %>% cat()

# TODO add some tuning of the recipe (say tune PCA comps) and
# use the automl model type
h2o::h2o.shutdown(prompt = FALSE)
