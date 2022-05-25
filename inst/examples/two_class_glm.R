library(modeldata)
library(parsnip)
library(tidyr)
library(rsample)
library(recipes)
library(h2o)
library(foreach)
library(tune)
library(workflows)
library(foreach)
h2o.init()

data(two_class_dat)
doParallel::registerDoParallel()
# model and workflow spec
folds <- vfold_cv(two_class_dat, nfolds = 10)
glm_spec <- logistic_reg(penalty = tune("lambda")) %>%
  set_engine("h2o") %>%
  set_mode("classification")

rec <- recipe(Class ~ A + B, two_class_dat) %>%
  step_ns(A, deg_free = tune("spline df"))

wf <- workflow() %>%
  add_model(glm_spec) %>%
  add_recipe(rec)

# build grid_info and looping iterators
pset <- hardhat::extract_parameter_set_dials(wf)
param_names <- pset$id
model_param_names <- dplyr::filter(pset, source == "model_spec")$id
grid <- tidyr::expand_grid(
  `spline df` = c(1, 3, 5, 10, 15),
  `lambda` = c(1e-5, 1e-4, 1e-3, 1e-2, 1e-1)
)

grid <- tune:::check_grid(grid = grid, workflow = wf, pset = pset)
grid_info <- tune:::compute_grid_info(wf, grid)
control <- control_grid(verbose = TRUE, save_pred = TRUE)
metrics <- check_metrics(NULL, wf)

packages <- c(control$pkgs, required_pkgs(wf))
n_resamples <- nrow(folds)
iterations <- seq_len(n_resamples)
n_grid_info <- nrow(grid_info)
rows <- seq_len(n_grid_info)
splits <- folds$splits


# model and preprocessor params

cols <- rlang::expr(
  c(
    .iter_model,
    .iter_config,
    .msg_model,
    dplyr::all_of(model_param_names)
  )
)
out_notes <-
  tibble::tibble(location = character(0), type = character(0), note = character(0))
grid_info_nest <- tidyr::nest(grid_info, data = !!cols)

# looping through resamples
seeds <- tune:::generate_seeds(rng = TRUE, n_resamples)

results <- foreach::foreach(
  split = splits,
  seed = seeds,
  .packages = packages,
  .errorhandling = "pass"
) %dopar% {
  tune_grid_loop_iter_h2o(
    split = split,
    grid_info = grid_info_nest,
    workflow = wf,
    metrics = metrics,
    control = control,
    seed = seed
  )
}

purrr::map(results, purrr::pluck, ".metrics")

dplyr::bind_rows(!!!list(iris, iris))


results[[1]]
results[[1]]$.predictions[[1]]

results[[1]] %>%
  select(.predictions) %>%
  unnest(.predictions)


results[[1]] %>%
  select(.metrics) %>%
  unnest(.metrics)
