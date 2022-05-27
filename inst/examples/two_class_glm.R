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
param_grid <- tidyr::expand_grid(
  `spline df` = c(1, 3, 5, 10, 15),
  `lambda` = c(1e-5, 1e-4, 1e-3, 1e-2, 1e-1)
)

grid <- tune:::check_grid(grid = param_grid,
                          workflow = wf,
                          pset = pset)
grid_info <- tune:::compute_grid_info(wf, grid)
control <- control_grid(verbose = TRUE, save_pred = TRUE)
metrics <- check_metrics(NULL, wf)
packages <- c(control$pkgs, required_pkgs(wf))
splits <- folds$splits

# manual looping
seeds <- tune:::generate_seeds(rng = TRUE, n_resamples)
results <- foreach::foreach(
  split = splits,
  seed = seeds,
  .packages = packages,
  .errorhandling = "pass"
) %dopar% {
  tune_grid_loop_iter_h2o(
    split = split,
    grid_info = grid_info,
    workflow = wf,
    metrics = metrics,
    control = control,
    seed = seed
  )
}

# debugging
# debug(tune_grid_loop_iter_h2o)
tune_grid_loop_iter_h2o(
  split = splits[[1]],
  grid_info = grid_info,
  workflow = wf,
  metrics = metrics,
  control = control,
  seed = seeds[[1]]
)

# no recipe
wf_no_rec <- wf %>%
  remove_recipe() %>%
  add_formula(Class ~ A + B)
param_grid_no_rec <- tidyr::expand_grid(
  lambda = c(1e-5, 1e-4, 1e-3, 1e-2, 1e-1)
)
grid_no_rec <- tune:::check_grid(grid = param_grid_no_rec,
                          workflow = wf_no_rec,
                          pset = hardhat::extract_parameter_set_dials(wf_no_rec))
grid_info_no_rec <- tune:::compute_grid_info(wf_no_rec, grid_no_rec)
results_no_rec <- foreach::foreach(
  split = splits,
  seed = seeds,
  .packages = packages,
  .errorhandling = "pass"
) %dopar% {
  tune_grid_loop_iter_h2o(
    split = split,
    grid_info = grid_info_no_rec,
    workflow = wf_no_rec,
    metrics = metrics,
    control = control,
    seed = seed
  )
}

tune_grid_loop_iter_h2o(
  split = splits[[1]],
  grid_info = grid_info_no_rec,
  workflow = wf_no_rec,
  metrics = metrics,
  control = control,
  seed = seed
)

# use tune_grid
# need to use tidymodels/tune@agua branch
results <- tune_grid(
  wf,
  resamples = folds,
  grid = param_grid,
  control = control_grid(save_pred = TRUE)
)
