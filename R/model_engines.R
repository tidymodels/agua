# TODO: model types: automl, h2o.gam, h2o.coxph
# raw prediction type for predict
class_info <- list(
  pre = NULL,
  post = NULL,
  func = c(pkg = "agua", fun = "h2o_predict_classification"),
  args = list(
    object = quote(object$fit),
    new_data = quote(new_data)
  )
)
class_info_prob <- class_info
class_info_prob$args$type <- "prob"

reg_info <- list(
  pre = NULL,
  post = NULL,
  func = c(pkg = "agua", fun = "h2o_predict_regression"),
  args = list(
    object = quote(object$fit),
    new_data = quote(new_data)
  )
)
reg_info_raw <- reg_info
reg_info_raw$args$type <- "raw"


add_linear_reg_h2o <- function() {
  parsnip::set_model_engine("linear_reg", "regression", "h2o")
  parsnip::set_dependency("linear_reg", "h2o", "h2o", "regression")
  parsnip::set_dependency("linear_reg", "h2o", "agua", "regression")

  parsnip::set_model_arg(
    model = "linear_reg",
    eng = "h2o",
    parsnip = "mixture",
    original = "alpha",
    func = list(pkg = "dials", fun = "mixture"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "linear_reg",
    eng = "h2o",
    parsnip = "penalty",
    original = "lambda",
    func = list(pkg = "dials", fun = "penalty"),
    has_submodel = FALSE
  )
  parsnip::set_fit(
    model = "linear_reg",
    eng = "h2o",
    mode = "regression",
    value = list(
      interface = "data.frame",
      protect = c("x", "y", "weights", "validation_frame"),
      func = c(pkg = "agua", fun = "h2o_train_glm"),
      defaults = list(
        family = "gaussian"
      )
    )
  )
  parsnip::set_encoding(
    model = "linear_reg",
    eng = "h2o",
    mode = "regression",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = FALSE,
      remove_intercept = TRUE,
      allow_sparse_x = FALSE
    )
  )
  parsnip::set_pred(
    model = "linear_reg",
    eng = "h2o",
    mode = "regression",
    type = "numeric",
    value = reg_info
  )
  parsnip::set_pred(
    model = "linear_reg",
    eng = "h2o",
    mode = "regression",
    type = "raw",
    value = reg_info_raw
  )
}

add_logistic_reg_h2o <- function() {
  parsnip::set_model_engine("logistic_reg", "classification", "h2o")
  parsnip::set_dependency("logistic_reg", "h2o", "h2o", "classification")
  parsnip::set_dependency("logistic_reg", "h2o", "agua", "classification")

  parsnip::set_model_arg(
    model = "logistic_reg",
    eng = "h2o",
    parsnip = "mixture",
    original = "alpha",
    func = list(pkg = "dials", fun = "mixture"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "logistic_reg",
    eng = "h2o",
    parsnip = "penalty",
    original = "lambda",
    func = list(pkg = "dials", fun = "penalty"),
    has_submodel = FALSE
  )
  parsnip::set_fit(
    model = "logistic_reg",
    eng = "h2o",
    mode = "classification",
    value = list(
      interface = "data.frame",
      protect = c("x", "y", "weights", "validation_frame"),
      func = c(pkg = "agua", fun = "h2o_train_glm"),
      defaults = list(
        family = "binomial"
      )
    )
  )
  parsnip::set_encoding(
    model = "logistic_reg",
    eng = "h2o",
    mode = "classification",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = FALSE,
      remove_intercept = TRUE,
      allow_sparse_x = FALSE
    )
  )
  parsnip::set_pred(
    model = "logistic_reg",
    eng = "h2o",
    mode = "classification",
    type = "class",
    value = class_info
  )
  parsnip::set_pred(
    model = "logistic_reg",
    eng = "h2o",
    mode = "classification",
    type = "prob",
    value = class_info_prob
  )
}

add_poisson_reg_h2o <- function() {
  parsnip::set_model_engine("poisson_reg", "regression", "h2o")
  parsnip::set_dependency("poisson_reg", "h2o", "h2o", "regression")
  parsnip::set_dependency("poisson_reg", "h2o", "agua", "regression")

  parsnip::set_model_arg(
    model = "poisson_reg",
    eng = "h2o",
    parsnip = "mixture",
    original = "alpha",
    func = list(pkg = "dials", fun = "mixture"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "poisson_reg",
    eng = "h2o",
    parsnip = "penalty",
    original = "lambda",
    func = list(pkg = "dials", fun = "penalty"),
    has_submodel = FALSE
  )
  parsnip::set_fit(
    model = "poisson_reg",
    eng = "h2o",
    mode = "regression",
    value = list(
      interface = "data.frame",
      protect = c("x", "y", "weights", "validation_frame"),
      func = c(pkg = "agua", fun = "h2o_train_glm"),
      defaults = list(
        family = "poisson"
      )
    )
  )
  parsnip::set_encoding(
    model = "poisson_reg",
    eng = "h2o",
    mode = "regression",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = FALSE,
      remove_intercept = TRUE,
      allow_sparse_x = FALSE
    )
  )
  parsnip::set_pred(
    model = "poisson_reg",
    eng = "h2o",
    mode = "regression",
    type = "numeric",
    value = reg_info
  )
  parsnip::set_pred(
    model = "poisson_reg",
    eng = "h2o",
    mode = "regression",
    type = "raw",
    value = reg_info_raw
  )
}

add_multinom_reg_h2o <- function() {
  parsnip::set_model_engine("multinom_reg", "classification", "h2o")
  parsnip::set_dependency("multinom_reg", "h2o", "h2o", "classification")
  parsnip::set_dependency("multinom_reg", "h2o", "agua", "classification")

  parsnip::set_model_arg(
    model = "multinom_reg",
    eng = "h2o",
    parsnip = "mixture",
    original = "alpha",
    func = list(pkg = "dials", fun = "mixture"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "multinom_reg",
    eng = "h2o",
    parsnip = "penalty",
    original = "lambda",
    func = list(pkg = "dials", fun = "penalty"),
    has_submodel = FALSE
  )
  parsnip::set_fit(
    model = "multinom_reg",
    eng = "h2o",
    mode = "classification",
    value = list(
      interface = "data.frame",
      protect = c("x", "y", "weights", "validation_frame"),
      func = c(pkg = "agua", fun = "h2o_train_glm"),
      defaults = list(
        family = "multinomial"
      )
    )
  )
  parsnip::set_encoding(
    model = "multinom_reg",
    eng = "h2o",
    mode = "classification",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = FALSE,
      remove_intercept = TRUE,
      allow_sparse_x = FALSE
    )
  )
  parsnip::set_pred(
    model = "multinom_reg",
    eng = "h2o",
    mode = "classification",
    type = "class",
    value = class_info
  )
  parsnip::set_pred(
    model = "multinom_reg",
    eng = "h2o",
    mode = "classification",
    type = "prob",
    value = class_info_prob
  )
}


add_rand_forest_h2o <- function() {
  parsnip::set_model_engine("rand_forest", "classification", "h2o")
  parsnip::set_model_engine("rand_forest", "regression", "h2o")
  parsnip::set_dependency("rand_forest", "h2o", "h2o")
  parsnip::set_dependency("rand_forest", "h2o", "agua")

  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "h2o",
    parsnip = "trees",
    original = "ntrees",
    func = list(pkg = "dials", fun = "trees"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "h2o",
    parsnip = "min_n",
    original = "min_rows",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "h2o",
    parsnip = "mtry",
    original = "mtries",
    func = list(pkg = "dials", fun = "mtry"),
    has_submodel = FALSE
  )
  parsnip::set_fit(
    model = "rand_forest",
    eng = "h2o",
    mode = "regression",
    value = list(
      interface = "data.frame",
      protect = c("x", "y", "weights", "validation_frame"),
      func = c(pkg = "agua", fun = "h2o_train_rf"),
      defaults = list()
    )
  )
  parsnip::set_fit(
    model = "rand_forest",
    eng = "h2o",
    mode = "classification",
    value = list(
      interface = "data.frame",
      protect = c("x", "y", "weights", "validation_frame"),
      func = c(pkg = "agua", fun = "h2o_train_rf"),
      defaults = list()
    )
  )
  parsnip::set_encoding(
    model = "rand_forest",
    eng = "h2o",
    mode = "classification",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )
  parsnip::set_encoding(
    model = "rand_forest",
    eng = "h2o",
    mode = "regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )
  # regression predict
  parsnip::set_pred(
    model = "rand_forest",
    eng = "h2o",
    mode = "regression",
    type = "numeric",
    value = reg_info
  )
  parsnip::set_pred(
    model = "rand_forest",
    eng = "h2o",
    mode = "regression",
    type = "raw",
    value = reg_info_raw
  )
  # classification predict
  parsnip::set_pred(
    model = "rand_forest",
    eng = "h2o",
    mode = "classification",
    type = "class",
    value = class_info
  )
  parsnip::set_pred(
    model = "rand_forest",
    eng = "h2o",
    mode = "classification",
    type = "prob",
    value = class_info_prob
  )
}

add_xgboost_h2o <- function() {
  parsnip::set_model_engine("boost_tree", "classification", "h2o")
  parsnip::set_model_engine("boost_tree", "regression", "h2o")
  parsnip::set_dependency("boost_tree", "h2o", "h2o")
  parsnip::set_dependency("boost_tree", "h2o", "agua")

  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "h2o",
    parsnip = "trees",
    original = "ntrees",
    func = list(pkg = "dials", fun = "trees"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "h2o",
    parsnip = "tree_depth",
    original = "max_depth",
    func = list(pkg = "dials", fun = "tree_depth"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "h2o",
    parsnip = "min_n",
    original = "min_rows",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "h2o",
    parsnip = "learn_rate",
    original = "learn_rate",
    func = list(pkg = "dials", fun = "learn_rate"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "h2o",
    parsnip = "sample_size",
    original = "sample_rate",
    func = list(pkg = "dials", fun = "sample_size"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "h2o",
    parsnip = "mtry",
    original = "col_sample_rate",
    func = list(pkg = "dials", fun = "mtry"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "h2o",
    parsnip = "loss_reduction",
    original = "min_split_improvement",
    func = list(pkg = "dials", fun = "loss_reduction"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "h2o",
    parsnip = "stop_iter",
    original = "stopping_rounds",
    func = list(pkg = "dials", fun = "stop_iter"),
    has_submodel = FALSE
  )
  parsnip::set_fit(
    model = "boost_tree",
    eng = "h2o",
    mode = "regression",
    value = list(
      interface = "data.frame",
      protect = c("x", "y", "weights", "validation_frame"),
      func = c(pkg = "agua", fun = "h2o_train_xgboost"),
      defaults = list()
    )
  )
  parsnip::set_fit(
    model = "boost_tree",
    eng = "h2o",
    mode = "classification",
    value = list(
      interface = "data.frame",
      protect = c("x", "y", "weights", "validation_frame"),
      func = c(pkg = "agua", fun = "h2o_train_xgboost"),
      defaults = list()
    )
  )
  parsnip::set_encoding(
    model = "boost_tree",
    eng = "h2o",
    mode = "classification",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )
  parsnip::set_encoding(
    model = "boost_tree",
    eng = "h2o",
    mode = "regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )
  # regression predict
  parsnip::set_pred(
    model = "boost_tree",
    eng = "h2o",
    mode = "regression",
    type = "numeric",
    value = reg_info
  )
  parsnip::set_pred(
    model = "boost_tree",
    eng = "h2o",
    mode = "regression",
    type = "raw",
    value = reg_info_raw
  )
  # classification predict
  parsnip::set_pred(
    model = "boost_tree",
    eng = "h2o",
    mode = "classification",
    type = "class",
    value = class_info
  )
  parsnip::set_pred(
    model = "boost_tree",
    eng = "h2o",
    mode = "classification",
    type = "prob",
    value = class_info_prob
  )
}

add_gbm_h2o <- function() {
  parsnip::set_model_engine("boost_tree", "classification", "h2o_gbm")
  parsnip::set_model_engine("boost_tree", "regression", "h2o_gbm")
  parsnip::set_dependency("boost_tree", "h2o_gbm", "h2o")
  parsnip::set_dependency("boost_tree", "h2o_gbm", "agua")

  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "h2o_gbm",
    parsnip = "trees",
    original = "ntrees",
    func = list(pkg = "dials", fun = "trees"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "h2o_gbm",
    parsnip = "tree_depth",
    original = "max_depth",
    func = list(pkg = "dials", fun = "tree_depth"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "h2o_gbm",
    parsnip = "min_n",
    original = "min_rows",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "h2o_gbm",
    parsnip = "learn_rate",
    original = "learn_rate",
    func = list(pkg = "dials", fun = "learn_rate"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "h2o_gbm",
    parsnip = "sample_size",
    original = "sample_rate",
    func = list(pkg = "dials", fun = "sample_size"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "h2o_gbm",
    parsnip = "mtry",
    original = "col_sample_rate",
    func = list(pkg = "dials", fun = "mtry"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "h2o_gbm",
    parsnip = "loss_reduction",
    original = "min_split_improvement",
    func = list(pkg = "dials", fun = "loss_reduction"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "h2o_gbm",
    parsnip = "stop_iter",
    original = "stopping_rounds",
    func = list(pkg = "dials", fun = "stop_iter"),
    has_submodel = FALSE
  )
  parsnip::set_fit(
    model = "boost_tree",
    eng = "h2o_gbm",
    mode = "regression",
    value = list(
      interface = "data.frame",
      protect = c("x", "y", "weights", "validation_frame"),
      func = c(pkg = "agua", fun = "h2o_train_gbm"),
      defaults = list()
    )
  )
  parsnip::set_fit(
    model = "boost_tree",
    eng = "h2o_gbm",
    mode = "classification",
    value = list(
      interface = "data.frame",
      protect = c("x", "y", "weights", "validation_frame"),
      func = c(pkg = "agua", fun = "h2o_train_gbm"),
      defaults = list()
    )
  )
  parsnip::set_encoding(
    model = "boost_tree",
    eng = "h2o_gbm",
    mode = "classification",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )
  parsnip::set_encoding(
    model = "boost_tree",
    eng = "h2o_gbm",
    mode = "regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )
  # regression predict
  parsnip::set_pred(
    model = "boost_tree",
    eng = "h2o_gbm",
    mode = "regression",
    type = "numeric",
    value = reg_info
  )
  parsnip::set_pred(
    model = "boost_tree",
    eng = "h2o_gbm",
    mode = "regression",
    type = "raw",
    value = reg_info_raw
  )
  # classification predict
  parsnip::set_pred(
    model = "boost_tree",
    eng = "h2o_gbm",
    mode = "classification",
    type = "class",
    value = class_info
  )
  parsnip::set_pred(
    model = "boost_tree",
    eng = "h2o_gbm",
    mode = "classification",
    type = "prob",
    value = class_info_prob
  )
}



add_naive_Bayes_h2o <- function() {
  parsnip::set_model_engine("naive_Bayes", "classification", "h2o")
  parsnip::set_dependency("naive_Bayes", "h2o", "h2o", "classification")
  parsnip::set_dependency("naive_Bayes", "h2o", "agua", "classification")

  parsnip::set_model_arg(
    model = "naive_Bayes",
    eng = "h2o",
    parsnip = "Laplace",
    original = "laplace",
    func = list(pkg = "dials", fun = "Laplace"),
    has_submodel = FALSE
  )
  parsnip::set_fit(
    model = "naive_Bayes",
    eng = "h2o",
    mode = "classification",
    value = list(
      interface = "data.frame",
      protect = c("x", "y", "weights", "validation_frame"),
      func = c(pkg = "agua", fun = "h2o_train_nb"),
      defaults = list()
    )
  )
  parsnip::set_encoding(
    model = "naive_Bayes",
    eng = "h2o",
    mode = "classification",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )
  parsnip::set_pred(
    model = "naive_Bayes",
    eng = "h2o",
    mode = "classification",
    type = "class",
    value = class_info
  )
  parsnip::set_pred(
    model = "naive_Bayes",
    eng = "h2o",
    mode = "classification",
    type = "prob",
    value = class_info_prob
  )
}

add_mlp_h2o <- function() {
  parsnip::set_model_engine("mlp", "classification", "h2o")
  parsnip::set_model_engine("mlp", "regression", "h2o")
  parsnip::set_dependency("mlp", "h2o", "h2o")
  parsnip::set_dependency("mlp", "h2o", "agua")

  parsnip::set_model_arg(
    model = "mlp",
    eng = "h2o",
    parsnip = "hidden_units",
    original = "hidden",
    func = list(pkg = "dials", fun = "hidden_units"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "mlp",
    eng = "h2o",
    parsnip = "penalty",
    original = "l2",
    func = list(pkg = "dials", fun = "penalty"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "mlp",
    eng = "h2o",
    parsnip = "dropout",
    original = "hidden_dropout_ratios",
    func = list(pkg = "dials", fun = "dropout"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "mlp",
    eng = "h2o",
    parsnip = "epochs",
    original = "epochs",
    func = list(pkg = "dials", fun = "epochs"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "mlp",
    eng = "h2o",
    parsnip = "activation",
    original = "activation",
    func = list(pkg = "agua", fun = "h2o_activation"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "mlp",
    eng = "h2o",
    parsnip = "learn_rate",
    original = "rate",
    func = list(pkg = "dials", fun = "learn_rate"),
    has_submodel = FALSE
  )

  parsnip::set_fit(
    model = "mlp",
    eng = "h2o",
    mode = "regression",
    value = list(
      interface = "data.frame",
      protect = c("x", "y", "weights", "validation_frame"),
      func = c(pkg = "agua", fun = "h2o_train_mlp"),
      defaults = list()
    )
  )
  parsnip::set_fit(
    model = "mlp",
    eng = "h2o",
    mode = "classification",
    value = list(
      interface = "data.frame",
      protect = c("x", "y", "weights", "validation_frame"),
      func = c(pkg = "agua", fun = "h2o_train_mlp"),
      defaults = list()
    )
  )
  parsnip::set_encoding(
    model = "mlp",
    eng = "h2o",
    mode = "classification",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = FALSE,
      remove_intercept = TRUE,
      allow_sparse_x = FALSE
    )
  )
  parsnip::set_encoding(
    model = "mlp",
    eng = "h2o",
    mode = "regression",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = FALSE,
      remove_intercept = TRUE,
      allow_sparse_x = FALSE
    )
  )
  # regression predict
  parsnip::set_pred(
    model = "mlp",
    eng = "h2o",
    mode = "regression",
    type = "numeric",
    value = reg_info
  )
  parsnip::set_pred(
    model = "mlp",
    eng = "h2o",
    mode = "regression",
    type = "raw",
    value = reg_info_raw
  )
  # classification predict
  parsnip::set_pred(
    model = "mlp",
    eng = "h2o",
    mode = "classification",
    type = "class",
    value = class_info
  )
  parsnip::set_pred(
    model = "mlp",
    eng = "h2o",
    mode = "classification",
    type = "prob",
    value = class_info_prob
  )
}

add_rule_fit_h2o <- function() {
  parsnip::set_model_engine("rule_fit", "classification", "h2o")
  parsnip::set_model_engine("rule_fit", "regression", "h2o")
  parsnip::set_dependency("rule_fit", "h2o", "h2o")
  parsnip::set_dependency("rule_fit", "h2o", "agua")

  parsnip::set_model_arg(
    model = "rule_fit",
    eng = "h2o",
    parsnip = "trees",
    original = "rule_generation_ntrees",
    func = list(pkg = "dials", fun = "trees"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "rule_fit",
    eng = "h2o",
    parsnip = "tree_depth",
    original = "max_rule_length",
    func = list(pkg = "dials", fun = "tree_depth"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "rule_fit",
    eng = "h2o",
    parsnip = "penalty",
    original = "lambda",
    func = list(pkg = "dials", fun = "penalty"),
    has_submodel = FALSE
  )
  parsnip::set_fit(
    model = "rule_fit",
    eng = "h2o",
    mode = "regression",
    value = list(
      interface = "data.frame",
      protect = c("x", "y", "weights", "validation_frame"),
      func = c(pkg = "agua", fun = "h2o_train_rule"),
      defaults = list()
    )
  )
  parsnip::set_fit(
    model = "rule_fit",
    eng = "h2o",
    mode = "classification",
    value = list(
      interface = "data.frame",
      protect = c("x", "y", "weights", "validation_frame"),
      func = c(pkg = "agua", fun = "h2o_train_rule"),
      defaults = list()
    )
  )
  parsnip::set_encoding(
    model = "rule_fit",
    eng = "h2o",
    mode = "classification",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )
  parsnip::set_encoding(
    model = "rule_fit",
    eng = "h2o",
    mode = "regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )
  # regression predict
  parsnip::set_pred(
    model = "rule_fit",
    eng = "h2o",
    mode = "regression",
    type = "numeric",
    value = reg_info
  )
  parsnip::set_pred(
    model = "rule_fit",
    eng = "h2o",
    mode = "regression",
    type = "raw",
    value = reg_info_raw
  )
  # classification predict
  parsnip::set_pred(
    model = "rule_fit",
    eng = "h2o",
    mode = "classification",
    type = "class",
    value = class_info
  )
  parsnip::set_pred(
    model = "rule_fit",
    eng = "h2o",
    mode = "classification",
    type = "prob",
    value = class_info_prob
  )
}

add_auto_ml_h2o <- function() {
  parsnip::set_model_engine("auto_ml", "classification", "h2o")
  parsnip::set_model_engine("auto_ml", "regression", "h2o")
  parsnip::set_dependency("auto_ml", "h2o", "h2o")
  parsnip::set_dependency("auto_ml", "h2o", "agua")

  parsnip::set_fit(
    model = "auto_ml",
    eng = "h2o",
    mode = "regression",
    value = list(
      interface = "data.frame",
      protect = c("x", "y", "weights", "validation_frame"),
      func = c(pkg = "agua", fun = "h2o_train_auto"),
      defaults = list(
        verbosity = NULL
      )
    )
  )
  parsnip::set_fit(
    model = "auto_ml",
    eng = "h2o",
    mode = "classification",
    value = list(
      interface = "data.frame",
      protect = c("x", "y", "weights", "validation_frame"),
      func = c(pkg = "agua", fun = "h2o_train_auto"),
      defaults = list(
        verbosity = NULL
      )
    )
  )
  parsnip::set_encoding(
    model = "auto_ml",
    eng = "h2o",
    mode = "classification",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )
  parsnip::set_encoding(
    model = "auto_ml",
    eng = "h2o",
    mode = "regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )
  # regression predict
  parsnip::set_pred(
    model = "auto_ml",
    eng = "h2o",
    mode = "regression",
    type = "numeric",
    value = reg_info
  )
  parsnip::set_pred(
    model = "auto_ml",
    eng = "h2o",
    mode = "regression",
    type = "raw",
    value = reg_info_raw
  )
  # classification predict
  parsnip::set_pred(
    model = "auto_ml",
    eng = "h2o",
    mode = "classification",
    type = "class",
    value = class_info
  )
  parsnip::set_pred(
    model = "auto_ml",
    eng = "h2o",
    mode = "classification",
    type = "prob",
    value = class_info_prob
  )
}
