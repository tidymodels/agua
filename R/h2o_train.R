#' Model wrappers for h2o
#'
#' Basic model wrappers for h2o model functions that include data conversion,
#' seed configuration, and so on.
#'
#' @inheritParams h2o::h2o.randomForest
#' @inheritParams h2o::h2o.xgboost
#' @inheritParams h2o::h2o.glm
#' @inheritParams h2o::h2o.deeplearning
#' @inheritParams h2o::h2o.rulefit
#' @inheritParams h2o::h2o.naiveBayes
#' @param x A data frame of predictors
#' @param y A vector of outcomes.
#' @param model A character string for the model. Current selections are
#' `"randomForest"`, `"xgboost"`, and `"glm"`. Use [h2o::h2o.xgboost.available()]
#' to see if that model can be used on your OS/h2o server.
#' @param validation The _proportion_ of the data that are used for performance
#' assessment and potential early stopping.
#' @param ... Other options to pass to the h2o model functions (e.g.,
#' [h2o::h2o.randomForest()]).
#' @return An h2o model object.
#' @examples
#' # start with h2o::h2o.init()
#'
#' if (h2o_running()) {
#'   # -------------------------------------------------------------------------
#'   # Using the model wrappers:
#'   h2o_train_glm(mtcars[, -1], mtcars$mpg)
#'
#'   # -------------------------------------------------------------------------
#'   # using parsnip:
#'
#'   spec <-
#'     rand_forest(mtry = 3, trees = 1000) %>%
#'     set_engine("h2o") %>%
#'     set_mode("regression")
#'
#'   set.seed(1)
#'   mod <- fit(spec, mpg ~ ., data = mtcars)
#'   mod
#'
#'   predict(mod, head(mtcars))
#' }
#' @export
#' @details
#' For fitting neural networks with `h2o_train_mlp()`, the underlying h2o
#' function [h2o::h2o.deeplearning()] ignores the seed and can produce different
#' models that cannot be reproduced.
h2o_train <- function(x, y, model, ...) {
  opts <- get_fit_opts(...)
  x <- as.data.frame(x)
  x_names <- names(x)
  x$.outcome <- y

  validation <- opts$validation
  opts$validation <- NULL

  if (!is.null(validation) && validation > 0) {
    # split x into train and validation set
    n <- nrow(x)
    m <- floor(n * (1 - validation)) + 1
    train_index <- sample(1:n, size = max(m, 2))
    validation_frame <- x[-train_index, , drop = FALSE]
    x <- x[train_index, , drop = FALSE]
    validation_frame <- as_h2o(validation_frame)
    opts$validation_frame <- validation_frame$data
    on.exit(h2o::h2o.rm(validation_frame$id))
  }

  x <- as_h2o(x)
  on.exit(h2o::h2o.rm(x$id))

  mod_fun <- paste0("h2o.", model)
  cl <-
    rlang::call2(
      mod_fun,
      .ns = "h2o",
      x = quote(x_names),
      y = ".outcome",
      training_frame = quote(x$data),
      !!!opts
    )
  h2o:::with_no_h2o_progress(rlang::eval_tidy(cl))
}

get_fit_opts <- function(...) {
  opts <- list(...)
  if (!any(names(opts) == "seed")) {
    opts$seed <- sample.int(10^5, 1)
  }
  opts
}

#' @export
#' @rdname h2o_train
h2o_train_rf <- function(x, y, ntrees = 50, mtries = -1, min_rows = 1, ...) {
  h2o_train(
    x,
    y,
    model = "randomForest",
    ntrees = ntrees,
    mtries = mtries,
    min_rows = min_rows,
    ...
  )
}



#' @export
#' @rdname h2o_train
h2o_train_xgboost <-
  function(x,
           y,
           ntrees = 50,
           max_depth = 6,
           min_rows = 1,
           learn_rate = 0.3,
           sample_rate = 1,
           col_sample_rate = 1,
           min_split_improvement = 0,
           stopping_rounds = 0,
           validation = 0,
           ...) {
    h2o_train(
      x,
      y,
      model = "xgboost",
      ntrees = ntrees,
      max_depth = max_depth,
      min_rows = min_rows,
      learn_rate = learn_rate,
      sample_rate = sample_rate,
      col_sample_rate = col_sample_rate,
      stopping_rounds = stopping_rounds,
      validation = validation,
      ...
    )
  }

#' @export
#' @rdname h2o_train
h2o_train_glm <-
  function(x,
           y,
           lambda = NULL,
           alpha = NULL,
           ...) {

    opts <- list(...)
    if (length(opts) >= 1 && opts$family == "poisson") {
      all_positive <- all(sum(y > 0))
      all_ints <- rlang::is_integerish(y)
      if (!(all_positive && all_ints)) {
        rlang::abort("Poisson regression expects non-negative integer response.")
      }
    }

    h2o_train(
      x,
      y,
      model = "glm",
      lambda = lambda,
      alpha = alpha,
      ...
    )
  }

#' @export
#' @rdname h2o_train
h2o_train_nb <- function(x, y, laplace = 0, ...) {
  h2o_train(
    x,
    y,
    model = "naiveBayes",
    laplace = laplace,
    ...
  )
}

#' @export
#' @rdname h2o_train
h2o_train_mlp <- function(x, y,
                          hidden = 200,
                          l2 = 0,
                          hidden_dropout_ratios = 0,
                          epochs = 10,
                          activation = "Rectifier",
                          validation = 0,
                          ...) {
  activation <- switch(activation,
    relu = "Rectifier",
    tanh = "Tanh",
    activation
  )

  all_activations <- c(
    "Rectifier", "Tanh", "TanhWithDropout",
    "RectifierWithDropout", "Maxout", "MaxoutWithDropout"
  )
  if (!(activation %in% all_activations)) {
    rlang::abort(
      glue::glue(
        "Activation function `{activation}` is not supported by the h2o engine. Possible values are {toString(all_activations)}."
      )
    )
  }


  if (activation == "Rectifier" & hidden_dropout_ratios > 0) {
    activation <- "RectifierWithDropout"
  } else if (activation == "Tanh" & hidden_dropout_ratios > 0) {
    activation <- "TanhWithDropout"
  } else if (activation == "Maxout" & hidden_dropout_ratios > 0) {
    activation <- "MaxoutWithDropout"
  }

  if (hidden_dropout_ratios == 0) {
    hidden_dropout_ratios <- NULL
  }

  h2o_train(
    x,
    y,
    model = "deeplearning",
    hidden = hidden,
    l2 = l2,
    hidden_dropout_ratios = hidden_dropout_ratios,
    epochs = epochs,
    activation = activation,
    validation = validation,
    ...
  )
}

#' @export
#' @rdname h2o_train
h2o_train_rule <- function(x, y,
                           rule_generation_ntrees = 50,
                           max_rule_length = 5,
                           lambda = NULL,
                           ...) {
  opts <- list(...)
  if (!is.null(opts$min_rule_length) && max_rule_length < opts$min_rule_length) {
    rlang::abort(
      glue::glue("`tree_depth` ({max_rule_length}) must be greater than the engine argument `min_rule_length` ({opts$min_rule_length}).")
    )
  }

  if (is.null(opts$min_rule_length) && max_rule_length < 3) {
    rlang::abort(
      glue::glue("`tree_depth` ({max_rule_length}) must be greater than the engine argument `min_rule_length`'s default value of 3.")
    )
  }


  h2o_train(
    x,
    y,
    model = "rulefit",
    rule_generation_ntrees = rule_generation_ntrees,
    max_rule_length = max_rule_length,
    lambda = lambda,
    ...
  )
}
