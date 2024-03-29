#' Model wrappers for h2o
#'
#' Basic model wrappers for h2o model functions that include data conversion,
#' seed configuration, and so on.
#'
#' @inheritParams h2o::h2o.randomForest
#' @inheritParams h2o::h2o.xgboost
#' @inheritParams h2o::h2o.gbm
#' @inheritParams h2o::h2o.glm
#' @inheritParams h2o::h2o.deeplearning
#' @inheritParams h2o::h2o.rulefit
#' @inheritParams h2o::h2o.naiveBayes
#' @param x A data frame of predictors.
#' @param y A vector of outcomes.
#' @param model A character string for the model. Current selections are
#' `"automl"`, `"randomForest"`, `"xgboost"`, `"gbm"`, `"glm"`, `"deeplearning"`,
#' `"rulefit"` and `"naiveBayes"`. Use [agua::h2o_xgboost_available()] to see
#' if xgboost can be used on your OS/h2o server.
#' @param weights A numeric vector of case weights.
#' @param validation An integer between 0 and 1 specifying the _proportion_ of
#' the data reserved as validation set. This is used by h2o for performance
#' assessment and potential early stopping. Default to 0.
#' @param verbosity Verbosity of the backend messages printed during training;
#' Must be one of NULL (live log disabled), "debug", "info", "warn", "error".
#' Defaults to NULL.
#' @param save_data A logical for whether training data should be saved on
#' the h2o server, set this to `TRUE` for AutoML models that needs to be
#' re-fitted.
#' @param ... Other options to pass to the h2o model functions (e.g.,
#' [h2o::h2o.randomForest()]).
#' @return An h2o model object.
#' @examplesIf agua:::should_run_examples()
#' # start with h2o::h2o.init()
#' if (h2o_running()) {
#'  # -------------------------------------------------------------------------
#'  # Using the model wrappers:
#'  h2o_train_glm(mtcars[, -1], mtcars$mpg)
#'
#'  # -------------------------------------------------------------------------
#'  # using parsnip:
#'
#'  spec <-
#'    rand_forest(mtry = 3, trees = 500) %>%
#'    set_engine("h2o") %>%
#'    set_mode("regression")
#'
#'  set.seed(1)
#'  mod <- fit(spec, mpg ~ ., data = mtcars)
#'  mod
#'
#'  predict(mod, head(mtcars))
#' }
#' @export
h2o_train <- function(x,
                      y,
                      model,
                      weights = NULL,
                      validation = NULL,
                      save_data = FALSE,
                      ...) {
  opts <- get_fit_opts(...)
  x <- as.data.frame(x)
  x_names <- names(x)
  x$.outcome <- y

  # if passed in case weights
  if (!is.null(weights)) {
    x$.weights <- weights
    opts$weights_column <- ".weights"
  }

  # if passed in validation, split x into train and validation set
  if (!is.null(validation)) {
    if (length(validation) > 1 || validation < 0 || validation > 1) {
      rlang::abort("`validation` should be a number between 0 and 1.")
    }
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
  if (!save_data) {
    on.exit(h2o::h2o.rm(x$id))
  }

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
  h2o::h2o.no_progress(rlang::eval_tidy(cl))
}

get_fit_opts <- function(...) {
  opts <- rlang::list2(...)
  if (!any(names(opts) == "seed")) {
    opts$seed <- sample.int(10^5, 1)
  }
  opts
}

#' @export
#' @rdname h2o_train
h2o_train_rf <- function(x, y, ntrees = 50, mtries = -1, min_rows = 1, ...) {
  if (mtries != -1 && mtries > ncol(x)) {
    rlang::abort("`mtry` should be smaller than the number of predictors.")
  }

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
           validation = NULL,
           ...) {
    if (!h2o_xgboost_available()) {
      msg <- paste0(
        "H2O's xgboost isn't available on this machine ",
        "try using the 'h2o_gbm' engine for `boost_tree()` ",
        "for gradient boosted trees instead."
      )
      rlang::abort(msg)
    }

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
h2o_train_gbm <-
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
           ...) {
    h2o_train(
      x,
      y,
      model = "gbm",
      ntrees = ntrees,
      max_depth = max_depth,
      min_rows = min_rows,
      learn_rate = learn_rate,
      sample_rate = sample_rate,
      col_sample_rate = col_sample_rate,
      stopping_rounds = stopping_rounds,
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
    if (is.null(opts$family)) {
      opts$family <- "AUTO"
    }
    # check for poisson reg
    if (opts$family == "poisson") {
      all_positive <- all(sum(y > 0))
      all_ints <- rlang::is_integerish(y)
      if (!(all_positive && all_ints)) {
        msg <- paste0(
          "Poisson regression expects the outcome ",
          "to be non-negative integers."
        )
        rlang::abort()
      }
    }
    # check for multinom reg
    if (opts$family == "multinomial") {
      if (nlevels(y) < 3) {
        msg <- paste0(
          "Multinomial regression expects the outcome ",
          "to be a factor with at least 3 levels."
        )
        rlang::abort(msg)
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
                          validation = NULL,
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
    msg <- glue::glue(paste0(
      "Activation function `{activation}` is not supported by the h2o engine. ",
      "Possible values are {toString(all_activations)}."
    ))
    rlang::abort(msg)
  }


  if (activation == "Rectifier" && hidden_dropout_ratios > 0) {
    activation <- "RectifierWithDropout"
  } else if (activation == "Tanh" && hidden_dropout_ratios > 0) {
    activation <- "TanhWithDropout"
  } else if (activation == "Maxout" && hidden_dropout_ratios > 0) {
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
    msg <- glue::glue(paste0(
      "`tree_depth` ({max_rule_length}) must be greater than the ",
      "engine argument `min_rule_length` ({opts$min_rule_length})."
    ))
    rlang::abort(msg)
  }

  if (is.null(opts$min_rule_length) && max_rule_length < 3) {
    msg <- glue::glue(paste0(
      "`tree_depth` ({max_rule_length}) must be greater than the ",
      "engine argument `min_rule_length`'s default value of 3."
    ))
    rlang::abort(msg)
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

#' @export
#' @rdname h2o_train
h2o_train_auto <- function(x, y, verbosity = NULL, save_data = FALSE, ...) {
  opts <- list(...)

  if (!is.null(opts$leaderboard_frame)) {
    opts$leaderboard_frame <- convert_frame(opts$leaderboard_frame, names(x))
  }
  if (!is.null(opts$blending_frame)) {
    opts$blending_frame <- convert_frame(opts$blending_frame, names(x))
  }

  cl <- rlang::call2(
    "h2o_train",
    .ns = "agua",
    x = quote(x),
    y = quote(y),
    model = "automl",
    verbosity = verbosity,
    save_data = save_data,
    !!!opts,
  )

  rlang::eval_tidy(cl)
}

convert_frame <- function(frame, x_names) {
  frame_x <- frame[x_names]
  y_name <- setdiff(names(frame), x_names)
  y <- frame[[y_name]]
  frame_x$.outcome <- y
  frame <- as_h2o(frame_x)
  on.exit(h2o::h2o.rm(frame$id))

  frame$data
}
