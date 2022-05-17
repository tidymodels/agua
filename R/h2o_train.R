

h2o_train <- function(x, y, model, ...) {
  opts <- get_fit_opts(...)
  x <- as.data.frame(x)
  x_names <- names(x)
  x$.outcome <- y
  x <- r_h2o(x)

  mod_fun <- paste0("h2o.", model)
  cl <-
    rlang::call2(
      mod_fun,
      .ns = "h2o",
      x = quote(x_names),
      y = ".outcome",
      training_frame = quote(x),
      !!!opts
    )
  rlang::eval_tidy(cl)
}

