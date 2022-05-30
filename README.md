
<!-- README.md is generated from README.Rmd. Please edit that file -->

# agua

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/tidymodels/agua/branch/main/graph/badge.svg)](https://app.codecov.io/gh/tidymodels/agua?branch=main)
[![R-CMD-check](https://github.com/tidymodels/agua/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tidymodels/agua/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

agua enables users to fit, optimize, and evaluate models via
[h2o](https://h2o.ai) using a tidymodels interface.

Most users will not have to use aqua directly; the features can be
accessed via a parsnip engine value of `'h2o'`.

There are two main components in agua:

-   parsnip engine definitions for the following models:
-   Infrastructure for the tune and finetune packages (not yet
    implemented).

When fitting a parsnip model, the data are passed to the h2o server
directly. For tuning, the data are passed once and instructions are
given to `h2o.grid()` to process them.

This work is based on @stevenpawley’s
[h2oparsnip](https://github.com/stevenpawley/h2oparsnip) package.
Additional work was done by Qiushi Yan for his 2022 summer internship at
RStudio.

## Installation

The CRAN version of the package can be installed via

``` r
install.packages("agua")
```

You can also install the development version of agua using:

``` r
require(pak)
pak::pak("tidymodels/agua")
```

## Example

The following code demonstrates how to create a single model on the h2o
server and how to make predictions.

``` r
library(tidymodels)
library(agua)

tidymodels_prefer()

# Start the h2o server before running models
logging <- capture.output(h2o::h2o.init())

# Demonstrate fitting parsnip models: 

if (h2o_running()) {

  # Specify the type of model
  spec <-
    rand_forest(mtry = 3, trees = 1000) %>%
    set_engine("h2o") %>%
    set_mode("regression")

  # Fit the model on the h2o server
  set.seed(1)
  mod <- fit(spec, mpg ~ ., data = mtcars)
  mod

  # Predictions
  predict(mod, head(mtcars))
  
  # When done
  h2o::h2o.shutdown(prompt = FALSE)
}
```

## Code of Conduct

Please note that the agua project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
