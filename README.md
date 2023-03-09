
<!-- README.md is generated from README.Rmd. Please edit that file -->

# agua <a href="https://agua.tidymodels.org/"><img src="man/figures/logo.svg" align="right" height="139" /></a>

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/tidymodels/agua/branch/main/graph/badge.svg)](https://app.codecov.io/gh/tidymodels/agua?branch=main)
[![R-CMD-check](https://github.com/tidymodels/agua/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tidymodels/agua/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

agua enables users to fit, optimize, and evaluate models via
[H2O](https://h2o.ai/) using tidymodels syntax. Most users will not have
to use aqua directly; the features can be accessed via the new parsnip
computational engine `'h2o'`.

There are two main components in agua:

- New parsnip engine `'h2o'` for many models, see [Get
  started](https://agua.tidymodels.org/articles/agua.html) for a
  complete list.

- Infrastructure for the tune package.

When fitting a parsnip model, the data are passed to the h2o server
directly. For tuning, the data are passed once and instructions are
given to `h2o.grid()` to process them.

This work is based on @stevenpawley’s
[h2oparsnip](https://github.com/stevenpawley/h2oparsnip) package.
Additional work was done by Qiushi Yan for his 2022 summer internship at
Posit.

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

## Examples

The following code demonstrates how to create a single model on the h2o
server and how to make predictions.

``` r
library(tidymodels)
library(agua)
library(h2o)
tidymodels_prefer()
```

``` r
# Start the h2o server before running models
h2o_start()

# Demonstrate fitting parsnip models: 
# Specify the type of model and the h2o engine 
spec <-
  rand_forest(mtry = 3, trees = 1000) %>%
  set_engine("h2o") %>%
  set_mode("regression")

# Fit the model on the h2o server
set.seed(1)
mod <- fit(spec, mpg ~ ., data = mtcars)
mod
#> parsnip model object
#> 
#> Model Details:
#> ==============
#> 
#> H2ORegressionModel: drf
#> Model ID:  DRF_model_R_1656520956148_1 
#> Model Summary: 
#>   number_of_trees number_of_internal_trees model_size_in_bytes min_depth
#> 1            1000                     1000              285914         4
#>   max_depth mean_depth min_leaves max_leaves mean_leaves
#> 1        10    6.70600         10         27    18.04100
#> 
#> 
#> H2ORegressionMetrics: drf
#> ** Reported on training data. **
#> ** Metrics reported on Out-Of-Bag training samples **
#> 
#> MSE:  4.354249
#> RMSE:  2.086684
#> MAE:  1.657823
#> RMSLE:  0.09848976
#> Mean Residual Deviance :  4.354249

# Predictions
predict(mod, head(mtcars))
#> # A tibble: 6 × 1
#>   .pred
#>   <dbl>
#> 1  20.9
#> 2  20.8
#> 3  23.3
#> 4  20.4
#> 5  17.9
#> 6  18.7

# When done
h2o_end()
```

Before using the `'h2o'` engine, users need to run `agua::h2o_start()`
or `h2o::h2o.init()` to start the h2o server, which will be storing
data, models, and other values passed from the R session.

There are several package vignettes including:

- [Introduction to agua](https://agua.tidymodels.org/articles/agua.html)

- [Model tuning](https://agua.tidymodels.org/articles/tune.html)

- [Automatic machine
  learning](https://agua.tidymodels.org/articles/auto_ml.html)

- [Parallel processing with agua and
  h2o](https://agua.tidymodels.org/articles/parallel.html)

## Code of Conduct

Please note that the agua project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
