
There are two main components in agua:

- New parsnip engine `'h2o'` for many models, see the
  [vignette](https:://agua.tidymodels.org/articles/agua.html) for a
  complete list.

- Infrastructure for the tune package.

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

## Examples

The following code demonstrates how to create a single model on the h2o
server and how to make predictions.

``` r
library(tidymodels)
library(agua)

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
#> Model ID:  DRF_model_R_1664991442414_1 
#> Model Summary: 
#>   number_of_trees number_of_internal_trees model_size_in_bytes min_depth
#> 1            1000                     1000              285901         4
#>   max_depth mean_depth min_leaves max_leaves mean_leaves
#> 1        10    6.70600         10         27    18.04100
#> 
#> 
#> H2ORegressionMetrics: drf
#> ** Reported on training data. **
#> ** Metrics reported on Out-Of-Bag training samples **
#> 
#> MSE:  4.354
#> RMSE:  2.087
#> MAE:  1.658
#> RMSLE:  0.09849
#> Mean Residual Deviance :  4.354

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
