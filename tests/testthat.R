library(testthat)
library(agua)

options("prefer_RCurl" = FALSE)
h2o_start()
test_check("agua")
