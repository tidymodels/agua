options("prefer_RCurl" = FALSE)
h2o_start()

test_that("data conversion", {
  expect_silent(cars_1 <- as_h2o(mtcars))
  expect_equal(class(cars_1$data), "H2OFrame")
  expect_equal(class(cars_1$id), "character")
  expect_equal(as_tibble(cars_1$data), tibble::as_tibble(mtcars))
})
