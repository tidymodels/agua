test_that("data conversion", {
  # skip("h2o server issues with testthat")

  expect_silent(cars_1 <- as_h2o(mtcars))
  expect_equal(class(cars_1$data), "H2OFrame")
  expect_equal(class(cars_1$id), "character")
  expect_equal(as_tibble(cars_1$data), tibble::as_tibble(mtcars))
})


test_that("server functions", {
  # skip("h2o server issues with testthat")

  expect_silent(agua:::h2o_start())
  expect_true(h2o_running())
  h2o::h2o.shutdown(prompt = FALSE)
  expect_false(h2o_running())
})
