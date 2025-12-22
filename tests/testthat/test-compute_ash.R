library(testthat)
library(MASS)

data(geyser)
wait_data <- geyser$wait

test_that("compute_ash returns correct structure and valid density", {
  h_val <- 9
  m_val <- 3
  res <- compute_ash(wait_data, h = h_val, m = m_val)
  # Test the S3 class membership
  expect_s3_class(res, "ash_density")
  # Test the output type
  expect_type(res, "list")
  # Test that density values (y) are never negative
  expect_true(all(res$y >= 0))
  # Test that x and y have the same length
  expect_equal(length(res$x), length(res$y))

  # waiting for the validate_density function
  # integral_val <- validate_density(res)
  # expect_equal(integral_val, 1, tolerance = 0.01)

  # Test that it handles parameters correctly in the output list
  expect_identical(res$parameters$h, h_val)
  expect_identical(res$parameters$m, m_val)
})

test_that("compute_ash handles edge cases or errors", {
  # Test that the function fails if data is not numeric
  expect_error(compute_ash("not numeric data", h = 9, m = 3))
  small_data <- c(10, 12, 11, 15)
  # Test that the function handles small datasets without crashing
  expect_no_error(compute_ash(small_data, h = 2, m = 2))
  # Test that the function fails if required parameter 'h' is missing
  expect_error(compute_ash(wait_data))
})
