library(testthat)
test_that("compute_fp returns correct structure", {
  data(faithful)

  fp <- compute_fp(faithful$waiting, bins = 10)

  expect_s3_class(fp, "frequency_polygon")
  expect_type(fp$x, "double")
  expect_type(fp$y, "double")
  expect_length(fp$x, 10)
  expect_length(fp$y, 10)
})

test_that("density integrates approximately to 1", {
  data(faithful)

  fp <- compute_fp(faithful$waiting, bins = 20)

  area <- sum(fp$y * fp$binwidth)
  expect_equal(area, 1, tolerance = 0.05)
})

test_that("non-numeric input throws error", {
  expect_error(
    compute_fp(c("a", "b", "c")),
    "numeric"
  )
})

test_that("too few observations throws error", {
  expect_error(
    compute_fp(1),
    "at least two"
  )
})

