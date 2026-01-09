library(testthat)
library(MASS)
test_that("compute_fp returns correct form", {
  data(geyser)

  fp <- compute_fp(geyser$waiting, bins = 10)

  expect_s3_class(fp, "frequency_polygon")
  expect_type(fp$x, "double")
  expect_type(fp$y, "double")
  expect_length(fp$x, 10)
  expect_length(fp$y, 10)
})


test_that("Inputs that arent numeric gives error", {
  expect_error(
    compute_fp(c("a", "b", "c")),
    "numeric"
  )
})

test_that("Not enough observations gives error", {
  expect_error(
    compute_fp(1),
    "at least two"
  )
})

