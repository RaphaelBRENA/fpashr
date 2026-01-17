test_that("validate_density works with ash_density objects", {
  library(MASS)
  data(geyser)

  ash_result <- compute_ash(geyser$waiting, h = 9, m = 3)
  validation <- validate_density(ash_result)

  expect_s3_class(validation, "density_validation")
  expect_true(is.numeric(validation$area))
  expect_true(is.logical(validation$valid))
  expect_true(is.numeric(validation$deviation))
  expect_equal(validation$deviation, abs(validation$area - 1.0))
})

test_that("validate_density works with frequency_polygon objects", {
  data(faithful)

  fp_result <- compute_fp(faithful$waiting, bins = 15, density = TRUE)
  validation <- validate_density(fp_result)

  expect_s3_class(validation, "density_validation")
  expect_true(is.numeric(validation$area))
  expect_true(is.logical(validation$valid))
})

test_that("validate_density respects custom tolerance", {
  library(MASS)
  data(geyser)

  ash_result <- compute_ash(geyser$waiting, h = 9, m = 3)

  # Strict tolerance
  validation_strict <- validate_density(ash_result, tolerance = 0.001)
  expect_equal(validation_strict$tolerance, 0.001)

  # Loose tolerance
  validation_loose <- validate_density(ash_result, tolerance = 0.1)
  expect_equal(validation_loose$tolerance, 0.1)
  expect_true(validation_loose$valid)
})

test_that("validate_density throws error for invalid input", {
  expect_error(validate_density(list(x = 1:10)),
               "must be an object of class")

  expect_error(validate_density(data.frame(x = 1:10, y = 1:10)),
               "must be an object of class")
})

test_that("validate_density throws error for missing components", {
  bad_object <- list(x = 1:10)
  class(bad_object) <- "ash_density"

  expect_error(validate_density(bad_object),
               "must contain 'x' and 'y' components")
})

test_that("validate_density throws error for insufficient data", {
  bad_object <- list(x = 1, y = 1)
  class(bad_object) <- "ash_density"

  expect_error(validate_density(bad_object),
               "Need at least 2 points")
})

test_that("validate_density throws error for mismatched lengths", {
  bad_object <- list(x = 1:10, y = 1:5)
  class(bad_object) <- "ash_density"

  expect_error(validate_density(bad_object),
               "x and y must have the same length")
})

test_that("validate_density throws error for NA values", {
  bad_object <- list(x = c(1:9, NA), y = 1:10)
  class(bad_object) <- "ash_density"

  expect_error(validate_density(bad_object),
               "cannot contain NA values")
})

test_that("print method works for density_validation", {
  library(MASS)
  data(geyser)

  ash_result <- compute_ash(geyser$waiting, h = 9, m = 3)
  validation <- validate_density(ash_result)

  expect_output(print(validation), "Density Validation Results")
  expect_output(print(validation), "Total area under curve")
  expect_output(print(validation), "Valid density")
})
