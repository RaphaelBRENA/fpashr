#' Validate Density Estimate
#'
#' Uses numerical integration to verify that the total area under a density
#' curve is approximately 1.0, as required for a valid probability density function.
#'
#' @param x An object of class \code{"ash_density"} or \code{"frequency_polygon"}
#'   produced by \code{compute_ash()} or \code{compute_fp()}.
#' @param tolerance Numeric. The acceptable deviation from 1.0. Defaults to 0.01.
#'
#' @return A list containing:
#'   \item{area}{The computed area under the density curve}
#'   \item{valid}{Logical indicating whether the area is within tolerance of 1.0}
#'   \item{deviation}{The absolute difference between the area and 1.0}
#'   \item{tolerance}{The tolerance level used}
#'
#' @details
#' This function uses the trapezoidal rule for numerical integration to compute
#' the area under the density curve. A valid probability density function should
#' integrate to 1.0 over its entire domain.
#'
#' @examples
#' library(MASS)
#' data(geyser)
#'
#' # Validate ASH density estimate
#' ash_result <- compute_ash(geyser$waiting, h = 9, m = 3)
#' validation <- validate_density(ash_result)
#' print(validation)
#'
#' # Validate frequency polygon density estimate
#' fp_result <- compute_fp(geyser$waiting, bins = 15, density = TRUE)
#' validation_fp <- validate_density(fp_result)
#' print(validation_fp)
#'
#' # Using a custom tolerance
#' validate_density(ash_result, tolerance = 0.05)
#'
#' @export
validate_density <- function(x, tolerance = 0.01) {
  # Check that input is of correct class
  if (!inherits(x, "ash_density") && !inherits(x, "frequency_polygon")) {
    stop("x must be an object of class 'ash_density' or 'frequency_polygon'")
  }

  # Check that object has required components
  if (!all(c("x", "y") %in% names(x))) {
    stop("x must contain 'x' and 'y' components")
  }

  # Extract x and y values
  x_vals <- x$x
  y_vals <- x$y

  # Check for valid data
  if (length(x_vals) < 2) {
    stop("Need at least 2 points to compute area")
  }

  if (length(x_vals) != length(y_vals)) {
    stop("x and y must have the same length")
  }

  if (any(is.na(x_vals)) || any(is.na(y_vals))) {
    stop("x and y cannot contain NA values")
  }

  # Compute area using trapezoidal rule
  # Area = sum of (width * average_height) for each trapezoid
  widths <- diff(x_vals)
  avg_heights <- (y_vals[-1] + y_vals[-length(y_vals)]) / 2
  area <- sum(widths * avg_heights)

  # Calculate deviation from 1.0
  deviation <- abs(area - 1.0)

  # Check if within tolerance
  is_valid <- deviation <= tolerance

  # Return results
  result <- list(
    area = area,
    valid = is_valid,
    deviation = deviation,
    tolerance = tolerance
  )

  class(result) <- "density_validation"

  return(result)
}

#' Print method for density validation
#'
#' @param x An object of class \code{"density_validation"}
#' @param ... Additional arguments (currently unused)
#'
#' @export
print.density_validation <- function(x, ...) {
  cat("Density Validation Results\n")
  cat("==========================\n")
  cat(sprintf("Total area under curve: %.6f\n", x$area))
  cat(sprintf("Deviation from 1.0: %.6f\n", x$deviation))
  cat(sprintf("Tolerance: %.6f\n", x$tolerance))
  cat(sprintf("Valid density: %s\n", ifelse(x$valid, "YES", "NO")))

  if (!x$valid) {
    cat(sprintf("\nWarning: Area deviates from 1.0 by more than tolerance (%.6f)\n",
                x$tolerance))
  }

  invisible(x)
}
