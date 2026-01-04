#' Compute Frequency Polygons
#'
#' Calculates the values needed to construct a frequency polygon from numeric data, without producing a plot
#'
#'
#'
#' @param x A numeric vector of observations
#' @param bins The number of bins used to construct the histogram
#' @param density Logical; if TRUE returns density instead of counts.
#'
#' @return An object of class \code{"frequency_polygon"} containing:
#'   \itemize{
#'   \item mids: Centerpoints of each histogram bin
#'   \item y: Counts or densities at each midpoint
#'   \item binwidth: The width of the bins
#'}
#' @examples
#' library(MASS)
#' data(geyser)
#' fp <- compute_fp(geyser$waiting, bins = 10)
#' fp$mids
#'
#' @author Conor Foran - <\email{conor.foran.2023@mumail.ie}
#'
#'
#'
#' @export
compute_fp <- function(x, bins = 10, density = TRUE) {
  if (!is.numeric(x)) {
    stop("x must be a numeric vector")
  }

  if (length(x) < 2) {
    stop("x must contain at least two observations")
  }
  breaks <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = bins + 1)
  h <- hist(x, breaks = breaks, plot = FALSE)


  y <- if (density) h$density else h$counts

  result <- list(
    data = x,
    x = h$mids,
    y = y,
    binwidth = diff(h$breaks)[1]
  )
  class(result) <- c("frequency_polygon", "hist_estimate")
  result
}
