#' Compute ASH
#'
#' Estimates density by averaging multiple shifted histograms.
#'
#' @importFrom graphics hist
#'
#' @param x Numeric vector of data.
#' @param h Numeric. The bin width for the underlying histograms.
#' @param m Integer. The number of shifts. Defaults to 3.
#'
#' @return A list of class "ash_density" containing:
#'   \item{x}{The x coordinates (midpoints of the fine bins)}
#'   \item{y}{The estimated density values}
#'
#' @examples
#' library(MASS)
#' data(geyser)
#' res <- compute_ash(geyser$wait, h = 9, m = 3)
#' head(res$y)
#' @export
compute_ash <- function(x, h, m=3) {
  delta <- h/m
  x_min <- min(x, na.rm = TRUE)
  x_max <- max(x, na.rm = TRUE)
  start_val <- floor(x_min) - h
  end_val <- ceiling(x_max)+2*h
  hist_list <- list()
  all_breaks <- c()
  for (i in 0:(m - 1)) {
    shift <- i * delta
    b <- seq(from = start_val + shift, to = end_val + shift, by=h)
    all_breaks <- c(all_breaks, b)
    hist_list[[i + 1]] <- hist(x, breaks = b, plot = FALSE)
  }
  sorted_breaks <- sort(unique(all_breaks))
  ax <- sorted_breaks[-length(sorted_breaks)]+delta/2
  histheight <- function(h_obj, val) {
    b <- h_obj$breaks
    if (val < min(b) || val >= max(b)) {
      return(0)
    } else {
      i <- sum(b <= val)
      if (i < 1 || i > length(h_obj$density)) {
        return(0)
      }
      return(h_obj$density[i])
    }
  }
  densities_matrix <- matrix(0, nrow = length(ax), ncol = m)
  for (j in 1:m) {
    densities_matrix[, j] <- sapply(ax, histheight, h_obj = hist_list[[j]])
  }
  ay <- rowMeans(densities_matrix)
  result <- list(
    x = ax,
    y = ay,
    parameters = list(h=h, m=m)
  )
  class(result) <- "ash_density"
  return(result)
}
