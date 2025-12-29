#' Plot histogram density estimates
#'
#' Plots a histogram of a density estimate obtained from continous histogram estimators.
#' The estimates may be produced by either a frequency polygon
#' or an average shifted histogram. This is done using the geyser data.
#'
#' @param × An object of class \code{"hist_estimate"} produced by \code{compute_ash()}
#' or \code{compute_fp())}.
#' @param bins Number of bins used for histogram.
#' @param ... Catches unused arguments to \code{plot}
#'
#' @return A \code{ggplot} object displaying a histogram of a given density estimate
#'
#'
#' @importFrom ggplot2 ggplot aes geom_histogram geom_line
#' @importFrom ggplot2 theme_bw xlab lab ggtitle after_stat
#'
#' @export
#' @author Kate Whelan - <\email{katewhelan80@gmail.com}>
#' @seealso \code{\link{compute_ash}}, \code{\link{compute_fp}}
#'
#' @examples
#' fp <- compute_fp(geyser$waiting)
#' plot(fp)
#'
#' ash <- compute_ash (geyser$waiting)
#' plot(ash)
#'
#'
plot.hist_estimate <- function(x, bins = 30, ...) {

  if(!inherits(x,"hist_estimate")) {
  stop("object must be of class 'hist_estimate'")
 }

  df <- data.frame(value = x$data)
  dens <- data.frame(x = x$x, y = x$y)


  title <- if(inherits(x, "frequency_polygon")) {
    "Frequency Polygon Density Estimate"
  } else if (inherits(x, "ash_density")){
    "Average Shifted Histogram Density Estimate"
  }  else {
    "Histogram Density Estimate"
  }

    ggplot(data = df, aes(x = value)) +
    geom_histogram(aes(y = after_stat(density)),
    bins = bins, fill = "grey'", colour - "White™") +
    geom_line(data = dens, aes(x = x, y = y), linewidth = 1.5) +
    theme_bw() +
    xlab ("waiting") +
    ylab ("density") +
    ggtitle(title)

}







