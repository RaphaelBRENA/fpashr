#' Plot histogram density estimates
#'
#' Plots a histogram of a density estimate obtained from continous histogram estimators.
#' The estimates may be produced by either a frequency polygon
#' or an average shifted histogram. This is done using the geyser data.
#'
#' @param x An object of class \code{"hist_estimate"} produced by \code{compute_ash()}
#' or \code{compute_fp())}.
#' @param fill The colour of the histogram bars, default is grey.
#' @param line The colour of the line, default is black.
#' @param ... Catches unused arguments to \code{plot}
#'
#' @return A \code{ggplot} object displaying a histogram of a given density estimate
#'
#'
#' @importFrom ggplot2 ggplot aes geom_histogram geom_line
#' @importFrom ggplot2 theme_bw xlab ylab ggtitle after_stat
#'
#' @export
#' @exportS3Method plot hist_estimate

#' @author Kate Whelan - <\email{katewhelan80@gmail.com}>
#' @seealso \code{\link{compute_ash}}, \code{\link{compute_fp}}
#'
#' @examples
#' library(MASS)
#' data(geyser)

#' fp <- compute_fp(geyser$waiting, bins = 10)
#' plot(fp)
#'
#' ash <- compute_ash(geyser$waiting, h = 9, m = 3)
#' plot(ash)
#'
#'
#'
plot.hist_estimate <- function(x, fill = "grey", line = "black",...) {

  if(!inherits(x,"hist_estimate")) {
    stop("object must be of class 'hist_estimate'")
  }

  binwidth <- if (inherits(x, "frequency_polygon")) {
    x$binwidth
  } else if (inherits(x, "ash_density")) {
    x$x[2] - x$x[1]
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

  p <- ggplot(data = df, aes(x = value)) +
    geom_histogram(aes(y = after_stat(density)),
                   binwidth = binwidth, fill = fill, colour = "white") +
    geom_line(data = dens, aes(x = x, y = y), color = line,linewidth = 1.5) +
    theme_bw() +
    ggplot2::xlab("waiting") +
    ggplot2::ylab("density") +
    ggtitle(title)

  return(p)

}



#ggplot2 aes uses value, density and y as column names, not variables
utils::globalVariables(c("value", "density", "y"))



