#' Plot results of a simulation as a scatterplot
#'
#' Generate a scatterplot using \code{\link[ggplot2]{ggplot}} to visualize
#' the relationship between two continuous variables.
#' 
#' @details
#' Points are placed on the figure using the width setting in
#' \code{\link[ggplot2]{position_jitter}} that defaults to 40% resolution of
#' the data, meaning that the jitter values will occupy 80% of the implied bins.
#' The previous information was found in the documentation for
#' \code{\link[ggplot2]{position_jitter}}.
#'
#' Values of \code{NA} are removed prior to plotting such that the typical
#' error message from \code{ggplot} is not printed to the screen.
#'
#' @template plot-functions
#' @template plot-functions-color
#' @export
#' @examples
#' # Plot scalar values
#' data("scalar_dat", package = "ss3sim")
#' re <- calculate_re(scalar_dat)
#' \dontrun{
#' plot_points(re, x = "E", y = "depletion_re", horiz = 'D',
#'   color = "max_grad", relative.error = TRUE)
#' }
#' rm(re)
#' # Merge scalar and time-series values to plot time series with color
#' data("ts_dat", package = "ss3sim")
#' re <- merge(by = "ID",
#'   calculate_re(ts_dat, add = FALSE),
#'   calculate_re(scalar_dat, add = FALSE)[, c("ID", "max_grad")])
#' \dontrun{
#' plot_points(re, x = "year", y = "SpawnBio_re",
#'   horiz = "scenario", color = "max_grad", relative.error = TRUE)
#' }
#' rm(re)
#'
plot_points <- function(data, x, y,
  horiz = NULL, horiz2 = NULL, vert = NULL, vert2 = NULL,
  color = NULL, relative.error = FALSE, axes.free = TRUE, print = TRUE) {

    g <- plot_ss3sim(data = data, x = x, y = y, color = color,
      relative.error = relative.error, axes.free = axes.free,
      horiz = horiz, horiz2 = horiz2, vert = vert, vert2 = vert2)
    g <- g + ggplot2::geom_jitter(size = 1,
      position = ggplot2::position_jitter(height = 0)) +
      ggplot2::scale_color_gradient(low = "black", high = "red")
    if (print) print(g)
    invisible(g)

}

plot_scalar_points <- function(data, x, y,
  horiz = NULL, horiz2 = NULL, vert = NULL, vert2 = NULL,
  color = NULL, relative.error = FALSE, axes.free = TRUE, print = TRUE) {
  .Deprecated("plot_points", package = "ss3sim")
  plot_points(data = data, x = x, y = y,
    horiz = horiz, horiz2 = horiz2,
    vert = vert, vert2 = vert2,
    color = color, relative.error = relative.error,
    axes.free = axes.free, print = print)
}
plot_ts_points <- function(data, y,
  horiz = NULL, horiz2 = NULL, vert = NULL, vert2 = NULL,
  color = NULL, relative.error = FALSE, axes.free = TRUE, print = TRUE) {
  .Deprecated("plot_points", package = "ss3sim")
  plot_points(data = data, x = "year", y = y,
    horiz = horiz, horiz2 = horiz2,
    vert = vert, vert2 = vert2,
    color = color, relative.error = relative.error,
    axes.free = axes.free, print = print)
}
