#' Plot results of a simulation as boxplots
#'
#' Generate boxplots using [ggplot2::ggplot()] to visualize
#' outliers and central tendencies.
#'
#' @details
#' Median, hinges, and whiskers as well as outliers are displayed to
#' summarize the data. The lower and upper hinges are the first and third
#' quantiles (i.e., 25th and 75th percentiles). The upper and lower
#' whiskers are 1.5*inner-quartile range, i.e., the distance between the first
#' and third quartiles. Outliers are those points that lie beyond the whiskers.
#' These explanations are detailed in [ggplot2::geom_boxplot()].
#'
#' Values of `NA` are removed prior to plotting such that the typical
#' error message from [ggplot2::ggplot()] is not printed to the screen.
#'
#' @template plot-functions
#' @param fill A character string that represents a single color that will
#' be used to fill the boxplots. The default value of `NA` leads to
#' unfilled boxplots.
#' @export
#' @examples
#' # Plot scalar values
#' data("scalar_dat", package = "ss3sim")
#' re <- calculate_re(scalar_dat)
#' \dontrun{
#' plot_boxplot(re,
#'   x = "E", y = "depletion_re", horiz = "D",
#'   relative.error = TRUE
#' )
#' }
#' rm(re)
#' # Merge scalar and time-series values to plot time series with color
#' # Be patient, the time-series boxplots take some time.
#' data("ts_dat", package = "ss3sim")
#' ts_dat[, "model_run"] <- factor(ts_dat[, "model_run"],
#'   levels = c("om", "em")
#' )
#' \dontrun{
#' plot_boxplot(ts_dat,
#'   x = "year", y = "SpawnBio",
#'   horiz = "scenario", vert = "model_run"
#' )
#' }
#'
plot_boxplot <- function(data, x, y,
                         horiz = NULL, horiz2 = NULL, vert = NULL, vert2 = NULL,
                         relative.error = FALSE, axes.free = TRUE, print = TRUE,
                         fill = NA) {
  g <- plot_ss3sim(
    data = data, x = x, y = y,
    relative.error = relative.error, axes.free = axes.free, print = print,
    horiz = horiz, horiz2 = horiz2, vert = vert, vert2 = vert2
  )
  g <- g + ggplot2::geom_boxplot(
    fill = fill,
    outlier.colour = grDevices::rgb(0, 0, 0, 0.5), lwd = 0.3,
    outlier.size = 0.8, fatten = ifelse(x == "year", 3, 1)
  )
  if (print) print(g)
  invisible(g)
}
