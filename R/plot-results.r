#' Plot scalar points
#'
#' Flexible function to make jittered scatter plots from a scalar data
#' frame. Use it to quickly explore results by varying dimensions and
#' $x$ and $y$ variables to explore patterns. Intended use is to
#' explore model convergence and other issues to help identify
#' outliers or model misspecification.
#'
#' @template plot_scalar
#' @param x The x-value for all plots as a character string.
#' @param color Character of a variable for how to color the points.
#' Useful variables are \code{max_grad} or other measures of
#' convergence.
#' @author Cole Monnahan
#' @import ggplot2
#' @export
#' @examples
#' # Explore the error in log R0 vs the maximum gradient. Outliers may
#' # be apparent
#' plot_scalar_points(final_results_scalar, x = "SR_LN_R0_om", y =
#' "SR_LN_R0_em", color = "max_grad", vert = "D")

plot_scalar_points <- function(data, x, y, horiz = "species", 
  vert = ".", vert2 = NULL, relative_error = FALSE, 
  axes_free = TRUE, color = "max_grad") {
  g <- ggplot(data = data) + theme_bw()
  if (relative_error) {
    g <- g + coord_cartesian(ylim = c(-1, 1)) + 
      ylab(paste("relative error for:", y))
    g <- g + geom_hline(yintercept = 0, col = "red")
  }
  if (is.null(vert2) | vert == ".") {
    form <- as.formula(paste(horiz, "~", vert))
  }
  else {
    form <- as.formula(paste(horiz, "~", vert, "+", vert2))
  }
  g <- g + geom_point(aes_string(x = x, y = y, color = color), 
    size = 2) + scale_color_gradient(low = "gray", high = "red") + 
    facet_grid(form, scales = ifelse(axes_free, "free", 
      "fixed"))
  print(g)
  return(invisible(g))
}


#' Plot scalar boxplot
#'
#' The same as \code{\link{plot_scalar_points}} except it produces
#' boxplots and therefore has no color parameter. Intended use is to
#' compare performance metrics, particularly relative error.
#'
#' @template plot_scalar
#' @param x The x-value for all plots as a character string.
#' @author Cole Monnahan
#' @import ggplot2
#' @export
#' @examples
#' plot_scalar_boxplot(final_results_scalar, x = "SR_LN_R0_om", y =
#' "SR_LN_R0_em", vert = "D")

plot_scalar_boxplot <- function(data, x, y, horiz = "species", 
  vert = ".", vert2 = NULL, relative_error = FALSE, axes_free = TRUE) {
  g <- ggplot(data = data) + theme_bw()
  if (relative_error) {
    g <- g + coord_cartesian(ylim = c(-1, 1)) + 
      ylab(paste("relative error for:", y))
    g <- g + geom_hline(yintercept = 0, col = "red")
  }
  if (is.null(vert2) | vert == ".") {
    form <- as.formula(paste(horiz, "~", vert))
  }
  else {
    form <- as.formula(paste(horiz, "~", vert, "+", vert2))
  }
  g <- g + geom_boxplot(aes_string(x = x, y = y), size = 0.3, 
    outlier.size = 1.5, outlier.colour = rgb(0, 0, 0, 0.5)) + 
    facet_grid(form, scales = ifelse(axes_free, "free", 
      "fixed"))
  print(g)
  return(invisible(g))
}


#' Plot timeseries boxplot
#'
#' The same as \code{\link{plot_scalar_points}} except it produces
#' boxplots across years for a timeseries data frame. Intended use is
#' to compare performance metrics, particularly relative error.
#'
#' @template plot_scalar
#' @author Cole Monnahan
#' @import ggplot2
#' @export
#' @examples
#' \dontrun{
#' # Not running this example because it can take a while to generate
#' plot_ts_boxplot(subset(final_results_ts, D == "D1"), 
#' y = "SpawnBio_em", vert = "D")
#' }

plot_ts_boxplot <- function(data, y, horiz = "species", 
  vert = ".", vert2 = NULL, relative_error = FALSE, axes_free = TRUE) {
  g <- ggplot(data = data, aes(x = year)) + xlab("Year") + 
    theme_bw()
  if (relative_error) {
    g <- g + coord_cartesian(ylim = c(-1, 1)) + 
      ylab(paste("relative error for:", y))
    g <- g + geom_hline(yintercept = 0, col = "red")
  }
  if (is.null(vert2) | vert == ".") {
    form <- as.formula(paste(horiz, "~", vert))
  }
  else {
    form <- as.formula(paste(horiz, "~", vert, "+", vert2))
  }
  g <- g + geom_boxplot(aes_string(y = y, group = "year"), 
    outlier.colour = rgb(0, 0, 0, 0.3), size = 0.5, outlier.size = 0.8) + 
    facet_grid(form, scales = ifelse(axes_free, "free", 
      "fixed"))
  print(g)
  return(invisible(g))
}


#' Plot timeseries points
#'
#' The same as \code{\link{plot_scalar_points}} except it produces
#' points across years for a timeseries data frame. Intended use is to
#' explore model convergence and diagnostics.
#'
#' @template plot_scalar
#' @param color Character of a variable for how to color the points.
#' Useful variables are \code{max_grad} or other measures of
#' convergence.
#' @author Cole Monnahan
#' @import ggplot2
#' @export
#' @examples
#' final_results <- merge(final_results_ts,
#' final_results_scalar[,c("scenario", "replicate", "max_grad")])
#' plot_ts_points(final_results, y = "SpawnBio_em", vert = "D", 
#' color = "max_grad")

plot_ts_points <- function(data, y, horiz = "species", 
  vert = ".", vert2 = NULL, relative_error = FALSE, 
  axes_free = TRUE, color = "max_grad") {
  g <- ggplot(data = data, aes(x = year)) + xlab("Year") + 
    theme_bw()
  if (relative_error) {
    g <- g + coord_cartesian(ylim = c(-1, 1)) + 
      ylab(paste("relative error for:", y))
    g <- g + geom_hline(yintercept = 0, col = "red")
  }
  if (is.null(vert2) | vert == ".") {
    form <- as.formula(paste(horiz, "~", vert))
  }
  else {
    form <- as.formula(paste(horiz, "~", vert, "+", vert2))
  }
  g <- g + geom_jitter(aes_string(y = y, group = "year", 
    colour = color), alpha = 0.5, size = 1) + facet_grid(form, 
    scales = ifelse(axes_free, "free", "fixed")) + 
    scale_color_gradient(low = "gray", high = "red")
  print(g)
  return(invisible(g))
}
