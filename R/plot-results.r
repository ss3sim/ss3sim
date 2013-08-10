#' Calculate run time
#'
#' Internal function used by \code{get_results_scenario} to calculate the
#' runtime (in minutes) from a Report.sso file.
#'
#' @param start_time Vector of characters as read in from the r4ss report file
#' @param end_time Vector of characters as read in from the r4ss report file
#' @author Cole Monnahan

calculate_runtime <- function(start_time, end_time) {
  start <- data.frame(do.call(rbind, strsplit(x = as.character(start_time), 
    split = " ", fixed = T))[, -(1:2)])
  end <- data.frame(do.call(rbind, strsplit(x = as.character(end_time), 
    split = " ", fixed = T))[, -(1:2)])
  names(start) <- names(end) <- c("month", "day", "time", 
    "year")
  start.date <- lubridate::ymd_hms(with(start, paste(year, 
    month, day, time, sep = "-")))
  end.date <- lubridate::ymd_hms(with(end, paste(year, 
    month, day, time, sep = "-")))
  run.mins <- as.vector(end.date - start.date)/60
  return(run.mins)
}


#' Plot scalar points
#'
#' Flexible function to make jittered scatter plots from a scalar data
#' frame. Use it to quickly explore results by varying dimensions and
#' $x$ and $y$ variables to explore patterns. Intended use is to
#' explore model convergence and other issues to help identify
#' outliers or model misspecification.
#'
#' @template plot_scalar
#' @param color Character of a variable for how to color the points.
#' Useful variables are \code{max_grad} or other measures of
#' convergence.
#' @author Cole Monnahan

#' @examples
#' Explore the error in log R0 vs the maximum gradient. Outliers may
#' be apparent
#' library(ggplot2)
#' scalars$log_max_grad <- log(scalars$max_grad)
#' plot_scalar_points(data=scalars, x = "SR_LN_R0_om", y = "SR_LN_R0_em",
#'   color = "log_max_grad", vert = "F")

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
  g <- g + geom_jitter(aes_string(x = x, y = y, color = color), 
    size = 1) + scale_color_gradient(low = "gray", high = "red") + 
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
#' @author Cole Monnahan

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
  g <- g + geom_boxplot(aes_string(x = x, y = y), size = 0.2, 
    outlier.size = 1, outlier.colour = rgb(0, 0, 0, 0.5)) + 
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
