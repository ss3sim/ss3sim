#' @details The \pkg{ss3sim} plotting functions are simply
#' wrappers for \pkg{ggplot2} code, specific to the output from
#' \pkg{ss3sim} \code{get_results_all()} objects. They are
#' designed to quickly explore simulation output, rather than produce
#' publication-level figures. The functions use arguments passed as
#' characters that refer to columns of \code{data}.
#' Scalar plots requires a value for \code{x}; while,
#' for time-series plots, \code{x = "year"} will be necessary.
#'
#' Note that there are some subtle differences between the
#' functions.
#' Boxplots cannot have a color mapped to them like points or lines,
#' and thus, \code{color} is not a
#' valid argument. The time-series point and line plots are grouped internally by
#' 'ID', which is a combination of scenario and iteration and will be
#' automatically added to the data set if not already present.
#' @section Output: These functions print the \code{ggplot} object, but
#' also return it invisibly for saving or printing again later.
#' For example, you could save the \code{ggplot} object and add a custom
#' theme or change an axis label before printing it.
#' @param data A valid data frame containing `scalar` or `timeseries` values
#' from a \pkg{ss3sim} simulation. That data are generated from
#' \code{\link{get_results_all}}.
#' @param x A character string denoting which column to use as the x variable.
#' For time-series data, setting \code{x = "year"} leads to a time-series plot.
#' @param y A character string denoting which column to use as the y
#' variable. Must be a numeric column.
#' @param horiz,horiz2 A character string denoting which column to use as
#' the first (\code{horiz}) and second (\code{horiz2}) level of faceting in
#' the horizontal direction. E.g., "M" or "species". A value of NULL (default)
#' indicates no faceting in the horizontal space.
#' @param vert,vert2 A character string denoting which column to use as
#' the first (\code{vert}) and second (\code{vert2}) level of faceting in
#' the vertical direction. E.g., "M" or "species". A value of NULL (default)
#' indicates no faceting in the vertical space.
#' @param relative.error Boolean for whether the y-axis scale should be
#' interpreted as relative error. If \code{TRUE}, \code{ylim} is set to
#' \code{c(-1, 1)}, the y-axis label is changed automatically, and a
#' black, dashed line at \code{y=0} is added. The argument can also accept a
#' color entry if you wish the line to be something other than black. E.g.,
#' \code{"red"} will add a red dashed line at zero as well as fix the y-axis
#' limits.
#' @param axes.free Boolean for whether the y-axis scales should be free
#' in \code{facet_grid}.
#' @param print A logical for whether the plot is printed or not.
#' @importFrom rlang .data
#' @author Cole C. Monnahan
