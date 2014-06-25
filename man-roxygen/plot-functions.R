#' @section Description: The \code{ss3sim} plotting functions are simply
#' wrappers for \code{ggplot2} code, specific to the output from
#' \code{ss3sim} simulation scalar and timeseries (ts) objects. They are
#' designed to quickly explore simulation output, rather than
#' publication-level figures.
#' @section Output: These functions print the \code{ggplot} object, but
#' also return it invisibly for saving or printing again later.
#' @param data A data frame containing scalar or timeseries values from a
#' \code{ss3sim} simulation.
#' @param x A character string denoting which column to use as the $x$
#' variable. Column can be a factor (e.g. "F") or numeric
#' (e.g. "SpawnBio_om") depending on the plot function being used.
#' @param y A character string denoting which column to use as the $y$
#' variable. Must be a numeric column.
#' @param horiz,horiz2 A character string denoting which column to use as
#' the first (\code{horiz}) and second (\code{horiz2}) level of faceting in
#' the horizontal direction. E.g. "M" or "species". A value of NULL (default)
#' indicates no faceting.
#' @param vert,vert2 A character string denoting which column to use as
#' the first (\code{vert}) and second (\code{vert2}) level of faceting in
#' the vertical direction. E.g. "M" or "species". A value of NULL (default)
#' indicates no faceting.
#' @param relative.error Boolean for whether the $y$-axis should be
#' interpreted as relative error. If \code{TRUE}, \code{ylim} is set to
#' \code{c(-1,1)}, the $y$ axis label is changed automatically, and a
#' red line at $y=0$ is added.
#' @param axes.free Boolean for whether the $y$-axis scales should be free
#' in \code{facet_grid}.
#' @param color A character string denoting which column to use to map
#' color. Not valid for boxplot functions.
