#' @export
#' @param data A data frame holding results
#' @param x The x-value for all plots as a character string.
#' @param y The y-value for all plots as a character string.
#' @param horiz Character of a variable for which variable to use for
#' grouping into horizontal rows.
#' @param vert Character of a variable for which variable to use for
#' grouping into vertical columns.
#' @param vert2 Character of a variable for how to further divide the
#' vertical columns
#' @param relative_error Boolean for whether the y-value is relative
#' @param axes_free Boolean. If TRUE axes between different rows and
#' columns (i.e. between levels of \code{horiz}, \code{vert} and
#' \code{vert2}) will be set automatically. If \code{FALSE} all x-
#' and y-axes will have a shared scale.
