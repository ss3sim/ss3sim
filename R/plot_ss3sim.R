#' Base plot for `ss3sim` data
#'
#' Use [ggplot2::ggplot] to plot data from `ss3sim`
#' simulation.
#'
#' @details The `ss3sim` plotting functions are simply wrappers for
#'   [ggplot2::ggplot()] code, specific to the output from
#'   [get_results_all()]. They are designed to quickly explore simulation
#'   output, rather than produce publication-level figures. The functions use
#'   arguments passed as characters that refer to columns of `data`. Scalar
#'   plots require a value for `x`; while, for time-series plots,
#'   `x = "year"` will be necessary.
#'
#'   Note that there are some subtle differences between the functions.
#'   Boxplots cannot have a color mapped to them like points or lines, and
#'   thus `color` is not a valid argument. The time-series point and line plots
#'   are grouped internally by `"ID"`, which is a combination of scenario and
#'   iteration and will be automatically added to the data set if not already
#'   present.
#'
#' @section Output:
#'   These functions print the `ggplot` object, but also return it invisibly
#'   for saving or printing again later. For example, you could save the
#'   `ggplot` object and add a custom theme or change an axis label before
#'   printing it.
#' @param data A valid data frame containing scalar or timeseries values from an
#'   `ss3sim` simulation. That data are generated from [get_results_all()].
#' @param x A character string denoting which column to use as the x variable.
#'   For time-series data, setting `x = "year"` leads to a time-series plot.
#' @param y A character string denoting which column to use as the y variable.
#'   Must be a numeric column.
#' @param color A character string denoting which column to use to map color.
#'   Not valid for boxplot functions. Useful for looking at estimation-model
#'   performance criteria against other dimensions of the estimation or
#'   operating model.
#' @param relative.error Boolean for whether the y-axis scale should be
#'   interpreted as relative error. If `TRUE`, `ylim` is set to `c(-1, 1)`,
#'   the y-axis label is changed automatically, and a black, dashed line at
#'   `y = 0` is added. The argument can also accept a color entry if you wish
#'   the line to be something other than black. For example, `"red"` will add a
#'   red dashed line at zero as well as fix the y-axis limits.
#' @param axes.free Boolean for whether the y-axis scales should be free in
#'   [ggplot2::facet_grid()].
#' @param print A logical for whether the plot is printed or not.
#' @param horiz,horiz2 A character string denoting which column to use as the
#'   first (`horiz`) and second (`horiz2`) level of faceting in the horizontal
#'   direction. A value of `NULL` indicates no faceting in the horizontal
#'   space.
#' @param vert,vert2 A character string denoting which column to use as the
#'   first (`vert`) and second (`vert2`) level of faceting in the vertical
#'   direction. A value of `NULL` indicates no faceting in the vertical space.
#' @importFrom rlang .data
#'
plot_ss3sim <- function(data, x, y, color = NULL,
                        relative.error = FALSE, axes.free = TRUE, print = TRUE,
                        horiz = NULL, horiz2 = NULL, vert = NULL, vert2 = NULL) {
  verify_plot_arguments(
    data = data, x = x, y = y, print = print,
    horiz = horiz, horiz2 = horiz2, vert = vert, vert2 = vert2,
    color = color, relative.error = relative.error, axes.free = axes.free
  )

  data <- data[!is.na(data[, y]), ]
  if (!"ID" %in% colnames(data)) {
    data[, "ID"] <- paste(data[, "scenario"], data[, "iteration"], sep = "-")
  }
  if (!is.null(color)) {
    stopifnot(is.numeric(data[, color]))
  }
  g <- ggplot2::ggplot(
    data = data,
    ggplot2::aes(x = !!x, y = !!y)
  ) +
    ggplot2::xlab(gsub("year", "Year", x))
  if (x == "year") {
    g <- g + ggplot2::aes(group = year)
  }
  if (relative.error != FALSE) {
    if (relative.error == TRUE) relative.error <- "black"
    g <- g + ggplot2::coord_cartesian(ylim = c(-1, 1)) +
      ggplot2::ylab(paste("Relative error for:", gsub("_re", "", y))) +
      ggplot2::geom_hline(yintercept = 0, col = relative.error, lty = 2)
  } else {
    g <- g + ggplot2::ylab(y)
  }
  form <- facet_form(horiz, horiz2, vert, vert2)
  if (!is.null(form)) {
    g <- g + ggplot2::facet_grid(form,
      scales = ifelse(axes.free, "free", "fixed")
    )
  }
  if (!is.null(color)) {
    g <- g + ggplot2::aes(color = !!ggplot2::sym(color)) +
      ggplot2::labs(colour = color)
  }
  invisible(g)
}
