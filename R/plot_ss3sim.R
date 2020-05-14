#' Base plot for \code{ss3sim} data
#'
#' Use \code{\link[ggplot2]{ggplot}} to plot data from \code{ss3sim}
#' simulation.
#' @template plot-functions
#' @template plot-functions-color
#'
plot_ss3sim <- function(data, x, y, color = NULL,
  relative.error = FALSE, axes.free = TRUE, print = TRUE,
  horiz = NULL, horiz2 = NULL, vert = NULL, vert2 = NULL) {


  verify_plot_arguments(data = data, x = x, y = y, print = print,
    horiz = horiz, horiz2 = horiz2, vert = vert, vert2 = vert2,
    color = color, relative.error = relative.error, axes.free = axes.free)

  data <- data[!is.na(data[, y]), ]
  if (!"ID" %in% colnames(data)) {
    data[, "ID"] <- paste(data[, "scenario"], data[, "iteration"], sep = "-")
  }
  if (!is.null(color)) {
    stopifnot(is.numeric(data[, color]))
  }
  g <- ggplot2::ggplot(data = data,
    ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
    ggplot2::xlab(gsub("year", "Year", x))
  if (x == "year") {
    g <- g + ggplot2::aes(group = .data[["year"]])
  }
  if (relative.error != FALSE) {
    if (relative.error == TRUE) relative.error <- "black"
    g <- g + ggplot2::coord_cartesian(ylim = c(-1, 1)) +
      ggplot2::ylab(paste("Relative error for:", gsub("_re", "", y))) +
      ggplot2::geom_hline(yintercept = 0, col = relative.error, lty = 2)
  }
  form <- facet_form(horiz, horiz2, vert, vert2)
  if (!is.null(form)) {
    g <- g + ggplot2::facet_grid(form,
      scales = ifelse(axes.free, "free", "fixed"))
  }
  if (!is.null(color)) {
    g <- g + ggplot2::aes(color = .data[[color]])
  }
  invisible(g)

}
