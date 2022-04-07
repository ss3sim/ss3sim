#' Plot time-series values as lines
#'
#' @template plot-functions
#' @template plot-functions-color
#' @export
#' @examples
#' data("scalar_dat", "ts_dat", package = "ss3sim")
#' # Merge in max_grad, a performance metric, to use for color
#' re <- merge(
#'   by = "ID",
#'   calculate_re(ts_dat, add = FALSE),
#'   calculate_re(scalar_dat, add = FALSE)[, c("ID", "max_grad")]
#' )
#' \dontrun{
#' plot_lines(re,
#'   y = "SpawnBio_re", horiz = "D", vert = "E",
#'   relative.error = TRUE, color = "max_grad"
#' )
#' }
#'
plot_lines <- function(data, x = "year", y,
                       horiz = NULL, horiz2 = NULL, vert = NULL, vert2 = NULL,
                       relative.error = FALSE, color = NULL, axes.free = TRUE, print = TRUE) {
  g <- plot_ss3sim(
    data = data, x = "year", y = y, color = color,
    relative.error = relative.error, axes.free = axes.free,
    horiz = horiz, horiz2 = horiz2, vert = vert, vert2 = vert2
  )
  g <- g + ggplot2::geom_line(ggplot2::aes(group = .data[["ID"]]),
    alpha = 0.5, lwd = 0.5
  ) +
    ggplot2::scale_color_gradient(low = "black", high = "red")
  if (print) print(g)
  return(invisible(g))
}

plot_ts_lines <- function(data, y,
                          horiz = NULL, horiz2 = NULL, vert = NULL, vert2 = NULL,
                          relative.error = FALSE, axes.free = TRUE, print = TRUE,
                          color = NULL) {
  .Deprecated("plot_lines", package = "ss3sim")
  plot_lines(
    data = data, x = "year", y = y,
    horiz = horiz, horiz2 = horiz2,
    vert = vert, vert2 = vert2,
    relative.error = relative.error,
    axes.free = axes.free, print = print,
    color = color
  )
}
