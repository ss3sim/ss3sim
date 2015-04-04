#' Plot relative error and indicate median absolute relative error
#'
#' @param x Cases to plot along the x axis
#' @param re A vector of relative errors that aligns with the \code{cases}
#'   vector.
#' @param ylim Y limits on the plot
#' @param mare_pos Y position to add median absolute relative error. Set to
#'   \code{NULL} to exclude this.
#' @param ypos Y axis ticks
#' @param xaxis Add an x axis?
#' @param yaxis Add a y axis?
#' @export
#'
#' @examples
#' set.seed(1)
#' x <- data.frame(case = c(rep(c(1, 2), each = 10)),
#'   re = c(rnorm(10, sd = 0.3), rnorm(10, 0.02, sd = 0.2)))
# plot_re_panel(x$case, x$re)
#' plot_re_panel(x$case, x$re, beans = TRUE, ylim = c(-1, 1), dots = TRUE, box = TRUE)
plot_re_panel <- function(x, re, ylim = c(-0.5, 0.5), mare_pos = max(ylim),
  ypos = c(-0.3, 0, 0.3, 0.6), xaxis = TRUE, yaxis = TRUE, col.axis = "grey50",
  cex.axis = 0.7, cex.mare = 0.7, lty.zero = 1, lwd.zero = 0.8,
  col.zero = "grey80", beans = FALSE, dots = FALSE, mare_col_scale = 7) {

  d_ <- plyr::ddply(data.frame(x, y = re), "x", plyr::summarize,
    median_ = median(y, na.rm = TRUE),
    l2      = quantile(y, 0.25, na.rm = TRUE),
    u2      = quantile(y, 0.75, na.rm = TRUE),
    mare    = 100 * round(median(abs(y), na.rm = TRUE), 2))

  d_ <- d_[order(d_$x), ]
  d_$xpos <- seq_len(length(unique(d_$x))) - 0
  xlim <- c(min(d_$xpos) - 0.4, max(d_$xpos) + 0.4)

  plot.default(d_$xpos, d_$median, ylim = ylim, ann = FALSE, axes = FALSE,
    xlim = xlim, type = "n")

  if (!beans) {
    abline(h = 0, lty = lty.zero, col = col.zero, lwd = lwd.zero)
    segments(d_$xpos, d_$l2, d_$xpos, d_$u2, lwd = 1, col = "grey55")
    points(d_$xpos, d_$median, pch = 21, bg = "grey30", col = "grey30", cex = 0.65)
  } else {
    bean_dat <- data.frame(cases, re)
    beanplot::beanplot(re ~ cases, data = bean_dat,
      add = FALSE, border = NA, axes = FALSE, col = rep("#00000050", 4),
      what = c(0, 1, 0, 0))
  }
  if (dots) {
    points(cases, re)
  }

  if (!is.null(mare_pos)) {
    mare_col <- grey(max(c(0, 1-(d_$mare/100*mare_col_scale - 0.1))))
    par(xpd = NA)
    mare_pos_rep <- rep(c(mare_pos, mare_pos + 0.0), 10)[1:length(unique(d_$x))]
    text(d_$xpos, mare_pos_rep, d_$mare, pos = 3, cex = cex.mare, col = mare_col)
    par(xpd = FALSE)
  }
  if (yaxis)
    axis(2, las = 1, at = ypos, label = ypos * 100, cex.axis = cex.axis, col.axis =
        col.axis, col = col.axis, lwd = 0.8)

  if (xaxis)
    axis(1, at = d_$xpos, labels = unique(x), las = 1, cex.axis = cex.axis,
      col.axis = col.axis, col = col.axis, lwd = 0.8)

}

#' Plot relative error
#'
#' @examples
#' library("dplyr")
#' d <- scalar_dat
#' d <- calculate_re(d)
#' d <- d %>% select(D, E, SSB_MSY_re, depletion_re, SSB_Unfished_re) %>%
#'   filter(!D %in% "D100", !E %in% c("E100", "E101"))
#' d <- d %>% reshape2::melt(value.name = "re")
#' plot_re(data = d, re = "re", x = "variable", row_dat = "D", column_dat = "E")
#' @export
plot_re <- function(data, re, x, row_dat, column_dat,
  mar = c(2.5,0.5,0.5,0.15), oma = c(3.5, 4.5, 3.5, 1), cex = 0.6, ...) {

  par(cex = cex, mar = mar, oma = oma)
  row_levels <- unique(data[, row_dat])
  col_levels <- unique(data[, column_dat])

  par(mfrow = c(length(row_levels), length(col_levels)))

  for(i in seq_along(row_levels)) {
    for(j in seq_along(col_levels)) {
      this_dat <- data[data[,row_dat] == row_levels[i] &
          data[,column_dat] == col_levels[j], ]
      plot_re_panel(re = this_dat[,re], x = this_dat[,x],
        yaxis = ifelse(j == 1, TRUE, FALSE),
        xaxis = ifelse(i == length(row_levels), TRUE, FALSE),
        ...)
    }
  }
}
