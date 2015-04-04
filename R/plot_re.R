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
#' @param col.axis Axis colour
#' @param cex.axis Axis cex value
#' @param cex.mare Size of MARE numbers
#' @param lty.zero Line type for the zero line
#' @param lwd.zero Line width for the zero line
#' @param col.zero Colour of the zero line
#' @param beans Should beanplots be used instead of dots and lines?
#' @param dots Should dots for each iteration value be shown?
#' @param mare_col_scale A value that scales the darkness of the MARE values
#'   Bigger values will make for darker numbers.
#' @param bean_maxwidth Maximum width of beans
#' @param col.pts Colour of the points, if shown
#' @param cex.pts Size of points, if shown
#' @param jitter.pts A value to jitter the points by, if shown
#' @param labels.xaxis Values to label the x axis ticks with
#' @param col.beanplot Colour of the beans, if shown
#' @importFrom plyr summarize ddply
#' @importFrom beanplot beanplot
#' @export
#'
#' @examples
#' set.seed(1)
#' x <- data.frame(case = c(rep(c(1, 2), each = 10)),
#'   re = c(rnorm(10, sd = 0.3), rnorm(10, 0.02, sd = 0.2)))
#' plot_re_panel(x$case, x$re, beans = TRUE, ylim = c(-1, 1), dots = TRUE)
plot_re_panel <- function(x, re, ylim = c(-0.5, 0.5), mare_pos = max(ylim),
  ypos = c(-0.3, 0, 0.3, 0.6), xaxis = TRUE, yaxis = TRUE, col.axis = "grey50",
  cex.axis = 0.7, cex.mare = 0.7, lty.zero = 1, lwd.zero = 0.8,
  col.zero = "grey80", beans = FALSE, dots = FALSE, mare_col_scale = 7,
  bean_maxwidth = 0.6, col.pts = "#00000030", cex.pts = 0.4, jitter.pts = 0.1,
  labels.xaxis = unique(x),
  col.beanplot = "grey65") {

  y <- re
  d_ <- ddply(data.frame(x, y), "x", summarize,
    median_ = median(y, na.rm = TRUE),
    l2      = quantile(y, 0.25, na.rm = TRUE),
    u2      = quantile(y, 0.75, na.rm = TRUE),
    mare    = 100 * round(median(abs(y), na.rm = TRUE), 2))

  d_ <- d_[order(d_$x), ]
  d_$xpos <- seq_len(length(unique(d_$x))) - 0
  xlim <- c(min(d_$xpos) - 0.4, max(d_$xpos) + 0.4)

  plot.default(d_$xpos, d_$median, ylim = ylim, ann = FALSE, axes = FALSE,
    xlim = xlim, type = "n")

  abline(h = 0, lty = lty.zero, col = col.zero, lwd = lwd.zero)

  if (!beans) {
    segments(d_$xpos, d_$l2, d_$xpos, d_$u2, lwd = 1, col = "grey55")
    points(d_$xpos, d_$median, pch = 21, bg = "grey30", col = "grey30", cex = 0.65)
  } else {
    bean_dat <- data.frame(x, re)
    beanplot(re ~ x, data = bean_dat,
      add = TRUE, border = NA, axes = FALSE, col = col.beanplot,
      what = c(0, 1, 0, 0), maxwidth = bean_maxwidth)
  }
  if (dots) {
    points(jitter(as.numeric(x), jitter.pts), re, pch = 20, cex = cex.pts,
      col = col.pts)
  }

  if (!is.null(mare_pos)) {
    mare_col <- grey(max(c(0, 1-(d_$mare/100*mare_col_scale - 0.1))))
    par(xpd = NA)
    mare_pos_rep <- rep(c(mare_pos, mare_pos + 0.0), 10)[1:length(unique(d_$x))]
    text(d_$xpos, mare_pos_rep, d_$mare, pos = 3, cex = cex.mare, col = mare_col)
    par(xpd = FALSE)
  }
  if (yaxis)
    axis(2, las = 1, at = ypos, labels = ypos * 100, cex.axis = cex.axis,
      col.axis = col.axis, col = col.axis, lwd = 0.8)

  if (xaxis)
    axis(1, at = d_$xpos, labels = labels.xaxis, las = 1, cex.axis = cex.axis,
      col.axis = col.axis, col = col.axis, lwd = 0.8)

}

#' Plot relative error
#'
#' This function plots multiple relative error panels for a complete figure
#'
#' @param data A data frame
#' @param re A character value giving the column name from \code{data} of the
#'   relative error values
#' @param x A character value giving the column name from \code{data} of the x
#'   values
#' @param row_dat A character value giving the column name from \code{data} of
#'   the row identifiers
#' @param column_dat A character value giving the column name from \code{data}
#'   of the column identifiers
#' @param mar Margin values to pass to \code{par}
#' @param oma Outer margin values to pass to \code{par}
#' @param cex cex value to pass to \code{par}
#' @param xlab X label
#' @param ylab Y label
#' @param xlab_line Line on which to print the x label (passed to \code{mtext})
#' @param ylab_line on which to print the y label (passed to \code{mtext})
#' @param col_labs Colour for the y and x labels
#' @param adj.ylab Value to adjust the vertical position of the y label (passed
#'   to \code{mtext}, \code{0.5} is the center of the whole figure)
#' @param ... Anything else to pass to \code{\link{plot_re_panel}}
#'
#' @examples
#' library("dplyr")
#' d <- scalar_dat
#' d <- calculate_re(d)
#' d <- d %>% select(D, E, SSB_MSY_re, depletion_re, SSB_Unfished_re) %>%
#'   filter(!D %in% "D100", !E %in% c("E100", "E101"))
#' d <- d %>% reshape2::melt(value.name = "re")
#' plot_re(data = d, re = "re", x = "variable", row_dat = "D", column_dat = "E",
#'   xlab = "Management quantity", beans = TRUE, dots = TRUE, adj.ylab = 0.6,
#'   labels.xaxis = c("SSB_MSY", "Depletion", "SSB_Unfished"))
#' @export
plot_re <- function(data, re, x, row_dat, column_dat,
  mar = c(2.5,0.5,0.5,0.15), oma = c(3.5, 4.5, 3.5, 1), cex = 0.6,
  xlab = "", ylab = "Relative error", xlab_line = 1.5, ylab_line = 2,
  col_labs = "grey40", adj.ylab = 0.5, ...) {

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
  mtext(xlab, side = 1, line = xlab_line, outer = TRUE, col = col_labs)
  mtext(ylab, side = 2, line = ylab_line, outer = TRUE, col = col_labs,
    adj = adj.ylab)
}
