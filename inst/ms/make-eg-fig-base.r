# first run make-eg-fig.r
# there may be unimportant errors currently

# this needs to be cleaned

cols <- RColorBrewer::brewer.pal(8, "Blues")
cols2 <- RColorBrewer::brewer.pal(8, "Reds")
cols3 <- RColorBrewer::brewer.pal(8, "Greens")
cols4 <- RColorBrewer::brewer.pal(8, "Greys")
cols5 <- RColorBrewer::brewer.pal(8, "Oranges")
label_col <- "grey40"
label_cex <- 0.75
axis_col <- "grey55"

pdf("fig2.pdf", width = 5, height = 4.5)
layout(rbind(
    c(1, 1, 2, 2),
    c(1, 1, 2, 2),
    c(3, 3, 4, 4),
    c(3, 3, 4, 4),
    c(5, 6, 7, 8),
    c(5, 6, 7, 8),
    c(5, 6, 7, 8)
    ))
par(mar = c(.2,.2,0,0))
par(oma = c(3,3.5,2,3))
par(cex = 0.7)
par(tck = -0.03)
par(mgp = c(2, 0.40, 0))
ii <<- 0
d_ply(quant_dat, c("D", "E"), transform, {
  ii <<- ii + 1
  plot(1, 1, xlim = range(year), ylim = c(-0.25, 0.5), type = "n", axes = FALSE, ann = FALSE, xaxs = "i", yaxs = "i")
  polygon(c(year, rev(year)), c(q05, rev(q95)), col = cols[4], border = NA)
  polygon(c(year, rev(year)), c(q25, rev(q75)), col = cols[6], border = NA)
  lines(year, q50, col = cols[8], lwd = 1.4)
  if(i %in% c(1, 3)) axis(2, las = 1, at = seq(-0.2, 0.5, 0.2), col = axis_col, col.axis = axis_col)
  if(i %in% c(3, 4)) axis(1, col = axis_col, col.axis = axis_col, padj = -0.15)
  box(col = axis_col)
  abline(h = 0, col = "#FFFFFF95", lty = 0, lwd = 1)
  #mtext("SSB", adj = 0.05, line = -1.3, col = "grey50", cex = 0.75)
  #mtext(LETTERS[ii], adj = 0.05, line = -1.5, col = axis_col, cex = 0.7)
  if(ii == 1) {
    text(1945, 0.15, "Median", pos = 4, col = "grey50")
    text(1959, 0.25, "50% range", pos = 4, col = "grey50")
    text(1978, 0.35, "90% range", pos = 4, col = "grey50")
    arrows(1955, 0.10, 1955, 0, length = 0.06, col = "grey25")
    arrows(1975, 0.20, 1975, 0.03, length = 0.06, col = "grey25")
    arrows(1995, 0.30, 1995, 0.06, length = 0.06, col = "grey25")
  }
})

library(beanplot)

panel_labs <- c(expression(Depletion), expression(italic(M)), expression(SSB[MSY]), expression(F))

par(mar = c(.2,.2,1.7,0))
d_ply(scalar_dat_long, "variable", function(x) {
  ii <<- ii + 1
  plot(1, 1, xlim = c(.6, 4.4), ylim = c(-0.6, 0.6), type = "n",
    axes = FALSE, ann = FALSE, yaxs = "i")
  #cols_ii <- list(cols3, cols4, cols, cols2)[[ii-4]]
  cols_ii <- cols4
  beanplot(relative_error ~ scenario, data = x, add = TRUE, border = NA, axes = FALSE, col = cols_ii[c(5, 3, 4, 8)], what = c(0, 1, 0, 0), log = FALSE)
  points(jitter(as.numeric(x$scenario), amount = 0.07), x$relative_error, col = paste0(cols_ii[3], "80"), pch = 20, cex = 0.2)
  if(ii %in% 5) axis(2, las = 1, at = seq(-0.6, 0.6, 0.3), col = axis_col, col.axis = axis_col)
  axis(1, col = axis_col, col.axis = label_col, at = 1:4, labels = substr(levels(x$scenario), 1, 5), las = 3)
  #axis(1, col = axis_col, col.axis = axis_col, at = 1:4, labels = c("D1E0", "D ), 1, 1, 1), las = 3)
  box(col = axis_col)
  abline(h = 0, col = "#00000040", lty = 1, lwd = 1.5)
  #mtext(LETTERS[ii], adj = 0.05, line = -1.5, col = "grey40", cex = 0.8)
  text(0.5, 0.48, panel_labs[ii-4], col = label_col, pos = 4, cex = 1.05)
})

mtext("Relative error in SSB", side = 2, outer = TRUE, line = 2.2, cex = label_cex, col = label_col, adj = 0.8)
mtext("Relative error", side = 2, outer = TRUE, line = 2.2, cex = label_cex, col = label_col, adj = 0.1)
mtext(expression(Fixed~italic(M)[historical]~(E0)), side = 3, outer = TRUE, cex = label_cex, adj = 0.18, line = 0.2, col = label_col)
mtext(expression(Estimated~italic(M)~(E1)), side = 3, outer = TRUE, cex = label_cex, adj = 0.85, line = 0.2, col = label_col)

mtext(expression(sigma[survey]==0.1~(D1)), side = 4, outer = TRUE, cex = label_cex, adj = 0.99, line = 0.6, col = label_col)
mtext(expression(Higher~survey~effort), side = 4, outer = TRUE, cex = label_cex, adj = 1.025, line = 1.8, col = label_col)

mtext(expression(sigma[survey]==0.4~(D2)), side = 4, outer = TRUE, cex = label_cex, adj = 0.58, line = 0.6, col = label_col)
mtext(expression(Lower~survey~effort), side = 4, outer = TRUE, cex = label_cex, adj = 0.58, line = 1.8, col = label_col)

dev.off()
