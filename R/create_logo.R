#' Create the ss3sim logo
#' 
#' Generate and save, if `outfile` is provided,
#' the ss3sim logo using the built-in data.
#' 
#' @template outfile
#' @examples
#' ss3sim:::create_logo()
#' grDevices::dev.off()
#' @return A `png` file or a graphics device with the 
#' logo used for the `ss3sim` project.
#' @author Kelli Faye Johnson
#' 
create_logo <- function(outfile = NULL) {
  if (!is.null(outfile)) {
    grDevices::png(outfile,
      width = 4, height = 4, units = "in", res = 600)
    on.exit(grDevices::dev.off())
  }
  utils::data(ts_dat, package = "ss3sim")
  # cols <- RColorBrewer::brewer.pal(8, "Blues")
  cols <- c("#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1",
    "#6BAED6", "#4292C6", "#2171B5", "#084594")
  
  ts_dat <- calculate_re(ts_dat, add = FALSE)
  ts_dat <- ts_dat[ts_dat$E == "E0" & ts_dat$D == "D1", ]
  quant_dat <- data.frame(do.call("rbind",
    tapply(ts_dat$SpawnBio_re, ts_dat$year, stats::quantile,
    probs = c(0.05,0.25, 0.50, 0.75, 0.95), na.rm = TRUE)))
  colnames(quant_dat) <- c("q05", "q25", "q50", "q75", "q95")
  quant_dat <- stats::na.omit(quant_dat)
  quant_dat$year <- row.names(quant_dat)

  graphics::plot(1, 1, 
    xlim = ceiling(stats::quantile(utils::type.convert(quant_dat$year, as.is = TRUE), probs = c(0.03, 0.50))),
    ylim = c(-0.3, 0.3),
    type = "n", axes = FALSE, ann = FALSE, xaxs = "i")
  graphics::polygon(c(quant_dat$year, rev(quant_dat$year)), c(quant_dat$q05, rev(quant_dat$q95)), 
    col = cols[4], border = NA)
  graphics::polygon(c(quant_dat$year, rev(quant_dat$year)), c(quant_dat$q25, rev(quant_dat$q75)), 
    col = cols[6], border = NA)
  graphics::lines(quant_dat$year, quant_dat$q50, col = cols[8], lwd = 3)
}
