#' Create the ss3sim logo
#' 
#' Generate and save, if \code{outfile} is provided,
#' the ss3sim logo using the built-in data.
#' 
#' @template outfile
#' @examples
#' create_logo()
#' dev.off()
#' @return A \code{.png} file or a graphics device with the 
#' logo used for the \code{ss3sim} project.
#' @author Kelli Faye Johnson
#' 
create_logo <- function(outfile = NULL) {
  if (!is.null(outfile)) {
    png(outfile,
      width = 4, height = 4, units = "in", res = 600)
    on.exit(dev.off())
  }
  data(ts_dat, package = "ss3sim")
  # cols <- RColorBrewer::brewer.pal(8, "Blues")
  cols <- c("#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1",
    "#6BAED6", "#4292C6", "#2171B5", "#084594")

  ts_dat <- ts_dat[ts_dat$E == "E0" & ts_dat$D == "D0", ]
  ts_dat$SpawnBio <- (ts_dat$SpawnBio_em - ts_dat$SpawnBio_om) / 
    ts_dat$SpawnBio_om
  ts_dat$Recruit_0 <- (ts_dat$Recruit_0_em - ts_dat$Recruit_0_om) / 
    ts_dat$Recruit_0_om
  quant_dat <- data.frame(do.call("rbind", 
    tapply(ts_dat$SpawnBio, ts_dat$year, quantile, 
    probs = c(0.05,0.25, 0.50, 0.75, 0.95))))
  colnames(quant_dat) <- c("q05", "q25", "q50", "q75", "q95")
  quant_dat$year <- row.names(quant_dat)

  plot(1, 1, 
    xlim = ceiling(quantile(ts_dat$year, probs = c(0.03, 0.32))), 
    ylim = c(-0.15, 0.2),
    type = "n", axes = FALSE, ann = FALSE, xaxs = "i")
  polygon(c(quant_dat$year, rev(quant_dat$year)), c(quant_dat$q05, rev(quant_dat$q95)), 
    col = cols[4], border = NA)
  polygon(c(quant_dat$year, rev(quant_dat$year)), c(quant_dat$q25, rev(quant_dat$q75)), 
    col = cols[6], border = NA)
  lines(quant_dat$year, quant_dat$q50, col = cols[8], lwd = 3)
}
