# Create Figure 2 for the paper
# SCA

library(plyr)

load("../../vignettes/scalar_dat.rda")
load("../../vignettes/ts_dat.rda")

# Bring in F to scalars:
f_merge <- subset(ts_dat, year == max(ts_dat$year))[ ,c("scenario",
  "replicate", "F_om", "F_em")]
scalar_dat <- join(scalar_dat, f_merge, by = c("scenario",
    "replicate"))

scen <- c()
scen[1] <- expression(sigma[H]~italic(M)[F])
scen[2] <- expression(sigma[H]~italic(M)[E])
scen[3] <- expression(sigma[L]~italic(M)[F])
scen[4] <- expression(sigma[L]~italic(M)[E])

scalar_dat <- transform(scalar_dat,
  steep     = (SR_BH_steep_om - SR_BH_steep_em)/SR_BH_steep_om,
  logR0     = (SR_LN_R0_om - SR_LN_R0_em)/SR_LN_R0_om,
  depletion = (depletion_om - depletion_em)/depletion_om,
  SSB_MSY   = (SSB_MSY_em - SSB_MSY_om)/SSB_MSY_om,
  SR_sigmaR = (SR_sigmaR_em - SR_sigmaR_om)/SR_sigmaR_om,
  NatM      = (NatM_p_1_Fem_GP_1_em - NatM_p_1_Fem_GP_1_om)/
               NatM_p_1_Fem_GP_1_om,
  Fmort     = (F_em - F_om) / F_om)

# add a tiny bit of noise for violin plotting:
# N <- length(scalar_dat[scalar_dat$E == "E0", "NatM"])
# scalar_dat[scalar_dat$E == "E0", "NatM"] <- rnorm(N, 0, 0.0001)
scalar_dat[scalar_dat$E == "E0", "NatM"] <- NA

ts_dat <- transform(ts_dat,
  SpawnBio  = (SpawnBio_em - SpawnBio_om)/SpawnBio_om,
  Recruit_0 = (Recruit_0_em - Recruit_0_om)/Recruit_0_om)

scalar_dat_det <- subset(scalar_dat, E %in% c("E100", "E101"))
scalar_dat_sto <- subset(scalar_dat, E %in% c("E0", "E1"))
ts_dat_det <- subset(ts_dat, E %in% c("E100", "E101"))
ts_dat_sto <- subset(ts_dat, E %in% c("E0", "E1"))
ts_dat_sto <- droplevels(ts_dat_sto)
scalar_dat_sto <- droplevels(scalar_dat_sto)

scalar_dat_sto <- subset(scalar_dat, E %in% c("E0", "E1"))
scalar_dat_long <- reshape2::melt(scalar_dat_sto[,c("scenario", "D",
  "E", "replicate", "max_grad", "depletion", "NatM", "SSB_MSY",
  "Fmort")], id.vars = c("scenario", "D", "E", "replicate",
  "max_grad"))
scalar_dat_long <- plyr::rename(scalar_dat_long,
  c("value" = "relative_error"))

scalar_dat_long <- droplevels(scalar_dat_long)

quant_dat <- ddply(ts_dat_sto, c("D", "E", "year"), summarize,
  q05 = quantile(SpawnBio, probs = 0.05),
  q25 = quantile(SpawnBio, probs = 0.25),
  q50 = quantile(SpawnBio, probs = 0.50),
  q75 = quantile(SpawnBio, probs = 0.75),
  q95 = quantile(SpawnBio, probs = 0.95)
)

cols <- RColorBrewer::brewer.pal(8, "Blues")
cols4 <- RColorBrewer::brewer.pal(8, "Greys")

label_col <- "grey40"
label_cex <- 0.75
axis_col <- "grey55"

pdf("fig2.pdf", width = 5, height = 4.5)
layout(rbind(c(1, 1, 2, 2),
             c(1, 1, 2, 2),
             c(3, 3, 4, 4),
             c(3, 3, 4, 4),
             c(5, 6, 7, 8),
             c(5, 6, 7, 8),
             c(5, 6, 7, 8)))
par(mar = c(.2,.2,0,0))
par(oma = c(3,3.5,2,3))
par(cex = 0.7)
par(tck = -0.03)
par(mgp = c(2, 0.40, 0))
ii <<- 0 # for panel counting

d_ply(quant_dat, c("D", "E"), transform, {
  ii <<- ii + 1
  plot(1, 1, xlim = range(year), ylim = c(-0.25, 0.5), type = "n",
    axes = FALSE, ann = FALSE, xaxs = "i", yaxs = "i")
  polygon(c(year, rev(year)), c(q05, rev(q95)), col = cols[4], border = NA)
  polygon(c(year, rev(year)), c(q25, rev(q75)), col = cols[6], border = NA)
  lines(year, q50, col = cols[8], lwd = 1.4)
  if(i %in% c(1, 3)) axis(2, las = 1, at = seq(-0.2, 0.5, 0.2), col =
    axis_col, col.axis = axis_col)
  if(i %in% c(3, 4)) axis(1, col = axis_col, col.axis = axis_col, padj = -0.15)
  box(col = axis_col)
  abline(h = 0, col = "#FFFFFF95", lty = 0, lwd = 1)
  mtext(paste(unique(D), unique(E), sep = "-"), side = 3, line =
    -1.15, cex = 0.7, col = label_col, adj = 0.043)
  #mtext(scen[ii], side = 3, line = -1.15, cex = 0.7, col = label_col, adj = 0.043)
  #mtext("SSB", adj = 0.05, line = -1.3, col = "grey50", cex = 0.75)
  #mtext(LETTERS[ii], adj = 0.05, line = -1.5, col = axis_col, cex = 0.7)
  if(ii == 1) {
    text(1945, 0.15, "Median", pos = 4, col = cols[8])
    text(1959, 0.25, "50% range", pos = 4, col = cols[6])
    text(1978, 0.35, "90% range", pos = 4, col = cols[4])
    arrows(1955, 0.10, 1955, 0, length = 0.06, col = cols[8])
    arrows(1975, 0.20, 1975, 0.03, length = 0.06, col = cols[6])
    arrows(1995, 0.30, 1995, 0.06, length = 0.06, col = cols[4])
  }
})

panel_labs <- c(expression(Depletion), expression(italic(M)),
  expression(SSB[MSY]), expression(F))

par(mar = c(.2,.2,1.7,0))
d_ply(scalar_dat_long, "variable", function(x) {
  ii <<- ii + 1
  plot(1, 1, xlim = c(.6, 4.4), ylim = c(-0.6, 0.6), type = "n",
    axes = FALSE, ann = FALSE, yaxs = "i")
  cols_ii <- cols4
  beanplot::beanplot(relative_error ~ scenario, data = x, add = TRUE,
    border = NA, axes = FALSE, col = cols_ii[c(5, 3, 4, 8)], what =
    c(0, 1, 0, 0), log = FALSE)
  points(jitter(as.numeric(x$scenario), amount = 0.07),
    x$relative_error, col = paste0(cols_ii[3], "80"), pch = 20, cex =
    0.2)
  if(ii %in% 5) axis(2, las = 1, at = seq(-0.6, 0.6, 0.3), col =
    axis_col, col.axis = axis_col)
  axis(1, col = axis_col, col.axis = label_col, at = 1:4, labels =
    substr(levels(x$scenario), 1, 5), las = 3)
    #scen, las = 3)
  box(col = axis_col)
  abline(h = 0, col = "#00000040", lty = 1, lwd = 1.5)
  #mtext(LETTERS[ii], adj = 0.05, line = -1.5, col = "grey40", cex = 0.8)
  text(0.5, 0.48, panel_labs[ii-4], col = label_col, pos = 4, cex = 1.05)
})

# Label the axes:
mtext("Relative error in SSB", side = 2, outer = TRUE, line = 2.2, cex
  = label_cex, col = label_col, adj = 0.8)
mtext("Relative error", side = 2, outer = TRUE, line = 2.2, cex =
  label_cex, col = label_col, adj = 0.1)
mtext(expression(Fixed~italic(M)~(E0)), side = 3, outer =
  TRUE, cex = label_cex, adj = 0.18, line = 0.2, col = label_col)
mtext(expression(Estimated~italic(M)~(E1)), side = 3, outer = TRUE,
  cex = label_cex, adj = 0.85, line = 0.2, col = label_col)

mtext(expression(sigma[survey]==0.1~(D1)), side = 4, outer = TRUE, cex
  = label_cex, adj = 0.99, line = 0.6, col = label_col)
mtext(expression(Higher~survey~effort), side = 4, outer = TRUE, cex =
  label_cex, adj = 1.025, line = 1.8, col = label_col)

mtext(expression(sigma[survey]==0.4~(D2)), side = 4, outer = TRUE, cex
  = label_cex, adj = 0.58, line = 0.6, col = label_col)
mtext(expression(Lower~survey~effort), side = 4, outer = TRUE, cex =
  label_cex, adj = 0.58, line = 1.8, col = label_col)

dev.off()
