load("../../vignettes/ts_dat.rda")
load("../../vignettes/scalar_dat.rda")
ts_dat <- subset(ts_dat, D %in% c("D1", "D2"))
scalar_dat <- subset(scalar_dat, D %in% c("D1", "D2"))

scalar_dat <- transform(scalar_dat,
  steep = (SR_BH_steep_om - SR_BH_steep_em)/SR_BH_steep_om,
  logR0 = (SR_LN_R0_om - SR_LN_R0_em)/SR_LN_R0_om,
  depletion = (depletion_om - depletion_em)/depletion_om,
  SSB_MSY = (SSB_MSY_em - SSB_MSY_om)/SSB_MSY_om,
  SR_sigmaR = (SR_sigmaR_em - SR_sigmaR_om)/SR_sigmaR_om,
  NatM =
    (NatM_p_1_Fem_GP_1_em - NatM_p_1_Fem_GP_1_om)/
     NatM_p_1_Fem_GP_1_om)

ts_dat <- transform(ts_dat,
  SpawnBio = (SpawnBio_em - SpawnBio_om)/SpawnBio_om,
  Recruit_0 = (Recruit_0_em - Recruit_0_om)/Recruit_0_om)

ts_dat <- merge(ts_dat, scalar_dat[,c("scenario", "replicate",
    "max_grad")])

scalar_dat_det <- subset(scalar_dat, E %in% c("E100", "E101"))
scalar_dat_sto <- subset(scalar_dat, E %in% c("E0", "E1"))
ts_dat_det <- subset(ts_dat, E %in% c("E100", "E101"))
ts_dat_sto <- subset(ts_dat, E %in% c("E0", "E1"))
ts_dat_sto <- droplevels(ts_dat_sto)
scalar_dat_sto <- droplevels(scalar_dat_sto)

library(plyr)
quant_dat <- ddply(ts_dat_sto, c("D", "E", "year"), summarize,
  q05 = quantile(SpawnBio, probs = 0.05),
  q25 = quantile(SpawnBio, probs = 0.25),
  q50 = quantile(SpawnBio, probs = 0.50),
  q75 = quantile(SpawnBio, probs = 0.75),
  q95 = quantile(SpawnBio, probs = 0.95)
)

cols <- RColorBrewer::brewer.pal(8, "Blues")
levels(quant_dat$D) <- c("atop(sigma[survey]==0.1,
  Increased~survey~effort)", "atop(sigma[survey]==0.4,
  Decreased~survey~effort)")
levels(quant_dat$E) <- c("Fixed~italic('M')[historical]",
  "Estimated~italic('M')")

library(ggplot2)
p <- ggplot(quant_dat) +
  geom_ribbon(aes(year, ymax = q05, ymin = q95), fill = cols[4]) +
  geom_ribbon(aes(year, ymax = q25, ymin = q75), fill = cols[6]) +
  geom_line(aes(year, q50), colour = cols[8]) +
  facet_grid(D~E, labeller = label_parsed) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = NA, linetype = 0),
    axis.text = element_text(colour = "grey50"),
    axis.title = element_text(colour = "grey30"),
    axis.ticks = element_line(colour = "grey50"),
    strip.text = element_text(colour = "grey30")) +
  geom_hline(yintercept = 0, linetype = 3, colour = "#FFFFFF95") +
  ylab("Relative error in SSB") + xlab("Year")

ggsave(p, file = "spawnb-re-ts.pdf", width = 6, height = 4.25)

levels(scalar_dat_sto$D) <- c("atop(sigma[survey]==0.1,
  Increased~survey~effort)", "atop(sigma[survey]==0.4,
  Decreased~survey~effort)")
levels(scalar_dat_sto$E) <- c("Fixed~italic('M')[historical]",
  "Estimated~italic('M')")

p <- ggplot(scalar_dat_sto, aes(x = 0, y = depletion),
  labeller = label_parsed) +
  geom_violin(fill = cols[4], colour = cols[6]) +
  geom_jitter(position = position_jitter(height = 0, width = 0.05), alpha = 0.6, colour = cols[8], pch = 21, size = 1.5) +
  facet_grid(D~E, labeller = label_parsed) +
  geom_hline(yintercept = 0, linetype = 3)  +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = NA, linetype = 0),
    axis.text = element_text(colour = "grey50"),
    axis.title = element_text(colour = "grey30"),
    axis.ticks = element_line(colour = "grey50"),
    strip.text = element_text(colour = "grey30"),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank()
    ) +
  ylab("Relative error in depletion") + xlab("")


ggsave(p, file = "depletion-re-scalar.pdf", width = 6, height = 4.25)

scalar_dat_sto <- subset(scalar_dat, E %in% c("E0", "E1"))
scalar_dat_long <- reshape2::melt(scalar_dat_sto[,c("scenario", "D", "E",
  "replicate", "max_grad", "depletion", "NatM", "SSB_MSY")], id.vars = c("scenario", "D", "E",
  "replicate", "max_grad"))
scalar_dat_long <- plyr::rename(scalar_dat_long,
  c("value" = "relative_error"))

scalar_dat_long <- droplevels(scalar_dat_long)
levels(scalar_dat_long$D) <- c("High", "Low")
levels(quant_dat$E) <- c("Fixed~italic('M')[historical]", "Estimated~italic('M')")
#levels(scalar_dat_long$variable) <- c("Depletion", "italic('M')")
levels(scalar_dat_long$variable) <- c("Depletion", "M", "SSB[MSY]")



# p <- ggplot(scalar_dat_long, aes(x = D, y = relative_error),
#   labeller = label_parsed) +
#   geom_violin(fill = cols[4], colour = cols[6]) +
#   facet_grid(variable~E, labeller = label_parsed, scales = "free_y") +
#   geom_jitter(position = position_jitter(height = 0, width = 0.05), alpha = 0.6, colour = cols[8], pch = 21, size = 1.5) +
#   geom_hline(yintercept = 0, linetype = 3)  +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     strip.background = element_rect(fill = NA, linetype = 0),
#     axis.text = element_text(colour = "grey50"),
#     axis.title = element_text(colour = "grey30"),
#     axis.ticks = element_line(colour = "grey50"),
#     strip.text = element_text(colour = "grey30")
#     ) +
#   ylab("Relative error") + xlab("Survey effort")

#ggsave(p, file = "depletion-re-scalar.pdf", width = 4, height = 4.25)

levels(scalar_dat_long$D) <- c("atop(sigma[survey]==0.1,
  Increased~survey~effort)", "atop(sigma[survey]==0.4,
  Decreased~survey~effort)")
levels(scalar_dat_long$E) <- c("Fixed~italic('M')[historical]", "Estimated~italic('M')")

p <- ggplot(scalar_dat_long, aes(x = variable, y = relative_error)) +
  geom_boxplot(aes(fill = variable), colour = "grey50", outlier.colour = "grey50", notch = TRUE, outlier.size = 1.5) +
  facet_grid(D~E, labeller = label_parsed)+
  geom_hline(yintercept = 0, linetype = 3)  +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = NA, linetype = 0),
    axis.text = element_text(colour = "grey50"),
    axis.title = element_text(colour = "grey30"),
    axis.ticks = element_line(colour = "grey50"),
    strip.text = element_text(colour = "grey30"),
    legend.position = "none"
    ) +

  ylab("Relative error") + xlab("")
  #

ggsave(p, file = "scalar-boxplots.pdf", width = 6, height = 4.25)
