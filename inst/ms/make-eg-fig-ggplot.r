ts_dat1 <- read.csv("~/Pending/ss3sim-ms/final_results_ts-run1.csv")
ts_dat2 <- read.csv("~/Pending/ss3sim-ms/final_results_ts.csv")
ts_dat <- rbind(ts_dat1, ts_dat2)
#ts_dat <- subset(ts_dat, replicate %in% 1:50)
scalar_dat1 <- read.csv("~/Pending/ss3sim-ms/final_results_scalar-run1.csv")
scalar_dat2 <- read.csv("~/Pending/ss3sim-ms/final_results_scalar.csv")
scalar_dat1$StartTime <- NULL
scalar_dat2$StartTime <- NULL
scalar_dat1$EndTime <- NULL
scalar_dat2$EndTime <- NULL
scalar_dat <- rbind(scalar_dat1, scalar_dat2)
#scalar_dat <- subset(scalar_dat, replicate %in% 1:50)
#save(ts_dat, file = "ts_dat.rda")
#save(scalar_dat, file = "scalar_dat.rda")



#load("../../vignettes/ts_dat.rda")
#load("../../vignettes/scalar_dat.rda")
ts_dat <- subset(ts_dat, D %in% c("D1", "D2"))
scalar_dat <- subset(scalar_dat, D %in% c("D1", "D2"))

f_merge <- subset(ts_dat, year == max(ts_dat$year))[,c("scenario", "replicate", "F_om", "F_em")]
scalar_dat <- join(scalar_dat, f_merge)

##

scalar_dat <- transform(scalar_dat,
  steep = (SR_BH_steep_om - SR_BH_steep_em)/SR_BH_steep_om,
  logR0 = (SR_LN_R0_om - SR_LN_R0_em)/SR_LN_R0_om,
  depletion = (depletion_om - depletion_em)/depletion_om,
  SSB_MSY = (SSB_MSY_em - SSB_MSY_om)/SSB_MSY_om,
  SR_sigmaR = (SR_sigmaR_em - SR_sigmaR_om)/SR_sigmaR_om,
  NatM =
    (NatM_p_1_Fem_GP_1_em - NatM_p_1_Fem_GP_1_om)/
     NatM_p_1_Fem_GP_1_om,
  Fmort = (F_em - F_om) / F_om)

# add a tiny bit of noise for violin plotting:
N <- length(scalar_dat[scalar_dat$E == "E0", "NatM"])
#scalar_dat[scalar_dat$E == "E0", "NatM"] <- rnorm(N, 0, 0.0001)
scalar_dat[scalar_dat$E == "E0", "NatM"] <- NA


ts_dat <- transform(ts_dat,
  SpawnBio = (SpawnBio_em - SpawnBio_om)/SpawnBio_om,
  Recruit_0 = (Recruit_0_em - Recruit_0_om)/Recruit_0_om)

#ts_dat <- merge(ts_dat, scalar_dat[,c("scenario", "replicate",
    #"max_grad")])

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
    strip.text = element_text(colour = "grey30")
    #plot.margin = grid::unit(c(.1,.1,.1,.1), "cm"),
    #panel.margin = grid::unit(0, "cm")
    ) +
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
scalar_dat_long <- reshape2::melt(scalar_dat_sto[,c("scenario", "D",
    "E", "replicate", "max_grad", "depletion", "NatM", "SSB_MSY", "Fmort")],
 id.vars = c("scenario", "D", "E", "replicate", "max_grad"))
scalar_dat_long <- plyr::rename(scalar_dat_long,
  c("value" = "relative_error"))

scalar_dat_long <- droplevels(scalar_dat_long)
levels(scalar_dat_long$D) <- c("High", "Low")
levels(quant_dat$E) <- c("Fixed~italic('M')[historical]", "Estimated~italic('M')")
#levels(scalar_dat_long$variable) <- c("Depletion", "italic('M')")
levels(scalar_dat_long$variable) <- c("Depletion", "M", "SSB[MSY]", "F")



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

#levels(scalar_dat_long$scenario) <- c("High survey effort, Fixed M", "High survey effort, Estimate M", "Low survey effort, Fixed M", "Low survey effort, Estimated M" )
#levels(scalar_dat_long$scenario) <- c("s_surv = 0.1, Fixed M", "s_surv = 0.1, Estimate M", "s_surv = 0.4, Fixed M", "s_surv = 0.4, Estimated M" )

#### put the same variable in the same panel:
p2 <- ggplot(scalar_dat_long, aes(x = scenario, y = relative_error)) +
  #geom_boxplot(aes(fill = variable), colour = "grey50", outlier.colour = "grey50", notch = TRUE, outlier.size = 1.5) +
  geom_violin(fill = cols[4], colour = cols[7]) +
  geom_jitter(position = position_jitter(height = 0, width = 0.05), alpha = 0.4, colour = cols[7], pch = 20, size = 1) +
  facet_grid(~variable, labeller = label_parsed, scales = "fixed")+
  geom_hline(yintercept = 0, linetype = 3)  +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = NA, linetype = 0),
    axis.text = element_text(colour = "grey50"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.title = element_text(colour = "grey30"),
    axis.ticks = element_line(colour = "grey50"),
    strip.text = element_text(colour = "grey30"),
    legend.position = "none"
    ) +

  ylab("Relative error") + xlab("")
  print(p)
  #

#ggsave(p, file = "scalar-boxplots.pdf", width = 6, height = 4.25)

library(gridExtra)
p3 <- grid.arrange(p1, p2, ncol = 1, heights = c(1, 1))
print(p3)
