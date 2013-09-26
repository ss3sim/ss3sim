load("../../vignettes/ts_dat.rda")
load("../../vignettes/scalar_dat.rda")
ts_dat <- subset(ts_dat, D %in% c("D1", "D2"))
scalar_dat <- subset(scalar_dat, D %in% c("D1", "D2"))

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

library(plyr)
quant_dat <- ddply(ts_dat_sto, c("D", "E", "year"), summarize,
  q05 = quantile(SpawnBio, probs = 0.05),
  q25 = quantile(SpawnBio, probs = 0.25),
  q50 = quantile(SpawnBio, probs = 0.50),
  q75 = quantile(SpawnBio, probs = 0.75),
  q95 = quantile(SpawnBio, probs = 0.95)
)

cols <- RColorBrewer::brewer.pal(8, "Blues")
levels(quant_dat$D) <- c("atop(sigma[survey]==0.1, Decreased~survey~effort)", "atop(sigma[survey]==0.4, Increased~survey~effort)")
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
  ylab("Relative error") + xlab("Year")

ggsave(p, file = "spawnb-re-ts.pdf", width = 6, height = 4.25)
