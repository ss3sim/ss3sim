context("weight-at-age data")

temp_path <- file.path(tempdir(), "ss3sim-test")
dir.create(temp_path, showWarnings = FALSE)
wd <- getwd()
setwd(temp_path)
on.exit(setwd(wd), add = TRUE)
on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)

d <- system.file("extdata", package = "ss3sim")
om <- file.path(d, "models", "cod-om")
em <- file.path(d, "models", "cod-em")
case_folder <- file.path(d, "eg-cases")
dat <- r4ss::SS_readdat(
  file = dir(om, pattern = "\\.dat", full.names = TRUE),
  verbose = FALSE)

dat[["use_MeanSize_at_Age_obs"]] <- 1
set.seed(4)
temp <- t(sapply(rnorm(length(26:100), mean = 10), 
 seq, to = 100, length.out = dat$Nages))
colnames(temp) <- paste0("a", 1:dat$Nages)
dat[["MeanSize_at_Age_obs"]] <- data.frame(
 "Yr" = 26:100, "Seas" = 1, "FltSvy" = 1, "Gender" = 0, "Part" = 0,
 "AgeErr" = 0, "Ignore" = 10, temp)

test_that("Check standard errors in sample_wtatage", {
  expect_error(sample_wtatage(outfile = NULL, dat_list = dat,
    ctl_file_in = dir(d, full.names = TRUE, pattern = "\\.ctl"),
    years = list(50:100), fill_fnc = fill_across,
    fleets = 1, cv_wtatage = NULL))
  expect_error(sample_wtatage(outfile = NULL, dat_list = dat,
    ctl_file_in = dir(d, full.names = TRUE, pattern = "\\.ctl"),
    years = list(50:100), fill_fnc = fill_across,
    fleets = 7, cv_wtatage = 0.05))
  expect_error(sample_wtatage(outfile = NULL, dat_list = dat,
    ctl_file_in = dir(d, full.names = TRUE, pattern = "\\.ctl"),
    years = list(1:10), fill_fnc = fill_across,
    fleets = 1, cv_wtatage = 0.05))
})

test_that("A single sampled fleet copied over correctly", {
  wt <- sample_wtatage(outfile = NULL, dat_list = dat,
    ctl_file_in = dir(om, full.names = TRUE, pattern = "\\.ctl"),
    years = list(50:100), fill_fnc = fill_across,
   fleets = 1, cv_wtatage = 0.05)
  expect_equal(wt[wt$Fleet == 2, "a1"], wt[wt$Fleet == 1, "a1"])
  expect_equal(wt[wt$Fleet == 2, "a1"], wt[wt$Fleet == -1, "a1"])
  expect_equal(wt[wt$Fleet == 1, "a0"], wt[wt$Fleet == 1, "a1"])
  expect_equivalent(
    wt[wt$Fleet == 1 & wt$Yr == 49, paste0("a", 1:10)],
    wt[wt$Fleet == 1 & wt$Yr == 50, paste0("a", 1:10)])
  expect_equivalent(
    wt[wt$Fleet == 1 & wt$Yr == 1, paste0("a", 1:10)],
    wt[wt$Fleet == 1 & wt$Yr == 50, paste0("a", 1:10)])
  expect_true(all(wt[, "a1"] <= wt[, "a2"]))
})

test_that("Population is mean of both fleets", {
  dat$MeanSize_at_Age_obs <- do.call("rbind", mapply(`[<-`,
    replicate(2, dat$MeanSize_at_Age_obs, simplify = FALSE),
    "FltSvy", value = 1:2, SIMPLIFY = FALSE))
  wt <- sample_wtatage(outfile = NULL, dat_list = dat,
    ctl_file_in = dir(om, full.names = TRUE, pattern = "\\.ctl"),
    years = list(50:100), fill_fnc = fill_across,
   fleets = 1:2, cv_wtatage = 0.05)
  expect_equal(wt[wt$Fleet == 1, "a0"], wt[wt$Fleet == 1, "a1"])
  expect_equal(
    apply(rbind(wt[wt$Fleet == 2, "a1"], wt[wt$Fleet == 1, "a1"]), 2, mean),
    wt[wt$Fleet == -1, "a1"])
  expect_true(all(wt[, "a1"] <= wt[, "a2"]))
})

test_that("Mean weight-at-age data are logical", {
  parlines <- r4ss::SS_parlines(dir(om, pattern = "ctl", full.names = TRUE))
  dat$agecomp[dat$agecomp$Yr == 100 & dat$agecomp$FltSvy == 1, paste0("a", 1:25)] <- 1000
  set.seed(4)
  wt <- sample_wtatage(outfile = NULL, dat_list = dat,
    ctl_file_in = dir(om, full.names = TRUE, pattern = "\\.ctl"),
    years = list(1:100), fill_fnc = fill_across,
   fleets = 1, cv_wtatage = 0.0005)
  expect_equivalent(
    ss3sim:::split_comp(wt[wt$Yr == 99 & wt$Fleet == 1, ])$raw[, -1],
    sapply(mapply(rnorm,
      n = ss3sim:::split_comp(dat$agecomp[dat$agecomp$Yr == 99 & dat$agecomp$FltSvy == 1, ])$raw,
      mean = ss3sim:::split_comp(dat$MeanSize_at_Age_obs[dat$MeanSize_at_Age_obs$Yr == 99 & dat$MeanSize_at_Age_obs$FltSvy == 1, ])$raw,
      sd = ss3sim:::split_comp(dat$MeanSize_at_Age_obs[dat$MeanSize_at_Age_obs$Yr == 99 & dat$MeanSize_at_Age_obs$FltSvy == 1, ])$raw * 0.005,
      SIMPLIFY = FALSE),
    function(x) mean(parlines[parlines$Label == "Wtlen_1_Fem", "INIT"] * x^parlines[parlines$Label == "Wtlen_2_Fem", "INIT"]), simplify = TRUE),
    tolerance = 0.04)
})

test_that("ss3sim runs with weight-at-age data", {
  skip_on_cran()
  if(Sys.info()[["user"]] != "Kelli.Johnson") skip("Not testing weight-at-age run")
  comp1 <- list(fleets = c(1, 2), Nsamp = list(100, 100),
    years = list(26:100, 26:100),
    lengthbin_vector = NULL, cpar = c(1, 1))
  ss3sim_base(iterations = 1,
    scenarios = "2cod",
    f_params = list(years = 1:100, fisheries = 1, fvals = c(rep(0, 25), rep(0.114, 75))),
    index_params = list(fleets = 2, years = list(26:100), sds_obs = list(0.01)),
    lcomp_params = comp1,
    agecomp_params = comp1,
    wtatage_params = list(fleets = c(1, 2), years = list(26:100, 26:100),
      cv_wtatage = 0.005),
    estim_params = list(par_name = c("NatM_p_1_Fem_GP_1"), 
      par_int = c(NA), par_phase = c(-1), forecast_num = 0),
    om_dir = om,
    em_dir = em)
  replist <- r4ss::SS_output("2cod/1/em", verbose = FALSE, printstats = FALSE, covar = FALSE)
  omdata <- r4ss::SS_readwtatage("2cod/1/om/wtatage.ss_new")
  expect_true(replist$wtatage_switch)
  expect_equivalent(replist$Empirical_wt_at_age, 1)
  expect_equivalent(
    unlist(apply(replist$mean_body_wt[replist$mean_body_wt$Yr %in% 26:100, as.character(0:25)], 2, mean)),
    unlist(omdata[omdata$Yr %in% 26 & omdata$Fleet == 1, as.character(0:25)]),
    tolerance = 1)
  unlink("2cod", recursive = TRUE)
})
