context("sample_mlacomp() is working")

temp_path <- file.path(tempdir(), "ss3sim-test")
dir.create(temp_path, showWarnings = FALSE)
wd <- getwd()
setwd(temp_path)

test_that("sample_mlacomp() works", {
  dat_list <- r4ss::SS_readdat(system.file("extdata/models/cod-om/codOM.dat", package = "ss3sim"), verbose = FALSE)
  fctl <- system.file("extdata/models/cod-om/codOM.ctl", package = "ss3sim")
  newdat <- change_data(dat_list, outfile = "codOM-temp.dat",
    types = c("age", "mla"), fleets = 1, years = 2000:2012, write_file = FALSE)
  set.seed(123)
  newdat <- change_fltname(newdat)
  out <- sample_mlacomp(newdat, outfile = "ignore.dat", ctl_file_in = fctl,
    Nsamp = list(rep(50, 13)), years = list(2000:2012), write_file = FALSE,
    mean_outfile = NULL)
  # Make the expected names based on the original codOM datalist value
  expected_names <-  c("Yr", "Seas", "FltSvy", "Gender", "Part", "AgeErr",
                       "Nsamp")
  expected_names <- c(expected_names, paste0("a",1:dat_list$Nages),
                      paste0("N", 1:dat_list$Nages))
  expect_equal(names(out$MeanSize_at_Age_obs),
    expected_names)
  ## TODO: test below commented out because it needs to be rewritten for Nages
  ## ages instead of 15. Need an intuitive way to get these values without using
  ## function.
  # expect_equal(round(out$MeanSize_at_Age_obs$a1, 2),
  #   c(0.9, 0.99, 1, 0.96, 0.97, 1.09, 0.96, 0.99, 1.05, 0.99, 0.96,
  #     0.87, 1.12))
})

test_that("mean of sample_mlacomp() is unbiased", {
  dat_list <- r4ss::SS_readdat(system.file("extdata/models/cod-om/codOM.dat",
    package = "ss3sim"), verbose = FALSE)
  fctl <- system.file("extdata/models/cod-om/codOM.ctl", package = "ss3sim")
  newdat <- change_data(dat_list, outfile = NULL, types = c("age", "mla"),
    fleets = 1, years = 2000, write_file = FALSE)
  newdat <- change_fltname(newdat)
  newdat$agecomp$Nsamp <- 800000
  ctlfile <- system.file("extdata/models/cod-om/codOM.ctl", package = "ss3sim")
  out <- sample_mlacomp(newdat, outfile = NULL, ctl_file_in = ctlfile,
    Nsamp = list(rep(800000, 1)), years = list(2000), write_file = FALSE,
    mean_outfile = NULL)
  result <- out$MeanSize_at_Age_obs[, -(1:9)]
  samplesize <- result[grepl("N", names(result))]
  proportion <- result[grepl("a", names(result))]
  expect_equal(mean(as.numeric(proportion[proportion > -1]), 3), 1,
               tolerance = .Machine$double.eps ^ 0.2)
})

setwd(wd)
