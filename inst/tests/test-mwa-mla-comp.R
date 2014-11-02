context("sample_mlacomp() and sample_mwacomp() are working")

temp_path <- file.path(tempdir(), "ss3sim-test")
dir.create(temp_path, showWarnings = FALSE)
wd <- getwd()
setwd(temp_path)

test_that("sample_mlacomp() works", {
  fdat <- system.file("extdata/models/cod-om/codOM.dat", package = "ss3sim")
  fctl <- system.file("extdata/models/cod-om/codOM.ctl", package = "ss3sim")
  change_bin(fdat, file_out = "codOM-temp.dat",
    type = c("mla"),
    fleet_dat = list("mla" = list(years = list(2000:2012), fleets = 1)),
      write_file = TRUE)
  set.seed(123)
  out <- sample_mlacomp("codOM-temp.dat", outfile = "ignore.dat", ctlfile = fctl,
    Nsamp = list(rep(50, 13)), years = list(2000:2012), write_file = FALSE,
    mean_outfile = NULL)
  expect_equal(names(out),
    c("Yr", "Seas", "Fleet", "Gender", "Part", "AgeErr", "Ignore",
    "a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10",
    "a11", "a12", "a13", "a14", "a15", "N_a1", "N_a2", "N_a3", "N_a4",
    "N_a5", "N_a6", "N_a7", "N_a8", "N_a9", "N_a10", "N_a11", "N_a12",
    "N_a13", "N_a14", "N_a15"))
  expect_equal(round(out$a1, 2),
    c(0.9, 0.99, 1, 0.96, 0.97, 1.09, 0.96, 0.99, 1.05, 0.99, 0.96,
      0.87, 1.12))
  expect_equal(out$N_a1,
    c(2, 4, 4, 3, 5, 3, 6, 3, 5, 4, 5, 1, 2))
  unlink("codOM-temp.dat")
})

test_that("sample_mwacomp() works", {
  change_bin(fdat, file_out = "codOM-temp.dat",
    type = c("mwa"),
    fleet_dat = list("mwa" = list(years = list(2000:2012), fleets = 1)),
    write_file = TRUE)
  set.seed(123)
  out <- sample_mwacomp("codOM-temp.dat", outfile = "ignore.dat", ctlfile = fctl,
    Nsamp = list(rep(50, 13)), years = list(2000:2012), write_file = FALSE,
    mean_outfile = NULL)
  expect_equal(names(out),
    c("Yr", "Seas", "Fleet", "Gender", "Part", "AgeErr", "Ignore",
      "a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10",
      "a11", "a12", "a13", "a14", "a15", "N_a1", "N_a2", "N_a3", "N_a4",
      "N_a5", "N_a6", "N_a7", "N_a8", "N_a9", "N_a10", "N_a11", "N_a12",
      "N_a13", "N_a14", "N_a15"))
  expect_equal(round(out$a1, 2),
    c(0.9, 0.99, 1, 0.96, 0.97, 1.09, 0.96, 0.99, 1.05, 0.99, 0.96,
      0.87, 1.12))
  expect_equal(out$N_a1,
    c(2, 4, 4, 3, 5, 3, 6, 3, 5, 4, 5, 1, 2))
  unlink("codOM-temp.dat")
})

setwd(wd)
