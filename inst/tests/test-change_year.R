context("Changing years in OM and EM")

wd.old <- getwd()
temp_path <- file.path(tempdir(), "year")
dir.create(temp_path, showWarnings = FALSE)
setwd(temp_path)
d <- system.file("extdata", package = "ss3sim")
file.copy(file.path(d, "Simple"), ".", recursive = TRUE)
setwd("Simple")

test_that("Control file references correct years", {
  # Manipulate files
  change_year(year_begin = 1, year_end = 26, burnin = 25,
              ctl_file_in = "simple.ctl", ctl_file_out = "change.ctl")
  test <- readLines("change.ctl")
  t1 <- grep("first year of main recr_devs", test)
  t2 <- grep("last year of main recr_devs", test)
  t3 <- grep("_last_early_yr", test)
  t4 <- grep("_first_yr_fullbias_adj", test)
  t5 <- grep("_last_yr_fullbias_adj", test)
  t6 <- grep("_first_recent_yr_nobias_adj", test)
    # Check F ballpark year
  range <- year_begin:year_end
  expect_true(as.numeric(strsplit(test[t1], "#")[[1]][1]) %in% range)
  expect_true(as.numeric(strsplit(test[t2], "#")[[1]][1]) %in% range)
  expect_true(as.numeric(strsplit(test[t3], "#")[[1]][1]) %in% range)
  expect_true(as.numeric(strsplit(test[t4], "#")[[1]][1]) %in% range)
  expect_true(as.numeric(strsplit(test[t5], "#")[[1]][1]) %in% range)
  expect_true(as.numeric(strsplit(test[t6], "#")[[1]][1]) %in% range)
})

om <- paste0(d, "/models/cod-om")
em <- paste0(d, "/models/cod-em")
file.copy(om, ".", recursive = TRUE)
verbose <- FALSE

test_that("Forecast file is readable.", {
  om.for <- SS_readforecast(file.path(om, "forecast.ss"), 0, 0, verbose = verbose)
  change_year(forecast_file_in = file.path(om, "forecast.ss"), 
              forecast_file_out = "new.ss")
  om.for.new <- SS_readforecast("new.ss", 0, 0)
  out <- evaluate_promise(SS_readforecast("new.ss", 0, 0), print = TRUE)$output
  expect_equal(grepl("Error", out), FALSE)
})

test_that("Correct lines are changed in starter file.", {
  change_year(starter_file_in = file.path(om, "starter.ss"), 
              starter_file_out = "new.ss")
  new <- readLines("new.ss")
  getnew <- new[grep("jitter", new) + 1:2]
  getnew <- gsub(" ", "", sapply(strsplit(getnew, "#"), "[[", 1))
  expect_equal(getnew, c("-1", "-2"))
})

test_that("Recruitment devs are of correct length in par file.", {
  start <- 1; end <- 100; burn <- 20
  change_year(year_begin = start, year_end = end, burnin = burn,
              par_file_in = file.path(om, "ss3.par"), 
              par_file_out = "new.ss")
  new <- readLines("new.ss")
  getnew <- new[grep("recdev1", new) + 1]
  getnew <- gsub(" ", "", getnew)
  expect_equal(nchar(getnew), end - start + 1)
})

setwd(wd.old)
unlink(temp_path, recursive = TRUE)
