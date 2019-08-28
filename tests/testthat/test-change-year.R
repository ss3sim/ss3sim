context("Changing years in OM and EM")

wd.old <- getwd()
temp_path <- file.path(tempdir(), "year")
dir.create(temp_path, showWarnings = FALSE)
setwd(temp_path)
d <- system.file("extdata", package = "ss3sim")
file.copy(file.path(d, "Simple"), ".", recursive = TRUE)
setwd("Simple")
on.exit(setwd(wd.old), add = TRUE)
on.exit(unlink(file.path(temp_path, "Simple"), recursive = TRUE), add = TRUE)

test_that("Control file references correct years", {
  # Manipulate files
  change_year(year_begin = 1, year_end = 26, burnin = 25,
              ctl_file_in = "simple.ctl", ctl_file_out = "change.ctl")
  test <- readLines("change.ctl")
  t1 <- grep("first year of main recr_devs", test)
  t2 <- grep("last year of main recr_devs", test)
  t3 <- grep("_first_yr_fullbias_adj", test)
  t4 <- grep("_last_yr_fullbias_adj", test)
    # Check F ballpark year
  range <- 1:26
  expect_true(as.numeric(strsplit(test[t1], "#")[[1]][1]) %in% range)
  expect_true(as.numeric(strsplit(test[t2], "#")[[1]][1]) %in% range)
  expect_true(as.numeric(strsplit(test[t3], "#")[[1]][1]) %in% range)
  expect_true(as.numeric(strsplit(test[t4], "#")[[1]][1]) %in% range)
})

om <- paste0(d, "/models/cod-om")
em <- paste0(d, "/models/cod-em")
ignore <- file.copy(om, ".", recursive = TRUE)
verbose <- FALSE
# use reading the starter file to determine the model version.
# get ss version by reading starter for om
om_version <- r4ss::SS_readstarter(paste0(om, "/starter.ss"))
if(om_version$SSversion == "3.24 or earlier"){
  om_version <- "3.24" # assume 3.24
} else if (om_version$SSversion == "3.30") {
  om_version <- om_version$SSversion
} else {
  om_version <- NA # version not known
}

test_that("Forecast file is readable.", {
  om.for <- r4ss::SS_readforecast(file.path(om, "forecast.ss"), 1, 1,
                                  version = om_version, verbose = verbose)
  change_year(for_file_in = file.path(om, "forecast.ss"),
              for_file_out = "new.ss")
  om.for.new <- r4ss::SS_readforecast("new.ss", 1, 1, version = om_version,
                                      verbose = verbose)
  out <- evaluate_promise(r4ss::SS_readforecast("new.ss", 1, 1, version = om_version),
                          print = TRUE)$output
  expect_equal(grepl("Error", out), FALSE)
})

test_that("Correct lines are changed in starter file.", {
  change_year(str_file_in = file.path(om, "starter.ss"),
              str_file_out = "new.ss")
  new <- readLines("new.ss")
  getnew <- new[grep("jitter", new) + 1:2]
  getnew <- gsub(" ", "", sapply(strsplit(getnew, "#"), "[[", 1))
  expect_equal(getnew, c("-1", "-2"))
})

test_that("Changes to dat file with change_year.", {
  start <- 1999; end <- 2000; burn <- 0
  change_year(year_begin = start, year_end = end, burnin = burn,
              dat_file_in = file.path(om, "codOM.dat"),
              dat_file_out = "newdat.ss")
  new <- r4ss::SS_readdat("newdat.ss", verbose = FALSE)
  expect_equal(end, new$endyr)
  expect_equal(nrow(new$lencomp), 1)
  expect_equal(nrow(new$agecomp), 1)
  expect_equal(nrow(new$catch), length(start:end) + 1)
})

unlink(temp_path, recursive = TRUE)
