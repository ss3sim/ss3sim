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

test_that(, {
  change_year()
  expect_true()
})

setwd(wd.old)
unlink(temp_path, recursive = TRUE)