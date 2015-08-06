context("Changing parameters in an EM")

wd.old <- getwd()
temp_path <- file.path(tempdir(), "pars")
dir.create(temp_path, showWarnings = FALSE)
setwd(temp_path)
d <- system.file("extdata", package = "ss3sim")
file.copy(file.path(d, "Simple"), ".", recursive = TRUE)
setwd("Simple")

verboseF <- FALSE

datalist <- r4ss::SS_readdat("simple.dat", verbose = verboseF)

test_that("Use change_e to change forecast year", {
  nfors <- 10
  # Manipulate files
  datnew <- change_e(ctl_file_in = "simple.ctl",
    ctl_file_out = "change.ctl",
    dat_list = datalist, for_file_in = "forecast.ss",
    forecast_num = nfors, run_change_e_full = FALSE)
  test <- readLines("forecast.ss")
  t1 <- grep("#_Forecast", test)
  t2 <- grep("#_Nforecastyrs", test)
  expect_true(as.numeric(strsplit(test[t1], "#")[[1]][1]) == 2)
  expect_true(as.numeric(strsplit(test[t2], "#")[[1]][1]) == nfors)
  expect_true(datnew$endyr == datalist$endyr - nfors)
})

# Test will be skipped on CRAN load b/c binary
om <- file.path(d, "models", "cod-om")
em <- file.path(d, "models", "cod-em")

file.copy(file.path(d, "eg-cases"), ".", recursive = TRUE)
case_folder <- file.path("eg-cases")
case_files <- list(F = "F", D = c("index", "lcomp", "agecomp"), E = "E")

newcase <- file.path("eg-cases", "E102-cod.txt")
file.copy(file.path("eg-cases", "E101-cod.txt"), newcase)
temp <- readLines(newcase)
temp[10] <- "forecast_num; 3"
writeLines(temp, newcase)

test_that("A basic run_ss3sim scenario with forecasting runs", {
  skip_on_cran()
  run_ss3sim(iterations = 1, scenarios = "D0-E102-F0-cod",
    case_folder = case_folder, case_files = case_files,
    om_dir = om, em_dir = em,
    ss_mode = "optimized")
  report <- r4ss::SS_output(file.path("D0-E102-F0-cod", "1", "em"),
    covar = FALSE, ncols = 400, NoCompOK = TRUE)
  get_results_all()
  res <- read.csv("ss3sim_scalar.csv", header = TRUE)
  t1 <- res[, grep("LnQ_base_3_CPU", colnames(res))]
  t2 <- res[, grep("SR_sigmaR", colnames(res))]
  expect_true(t1[1] == t1[2])
  expect_true(t2[2] == 0.001)
  unlink("D0-E102-F0-cod", recursive = TRUE) # clean up
})


setwd(wd.old)
unlink(temp_path, recursive = TRUE)
