context("run_ss3sim and get-results functions work across a range of scenarios")

# Note that these tests are skipped on CRAN since the SS3 executable won't
# be available (and some of these take a while to run).

# TODO: turn runs of ss3sim into actual expectations
# also add tests of ss3sim_base in addition to run_ss3sim.

temp_path <- file.path(tempdir(), "ss3sim-test")
dir.create(temp_path, showWarnings = FALSE)
wd <- getwd()
setwd(temp_path)
on.exit(setwd(wd), add = TRUE)

d <- system.file("extdata", package = "ss3sim")
om <- file.path(d, "models", "cod-om")
em <- file.path(d, "models", "cod-em")
case_folder <- file.path(d, "eg-cases")


test_that("A basic run_ss3sim scenario runs and existing iteration skipped", {
  skip_on_cran()
  suppressWarnings(run_ss3sim(iterations = 1, scenarios = "D0-F0-cod",
    case_folder = case_folder, om_dir = om, em_dir = em, ss_mode = "optimized"))
  #check ran
  expect_true("control.ss_new" %in% list.files(file.path("D0-F0-cod","1", "em")))
  #check provides warning if skippint iteration.
  expect_warning(run_ss3sim(iterations = 1, scenarios = "D0-F0-cod",
                            case_folder = case_folder,
                            om_dir = om,
                            em_dir = em,
                            ss_mode = "optimized"),
    "already exists", all = TRUE, fixed = TRUE)

  unlink("D0-F0-cod", recursive = TRUE) # clean up
})

test_that("run_ss3sim works with multiple scenarios (no parallel)", {
  skip_on_cran()
  suppressWarnings(run_ss3sim(iterations = 1, scenarios = c("D0-F0-cod", "D1-F0-cod"),
             case_folder = case_folder, om_dir = om, em_dir = em, ss_mode = "optimized"))
  expect_true("control.ss_new" %in% list.files(file.path("D0-F0-cod","1", "em")))
  expect_true("control.ss_new" %in% list.files(file.path("D1-F0-cod", "1", "em")))
  unlink("D0-F0-cod", recursive = TRUE) # clean up
  unlink("D1-F0-cod", recursive = TRUE)
})

# Test will be skipped on CRAN load b/c binary (moved from change-e)
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
