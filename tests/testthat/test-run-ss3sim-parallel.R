context("run_ss3sim and get-results functions work in parallel")

# Note that these tests are skipped on CRAN since the SS3 executable won't
# be available (and some of these take a while to run).

temp_path <- file.path(tempdir(), "ss3sim-test")
dir.create(temp_path, showWarnings = FALSE)
wd <- getwd()
setwd(temp_path)

d <- system.file("extdata", package = "ss3sim")
om <- paste0(d, "/models/cod-om")
em <- paste0(d, "/models/cod-em")
case_folder <- paste0(d, "/eg-cases")

test_that("run_ss3sim works with parallel iterations", {
  skip_on_cran()
  library("doParallel")
  library("foreach")
  registerDoParallel(cores = 2)
  run_ss3sim(iterations = 1:2, scenarios = "D0-F0-cod",
    case_folder = case_folder, om_dir = om, em_dir = em, ss_mode = "optimized",
    parallel = TRUE, parallel_iterations = TRUE)
unlink("D0-F0-cod", recursive = TRUE)
})

test_that("run_ss3sim works with parallel scenarios", {
  skip_on_cran()
  run_ss3sim(iterations = 1,
    scenarios = c("D0-F0-cod", "D1-F0-cod"),
    case_folder = case_folder, om_dir = om, em_dir = em, ss_mode = "optimized",
    parallel = TRUE)
})

test_that("get_results_all is working in parallel", {
  skip_on_cran()
  get_results_all(parallel = TRUE)
  d <- read.csv("ss3sim_scalar.csv")
  d <- read.csv("ss3sim_ts.csv")
  unlink(c("ss3sim_scalar.csv", "ss3sim_ts.csv"))
})

  # done with these scenarios now:
unlink(c("D0-F0-cod", "D1-F0-cod"), recursive = TRUE)

setwd(wd)
