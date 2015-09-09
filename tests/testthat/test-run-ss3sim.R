context("run_ss3sim and get-results functions work across a range of scenarios")

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

test_that("A basic run_ss3sim scenario runs", {
  skip_on_cran()
  run_ss3sim(iterations = 1, scenarios = "D0-F0-cod",
    case_folder = case_folder, om_dir = om, em_dir = em, ss_mode = "optimized")
  unlink("D0-F0-cod", recursive = TRUE) # clean up
})

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

test_that("get_results_all is working", {
  skip_on_cran()
  get_results_all()
  d <- read.csv("ss3sim_scalar.csv")
  d <- read.csv("ss3sim_ts.csv")
  unlink(c("ss3sim_scalar.csv", "ss3sim_ts.csv"))
})

test_that("get_results_all is working in parallel", {
  skip_on_cran()
  get_results_all(parallel = TRUE)
  d <- read.csv("ss3sim_scalar.csv")
  d <- read.csv("ss3sim_ts.csv")
  unlink(c("ss3sim_scalar.csv", "ss3sim_ts.csv"))
})

test_that("get_results_all warns if a scenario folder isn't available", {
  skip_on_cran()
  expect_warning(
    get_results_all(user_scenarios =
        c("D0-F0-cod", "D1-F0-cod", "D0-F0-X0-cod")))
  unlink(c("ss3sim_scalar.csv", "ss3sim_ts.csv"))

  # done with these scenarios now:
  unlink(c("D0-F0-cod", "D1-F0-cod"), recursive = TRUE)
})

test_that("Skip an iteration that already exists", {
  skip_on_cran()
  run_ss3sim(iterations = 1, scenarios = "D0-F0-cod",
    case_folder = case_folder, om_dir = om, em_dir = em, ss_mode = "optimized")
  expect_warning(run_ss3sim(iterations = 1:2, scenarios = "D0-F0-cod",
    case_folder = case_folder, om_dir = om, em_dir = em, ss_mode = "optimized"))
  unlink("D0-F0-cod", recursive = TRUE) # clean up
})

setwd(wd)
