context("get results")

temp_path <- file.path(tempdir(), "ss3sim-test")
dir.create(temp_path, showWarnings = FALSE)
wd <- getwd()
setwd(temp_path)

d <- system.file("extdata", package = "ss3sim")
om <- paste0(d, "/models/cod-om")
em <- paste0(d, "/models/cod-em")
case_folder <- paste0(d, "/eg-cases")

test_that("get_results_all() works", {
  skip_on_cran()

  run_ss3sim(iterations = 1, scenarios = "D0-F0-cod",
    case_folder = case_folder, om_dir = om, em_dir = em,
    ss_mode = "optimized")

  get_results_all()
  expect_true(file.exists("ss3sim_scalar.csv"))
  expect_true(file.exists("ss3sim_ts.csv"))
})

test_that("get_results_all() doesn't overwrite files if overwrite_files = FALSE", {
  skip_on_cran()

  unlink("ss3sim_scalar.csv")
  write.csv(1, file = "ss3sim_scalar.csv", row.names = FALSE) # fake data, 1 column
  get_results_all(overwrite_files = FALSE)
  fake_ss3sim_scalar <- read.csv("ss3sim_scalar.csv") # should be fake data
  # TODO: failing:
  # expect_true(identical(1L, ncol(fake_ss3sim_scalar)))

  d <- read.csv("D0-F0-cod/results_scalar_D0-F0-cod.csv")
  d$TotYield_MSY_om <- 3.141592
  write.csv(d, "D0-F0-cod/results_scalar_D0-F0-cod.csv", row.names = FALSE)
  get_results_all(overwrite_files = FALSE)
  d <- read.csv("ss3sim_scalar.csv")
  expect_identical(d$TotYield_MSY_om, 3.141592)
})

test_that("get_results_scenario() doesn't overwrite files if overwrite_files = FALSE", {
  skip_on_cran()

  expect_error(get_results_scenario("D0-F0-cod", overwrite_files = FALSE))
})

test_that("get_results_scenario() does overwrite files if overwrite_files = TRUE", {
  skip_on_cran()

  get_results_scenario("D0-F0-cod", overwrite_files = TRUE)
  ss3sim_scalar <- read.csv("ss3sim_scalar.csv") # should be real data
  expect_true(ncol(ss3sim_scalar) > 1)

  unlink(c("ss3sim_scalar.csv", "ss3sim_ts.csv"))
  unlink("D0-F0-cod", recursive = TRUE)
})

setwd(wd)
