context("get results")
#TODO: rewrite so no need to run model.
temp_path <- file.path(tempdir(), "ss3sim-test")
dir.create(temp_path, showWarnings = FALSE)
wd <- getwd()
setwd(temp_path)
on.exit(setwd(wd), add = TRUE)

d <- system.file("extdata", package = "ss3sim")
om <- file.path(d, "models", "cod-om")
em <- file.path(d, "models", "cod-em")
case_folder <- file.path(d, "eg-cases")

test_that("get_results_all() works", {
  skip_on_cran()

  suppressWarnings(run_ss3sim(iterations = 1, scenarios = "D0-F0-cod",
    case_folder = case_folder, om_dir = om, em_dir = em,
    ss_mode = "optimized"))

  get_results_all()
  expect_true(file.exists("ss3sim_scalar.csv"))
  expect_true(file.exists("ss3sim_ts.csv"))
})


test_that("get_results_all() doesn't overwrite files if overwrite_files = FALSE", {
  skip_on_cran()

  unlink("ss3sim_scalar.csv")
  write.csv(1, file = "ss3sim_scalar.csv", row.names = FALSE) # fake data, 1 column
  expect_warning(get_results_all(overwrite_files = FALSE))
  fake_ss3sim_scalar <- read.csv("ss3sim_scalar.csv") # should be fake data
  expect_identical(1L, ncol(fake_ss3sim_scalar))

  d <- read.csv("D0-F0-cod/results_scalar_D0-F0-cod.csv")
  d$TotYield_MSY_om <- 3.141592
  write.csv(d, "D0-F0-cod/results_scalar_D0-F0-cod.csv", row.names = FALSE)
  expect_warning(get_results_all(overwrite_files = FALSE))
  d <- read.csv(file.path("D0-F0-cod", "results_scalar_D0-F0-cod.csv"))
  expect_identical(d$TotYield_MSY_om, 3.141592)
})

test_that("get_results_scenario() doesn't overwrite files if overwrite_files = FALSE", {
  skip_on_cran()

  expect_error(get_results_scenario("D0-F0-cod", overwrite_files = FALSE),
               "Files already exist for")
})

test_that("get_results_scenario() does overwrite files if overwrite_files = TRUE", {
  skip_on_cran()
  write.csv(1, file.path("D0-F0-cod", "results_scalar_D0-F0-cod.csv"))
  get_results_scenario("D0-F0-cod", overwrite_files = TRUE)
  ss3sim_scalar <- read.csv(file.path("D0-F0-cod", "results_scalar_D0-F0-cod.csv")) # should be real data
  expect_true(ncol(ss3sim_scalar) > 1)

  unlink(c("ss3sim_scalar.csv", "ss3sim_ts.csv"))
  unlink("D0-F0-cod", recursive = TRUE)
})
