context("run_ss3sim works across a range of scenarios")

temp_path <- file.path(tempdir(), "ss3sim-test")
dir.create(temp_path, showWarnings = FALSE)
wd <- getwd()
setwd(temp_path)

d <- system.file("extdata", package = "ss3sim")
om <- paste0(d, "/models/cod-om")
em <- paste0(d, "/models/cod-em")
case_folder <- paste0(d, "/eg-cases")

test_that("A basic run_ss3sim scenario runs:", {
  skip_on_cran()
  run_ss3sim(iterations = 1, scenarios = "D0-E0-F0-R0-M0-cod",
    case_folder = case_folder, om_dir = om, em_dir = em, ss_mode = "optimized")
  unlink("D0-E0-F0-R0-M0-cod", recursive = TRUE) # clean up
})

test_that("run_ss3sim works with parallel iterations:", {
  skip_on_cran()
  library("doParallel")
  library("foreach")
  registerDoParallel(cores = 2)
  run_ss3sim(iterations = 1:2, scenarios = "D0-E0-F0-R0-M0-cod",
    case_folder = case_folder, om_dir = om, em_dir = em, ss_mode = "optimized",
    parallel = TRUE, parallel_iterations = TRUE)
  unlink("D0-E0-F0-R0-M0-cod", recursive = TRUE) # clean up
})

test_that("run_ss3sim works with parallel scenarios:", {
  skip_on_cran()
  run_ss3sim(iterations = 1,
    scenarios = c("D0-E0-F0-R0-M0-cod", "D0-E0-F0-R1-M0-cod"),
    case_folder = case_folder, om_dir = om, em_dir = em, ss_mode = "optimized",
    parallel = TRUE)
  unlink("D0-E0-F0-R0-M0-cod", recursive = TRUE)
  unlink("D1-E0-F0-R0-M0-cod", recursive = TRUE)
})

setwd(wd)
