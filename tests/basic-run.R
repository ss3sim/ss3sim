# Basic tests that should all run without errors

library(testthat)
devtools::install("..")
# library(ss3sim)
## this way was breaking parallel for Cole's windows machines
## devtools::load_all("..")

## Setup:

# Find the data in the ss3sim package:
## d <- system.file("extdata", package = "ss3sim")
setwd("tests")
d <- '../inst/extdata'
om <- paste0(d, "/models/cod-om")
em <- paste0(d, "/models/cod-em")
case_folder <- paste0(d, "/eg-cases")

## Basic run
# serial:
run_ss3sim(iterations = 1, scenarios = "D0-E0-F0-R0-M0-cod",
  case_folder = case_folder, om_dir = om, em_dir = em, ss_mode = "safe")
unlink("D0-E0-F0-R0-M0-cod", recursive = TRUE) # clean up

# Set parallel cores:
library(doParallel)
library(foreach)
registerDoParallel(cores = 2)
getDoParWorkers() # check

# parallel scenarios:
run_ss3sim(iterations = 1,
  scenarios = c("D0-E0-F0-R0-M0-cod", "D0-E0-F0-R1-M0-cod"),
  case_folder = case_folder, om_dir = om, em_dir = em, ss_mode = "optimized",
  parallel = TRUE)
unlink("D0-E0-F0-R0-M0-cod", recursive = TRUE)
unlink("D1-E0-F0-R0-M0-cod", recursive = TRUE)

# parallel iterations:
# run_ss3sim(iterations = 1:2,
#   scenarios = c("D0-E0-F0-R0-M0-cod", "D1-E0-F0-R0-M0-cod"),
#   case_folder = case_folder, om_dir = om, em_dir = em, ss_mode = "optimized",
#   parallel = TRUE, parallel_iterations = TRUE)
# unlink("D0-E0-F0-R0-M0-cod", recursive = TRUE)

## Test get_results_all
## run 2 iterations of 2 scenarios
run_ss3sim(iterations = 1:2, scenarios = c("D0-E0-F0-R0-M0-cod", "D1-E0-F0-R0-M0-cod"),
  case_folder = case_folder, om_dir = om, em_dir = em, ss_mode = "optimized",
  parallel = TRUE)
get_results_all()
expect_warning(
  get_results_all(user_scenarios = c("D0-E0-F0-R0-M0-cod", "D1-E0-F0-R0-M0-cod",
  "D0-E0-F0-R0-M0-T0-cod"), over = TRUE)
)
unlink("D0-E0-F0-R0-M0-cod/1/om/Report.sso")
expect_warning(get_results_all(over = TRUE))
unlink(c("D0-E0-F0-R0-M0-cod", "D1-E0-F0-R0-M0-cod"), recursive = TRUE)
unlink(c("ss3sim_scalar.csv", "ss3sim_ts.csv"))

## Test the addition of tail compression:
# case_files <- list(M = "M", F = "F", D = c("index", "lcomp", "agecomp"),
#   R = "R", E = "E", T="T")

# serial:
# run_ss3sim(iterations = 1:1, scenarios = "D0-E0-F0-R0-M0-T0-cod",
#   case_folder = case_folder, om_dir = om, em_dir = em, ss_mode = "safe",
#   case_files = case_files)
# unlink("D0-E0-F0-R0-M0-T0-cod", recursive = TRUE) # clean up

# parallel:
# run_ss3sim(iterations = 1:1,
#   scenarios = c("D0-E0-F0-R0-M0-T0-cod", "D0-E0-F0-R1-M0-T0-cod"),
#   case_folder = case_folder, om_dir = om, em_dir = em, ss_mode = "safe",
#   case_files = case_files, parallel = TRUE)
# unlink("D0-E0-F0-R0-M0-T0-cod", recursive = TRUE) # clean up
# unlink("D0-E0-F0-R1-M0-T0-cod", recursive = TRUE) # clean up
