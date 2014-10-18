# Basic tests that should all run without errors

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

# parallel iterations:
run_ss3sim(iterations = 1:2, scenarios = "D0-E0-F0-R0-M0-cod",
  case_folder = case_folder, om_dir = om, em_dir = em, ss_mode = "safe",
  parallel = TRUE, parallel_iterations = TRUE)
unlink("D0-E0-F0-R0-M0-cod", recursive = TRUE) # clean up

# parallel iterations with bias adjustment:
run_ss3sim(iterations = 1:2, scenarios = "D0-E0-F0-R0-M0-cod",
  case_folder = case_folder, om_dir = om, em_dir = em, ss_mode = "safe",
  parallel = TRUE, parallel_iterations = TRUE, bias_nsim = 2, bias_adjust = TRUE)
unlink("D0-E0-F0-R0-M0-cod", recursive = TRUE) # clean up

# parallel:
run_ss3sim(iterations = 1:1,
  scenarios = c("D0-E0-F0-R0-M0-cod", "D0-E0-F0-R1-M0-cod"),
  case_folder = case_folder, om_dir = om, em_dir = em, ss_mode = "safe",
  parallel = TRUE)
unlink("D0-E0-F0-R0-M0-cod", recursive = TRUE)
unlink("D0-E0-F0-R1-M0-cod", recursive = TRUE)

## Test the addition of tail compression:
case_files <- list(M = "M", F = "F", D = c("index", "lcomp", "agecomp"),
  R = "R", E = "E", T="T")

# serial:
run_ss3sim(iterations = 1:1, scenarios = "D0-E0-F0-R0-M0-T0-cod",
  case_folder = case_folder, om_dir = om, em_dir = em, ss_mode = "safe",
  case_files = case_files)
unlink("D0-E0-F0-R0-M0-T0-cod", recursive = TRUE) # clean up

# parallel:
run_ss3sim(iterations = 1:1,
  scenarios = c("D0-E0-F0-R0-M0-T0-cod", "D0-E0-F0-R1-M0-T0-cod"),
  case_folder = case_folder, om_dir = om, em_dir = em, ss_mode = "safe",
  case_files = case_files, parallel = TRUE)
unlink("D0-E0-F0-R0-M0-T0-cod", recursive = TRUE) # clean up
unlink("D0-E0-F0-R1-M0-T0-cod", recursive = TRUE) # clean up

