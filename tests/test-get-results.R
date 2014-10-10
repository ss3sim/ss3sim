# test get-results functions
library(testthat)

d <- '../inst/extdata'
om <- paste0(d, "/models/cod-om")
em <- paste0(d, "/models/cod-em")
case_folder <- paste0(d, "/eg-cases")

run_ss3sim(iterations = 1, scenarios = "D0-E0-F0-R0-M0-cod",
  case_folder = case_folder, om_dir = om, em_dir = em, ss_mode = "safe")
get_results_all()

expect_true(file.exists("ss3sim_scalar.csv"))
expect_true(file.exists("ss3sim_ts.csv"))

unlink("D0-E0-F0-R0-M0-cod", recursive = TRUE) # clean up
file.remove("ss3sim_scalar.csv")
file.remove("ss3sim_ts.csv")
