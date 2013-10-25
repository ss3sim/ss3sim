library(ss3sim)
d <- system.file("extdata", package = "ss3sim")
case_folder <- paste0(d, "/eg-cases")
om <- paste0(d, "/models/cod-om")
em <- paste0(d, "/models/cod-em")

recdevs_det <- matrix(0, nrow = 100, ncol = 20)

require(doParallel)
registerDoParallel(cores = 4)
require(foreach)
# getDoParWorkers()

run_ss3sim(iterations = 1:20, scenarios =
  c("D0-E100-F0-R0-M0-cod",
    "D1-E100-F0-R0-M0-cod",
    "D0-E101-F0-R0-M0-cod",
    "D1-E101-F0-R0-M0-cod"),
  case_folder = case_folder, om_model_dir = om, em_model_dir = em,
  bias_adjust = TRUE, user_recdevs = recdevs_det, parallel = TRUE)

run_ss3sim(iterations = 1:50, scenarios =
  c("D0-E0-F0-R0-M0-cod",
    "D1-E0-F0-R0-M0-cod",
    "D0-E1-F0-R0-M0-cod",
    "D1-E1-F0-R0-M0-cod"),
  case_folder = case_folder, om_model_dir = om,
  em_model_dir = em, bias_adjust = TRUE, parallel = TRUE)

get_results_all(user_scenarios =
  c("D0-E100-F0-R0-M0-cod",
    "D1-E100-F0-R0-M0-cod",
    "D0-E101-F0-R0-M0-cod",
    "D1-E101-F0-R0-M0-cod",
    "D0-E0-F0-R0-M0-cod",
    "D1-E0-F0-R0-M0-cod",
    "D0-E1-F0-R0-M0-cod",
    "D1-E1-F0-R0-M0-cod"))

scalar_dat <- read.csv("ss3sim_scalar.csv")
ts_dat <- read.csv("ss3sim_ts.csv")

save(ts_dat, file = "ts_dat.rda")
save(scalar_dat, file = "scalar_dat.rda")
