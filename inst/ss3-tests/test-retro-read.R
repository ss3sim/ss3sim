temp_path <- file.path(tempdir(), "ss3sim-test")
dir.create(temp_path, showWarnings = FALSE)
wd <- getwd()
setwd(temp_path)

d <- system.file("extdata", package = "ss3sim")
om <- paste0(d, "/models/cod-om")
em <- paste0(d, "/models/cod-em")
case_folder <- paste0(d, "/eg-cases")

sc <- c("D0-E0-F0-R0-M0-cod", "D0-E0-F0-R1-M0-cod")

run_ss3sim(iterations = 1, 
  scenarios = sc, case_folder = case_folder, om_dir = om, em_dir = em)

get_results_all(user_scenarios = sc)

scalars <- read.csv("ss3sim_scalar.csv")
ts <- read.csv("ss3sim_ts.csv")

# clean up:
unlink(sc)
unlink("ss3sim_scalar.csv")
unlink("ss3sim_ts.csv")
