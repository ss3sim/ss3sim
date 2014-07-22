# From ?run_ss3sim:
#temp_path <- file.path(tempdir(), "ss3sim-example")
#dir.create(temp_path, showWarnings = FALSE)
#wd <- getwd()
#setwd(temp_path)

# Find the data in the ss3sim package:
d <- system.file("extdata", package = "ss3sim")
om <- paste0(d, "/models/cod-om")
em <- paste0(d, "/models/cod-em")
case_folder <- paste0(d, "/eg-cases")

# Without bias adjustment:
run_ss3sim(iterations = 1:1, scenarios = "D0-E0-F0-R0-M0-cod",
  case_folder = case_folder, om_dir = om, em_dir = em, ss_mode = "optimized")
#setwd(wd)
unlink("D0-E0-F0-R0-M0-cod", recursive = TRUE) # clean up
