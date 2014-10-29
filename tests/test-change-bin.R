# some adhoc testing of the new change_bin()
# SA 20141028

d <- '../inst/extdata'
om <- paste0(d, "/models/cod-om")
em <- paste0(d, "/models/cod-em")
case_folder <- paste0(d, "/cases")

run_ss3sim(iterations = 1, scenarios = "D2-E0-F0-R0-M0-cod",
  case_folder = case_folder, om_dir = om, em_dir = em, ss_mode = "safe")
unlink("D2-E0-F0-R0-M0-cod", recursive = TRUE)
