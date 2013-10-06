require(ss3sim)
require(doParallel)
require(foreach)
registerDoParallel(cores = 2)
getDoParWorkers()

d <- system.file("extdata", package = "ss3sim")
case_folder <- paste0(d, "/eg-cases")
om <- paste0(d, "/models/cod-om")
em <- paste0(d, "/models/cod-em")

time_par <- system.time({
run_ss3sim(iterations = 1:20, scenarios = c("D0-E0-F0-G0-R0-S0-M0-cod",
    "D1-E0-F0-G0-R0-S0-M0-cod"), case_folder = case_folder,
  om_model_dir = om, em_model_dir = em, parallel = TRUE)
unlink("D0-E0-F0-G0-R0-S0-M0-cod", recursive = TRUE) # clean up
unlink("D1-E0-F0-G0-R0-S0-M0-cod", recursive = TRUE) # clean up
})

time_seq <- system.time({
run_ss3sim(iterations = 1:20, scenarios = c("D0-E0-F0-G0-R0-S0-M0-cod",
    "D1-E0-F0-G0-R0-S0-M0-cod"), case_folder = case_folder,
  om_model_dir = om, em_model_dir = em, parallel = FALSE)
unlink("D0-E0-F0-G0-R0-S0-M0-cod", recursive = TRUE) # clean up
unlink("D1-E0-F0-G0-R0-S0-M0-cod", recursive = TRUE) # clean up
})

sink("test-parallel-time.txt")
print(time_par)
print(time_seq)
sink()
