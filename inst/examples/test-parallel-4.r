require(ss3sim)
require(doParallel)
require(foreach)
registerDoParallel(cores = 4)
getDoParWorkers()

d <- system.file("extdata", package = "ss3sim")
case_folder <- paste0(d, "/eg-cases")
om <- paste0(d, "/models/cod-om")
em <- paste0(d, "/models/cod-em")
sc <- expand_scenarios(d = c(0, 1), e = c(0, 1), species = "cod")

time_par <- system.time({
run_ss3sim(iterations = 1:20, scenarios = sc, case_folder =
  case_folder, om_model_dir = om, em_model_dir = em, parallel = TRUE)
unlink(sc, recursive = TRUE)
})

time_seq <- system.time({
run_ss3sim(iterations = 1:20, scenarios = sc, case_folder =
  case_folder, om_model_dir = om, em_model_dir = em, parallel = FALSE)
unlink(sc, recursive = TRUE)
})

sink("test-parallel-4-time.txt")
print(time_par)
print(time_seq)
sink()
