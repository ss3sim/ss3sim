library(microbenchmark)

# Find the data in the ss3sim package:
d <- system.file("extdata", package = "ss3sim")
om <- paste0(d, "/models/cod-om")
em <- paste0(d, "/models/cod-em")
case_folder <- paste0(d, "/eg-cases")

# Set parallel cores:
library(doParallel)
library(foreach)
registerDoParallel(cores = 2)

# Code waiting until feature/data branch is merged into master:
# Benchmark parallel iterations:
# bench_rs_parallel_it <- microbenchmark(
#   parallel = {
#     run_ss3sim(iterations = 1:2,
#     scenarios = c("D0-E0-F0-R0-M0-cod"),
#     case_folder = case_folder, om_dir = om, em_dir = em,
#     ss_mode = "optimized", parallel = TRUE, parallel_iterations = TRUE)
#     unlink("D0-E0-F0-R0-M0-cod", recursive = TRUE)
#     },
#   serial = {
#     run_ss3sim(iterations = 1:2,
#     scenarios = c("D0-E0-F0-R0-M0-cod"),
#     case_folder = case_folder, om_dir = om, em_dir = em,
#     ss_mode = "optimized", parallel = FALSE)
#     unlink("D0-E0-F0-R0-M0-cod", recursive = TRUE)
#   },
#   times = 3L
# )
# boxplot(bench_rs_parallel_it)

# Benchmark parallel scenarios:
bench_rs_parallel_sc <- microbenchmark(
  parallel = {
    run_ss3sim(iterations = 1,
    scenarios = c("D0-E0-F0-R0-M0-cod", "D0-E0-F0-R1-M0-cod"),
    case_folder = case_folder, om_dir = om, em_dir = em,
    ss_mode = "optimized", parallel = TRUE)
    unlink(c("D0-E0-F0-R0-M0-cod", "D0-E0-F0-R1-M0-cod"), recursive = TRUE)
    },
  serial = {
    run_ss3sim(iterations = 1,
    scenarios = c("D0-E0-F0-R0-M0-cod", "D0-E0-F0-R1-M0-cod"),
    case_folder = case_folder, om_dir = om, em_dir = em,
    ss_mode = "optimized", parallel = FALSE)
    unlink(c("D0-E0-F0-R0-M0-cod", "D0-E0-F0-R1-M0-cod"), recursive = TRUE)
  },
  times = 1L
)
boxplot(bench_rs_parallel_sc)

registerDoParallel(cores = 4)
# Make folders for get results benchmarking:
run_ss3sim(iterations = 1, scenarios = c("D0-E0-F0-R0-M0-cod",
  "D1-E0-F0-R0-M0-cod", "D0-E1-F0-R0-M0-cod", "D0-E0-F0-R1-M0-cod"),
  case_folder = case_folder, om_dir = om, em_dir = em,
  ss_mode = "optimized", parallel = TRUE)

# Benchmark parallel get results:
bench_gr_parallel <-  microbenchmark(
  parallel = get_results_all(parallel=TRUE, over=TRUE),
  serial = get_results_all(parallel=FALSE, over=TRUE),
  times = 3L
)
boxplot(bench_gr_parallel)
unlink(c("D0-E0-F0-R0-M0-cod", "D1-E0-F0-R0-M0-cod",
  "D0-E1-F0-R0-M0-cod", "D0-E0-F0-R1-M0-cod"), recursive = TRUE)
unlink(c("ss3sim_scalar.csv", "ss3sim_ts.csv"))
