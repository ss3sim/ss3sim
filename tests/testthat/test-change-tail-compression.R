temp_path <- file.path(tempdir(), "ss3sim-test")
dir.create(temp_path, showWarnings = FALSE)
wd <- getwd()
setwd(temp_path)

d <- system.file("extdata", package = "ss3sim")
om <- paste0(d, "/models/cod-om")
em <- paste0(d, "/models/cod-em")
case_folder <- paste0(d, "/eg-cases")

test_that("change_tail_compression changes the tail compression value", {
  dat_file <- system.file("extdata", "example-om", "data.ss_new",
    package = "ss3sim")
  dat_file <- r4ss::SS_readdat(dat_file, verbose = FALSE)
  test_tc <- change_tail_compression(tail_compression = .1234, dat_file,
    file_out = "test.dat")
  expect_equal(test_tc$comp_tail_compression, 0.1234)
})

test_that("change_tail_compression works with ss3sim_base", {

  skip_on_cran()

  # T0 = no tail compression, T1 = 0.1 tail compression:
  run_ss3sim(iterations = 1,
    scenarios = c("D0-E0-F0-R0-M0-T0-cod", "D0-E0-F0-R0-M0-T1-cod"),
    case_folder = case_folder, om_dir = om,
    em_dir = em, case_files = list(M = "M", F = "F", D =
        c("index", "lcomp", "agecomp"), R = "R", E = "E", T = "tail_compression"),
    ss_mode = "optimized")

  # without specifying tail compression:
  run_ss3sim(iterations = 1,
    scenarios = c("D0-E0-F0-R0-M0-cod"),
    case_folder = case_folder, om_dir = om,
    em_dir = em, case_files = list(M = "M", F = "F", D =
        c("index", "lcomp", "agecomp"), R = "R", E = "E"),
    ss_mode = "optimized")

  # quickly grab results to see if any difference:
  get_results_all(user_scenarios =
      c("D0-E0-F0-R0-M0-T0-cod",
        "D0-E0-F0-R0-M0-T1-cod",
        "D0-E0-F0-R0-M0-cod"))

  results <- read.csv("ss3sim_scalar.csv")

  print("The first two values should be the same, the third different:")
  expect_equal(results$NLL_TOTAL_em[1], results$NLL_TOTAL_em[3])
  expect_false(results$NLL_TOTAL_em[1] == results$NLL_TOTAL_em[2])

  unlink(c("ss3sim_scalar.csv", "ss3sim_ts.csv"))
  unlink("D0-E0-F0-R0-M0-T0-cod", TRUE)
  unlink("D0-E0-F0-R0-M0-T1-cod", TRUE)
  unlink("D0-E0-F0-R0-M0-cod", TRUE)
})

setwd(wd)
