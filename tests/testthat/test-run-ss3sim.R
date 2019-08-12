context("run_ss3sim and get-results functions work across a range of scenarios")

# Note that these tests are skipped on CRAN since the SS3 executable won't
# be available (and some of these take a while to run).

# TODO: turn runs of ss3sim into actual expectations.

temp_path <- file.path(tempdir(), "ss3sim-test")
dir.create(temp_path, showWarnings = FALSE)
wd <- getwd()
setwd(temp_path)
on.exit(setwd(wd), add = TRUE)

d <- system.file("extdata", package = "ss3sim")
om <- file.path(d, "models", "cod-om")
em <- file.path(d, "models", "cod-em")
case_folder <- file.path(d, "eg-cases")


test_that("A basic run_ss3sim scenario runs and existing iteration skipped", {
  skip_on_cran()
  suppressWarnings(run_ss3sim(iterations = 1, scenarios = "D0-F0-cod",
    case_folder = case_folder, om_dir = om, em_dir = em, ss_mode = "optimized"))
  #check ran
  expect_true("control.ss_new" %in% list.files(file.path("D0-F0-cod","1", "em")))
  #check provides warning if skippint iteration.
  expect_warning(run_ss3sim(iterations = 1, scenarios = "D0-F0-cod",
                            case_folder = case_folder,
                            om_dir = om,
                            em_dir = em,
                            ss_mode = "optimized"),
    "already exists", all = TRUE, fixed = TRUE)

  unlink("D0-F0-cod", recursive = TRUE) # clean up
})

test_that("run_ss3sim works with multiple scenarios (no parallel)", {
  skip_on_cran()
  suppressWarnings(run_ss3sim(iterations = 1, scenarios = c("D0-F0-cod", "D1-F0-cod"),
             case_folder = case_folder, om_dir = om, em_dir = em, ss_mode = "optimized"))
  expect_true("control.ss_new" %in% list.files(file.path("D0-F0-cod","1", "em")))
  expect_true("control.ss_new" %in% list.files(file.path("D1-F0-cod", "1", "em")))
  unlink("D0-F0-cod", recursive = TRUE) # clean up
  unlink("D1-F0-cod", recursive = TRUE)
})
