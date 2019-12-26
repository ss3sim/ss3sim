context("run_ss3sim and get-results functions work across a range of scenarios")

# Note that these tests are skipped on CRAN since the SS3 executable won't
# be available (and some of these take a while to run).

# TODO: turn runs of ss3sim into actual expectations
# also add tests of ss3sim_base in addition to run_ss3sim.

temp_path <- file.path(tempdir(), "ss3sim-test")
dir.create(temp_path, showWarnings = FALSE)
wd <- getwd()
setwd(temp_path)
on.exit(setwd(wd), add = TRUE)
on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)

d <- system.file("extdata", package = "ss3sim")
om <- file.path(d, "models", "cod-om")
em <- file.path(d, "models", "cod-em")
case_folder <- file.path(d, "eg-cases")


test_that("A basic run_ss3sim scenario runs and existing iteration skipped", {
  skip_on_cran()
  suppressWarnings(run_ss3sim(iterations = 1, scenarios = "D0-F0-cod",
    case_folder = case_folder, om_dir = om, em_dir = em))
  #check ran
  expect_true("control.ss_new" %in% list.files(file.path("D0-F0-cod","1", "em")))
  success <- get_success(file.path("D0-F0-cod", "1", "em"))
  expect_equal(success["ran"], c("ran" = 1),
    label = "Sucess vector for the Report file is")
  expect_equal(success["hess"], c("hess" = 0),
    label = "Sucess vector for the hessian file is")
  #check provides warning if skippint iteration.
  expect_warning(run_ss3sim(iterations = 1, scenarios = "D0-F0-cod",
                            case_folder = case_folder,
                            om_dir = om,
                            em_dir = em),
    "already exists", all = TRUE, fixed = TRUE)
  biasdir <- file.path("D0-F0-cod", "1", "em")
  expect_warning(calculate_bias(dir = biasdir, "em.ctl"),
    label = "With no hessian calculate_bias")
  setwd(biasdir)
  expect_true(file.exists("bias_00"))
  unlink("bias_00", recursive = TRUE)
  system(get_bin(), show.output.on.console = FALSE)
  bias_list <- calculate_bias(getwd(), "em.ctl")
  expect_equal(bias_list$df$value[5], 0.56, tolerance = 0.01,
    label = "Estimated max adjust")
  bias_list <- suppressWarnings(calculate_bias(getwd(), "em.ctl"))
  expect_true(file.exists("bias_01"),
    label = "The bias_01 folder is present after calling bias 2x")
  setwd(temp_path)
  unlink("D0-F0-cod", recursive = TRUE) # clean up
})

test_that("run_ss3sim works with multiple scenarios (no parallel)", {
  skip_on_cran()
  suppressWarnings(run_ss3sim(iterations = 1, scenarios = c("D0-F0-cod", "D1-F0-cod"),
             case_folder = case_folder, om_dir = om, em_dir = em))
  expect_true("control.ss_new" %in% list.files(file.path("D0-F0-cod","1", "em")))
  expect_true("control.ss_new" %in% list.files(file.path("D1-F0-cod", "1", "em")))
  unlink("D0-F0-cod", recursive = TRUE) # clean up
  unlink("D1-F0-cod", recursive = TRUE)
})

test_that("run_ss3sim gives error if conditional age at length used", {
  # when CAL implemented, will want to remove this test.
  skip_on_cran()
  expect_error(run_ss3sim(iterations  = 1,
                          scenarios = "F1-D0-M1-E0-O0-cod",
                          case_folder = file.path(d, "eg-cases"),
                          om_dir = om_dir,
                          em_dir = em_dir,
                          case_files = list(F = "F",
                                            D = c("index", "lcomp", "agecomp",
                                                  "calcomp"),
                                            M = "M", E = "E", O = "O")
  ), "Conditional age at length (CAL) is not yet implemented", fixed = TRUE)
})

case_files <- list(F = "F", D = c("index", "lcomp", "agecomp"), E = "E")
test_that("A basic run_ss3sim scenario with forecasting runs", {
  skip_on_cran()
  suppressWarnings(run_ss3sim(iterations = 1, scenarios = "D0-E102-F0-cod",
             case_folder = case_folder, case_files = case_files,
             om_dir = om, em_dir = em))
  expect_true("control.ss_new" %in% list.files(file.path("D0-E102-F0-cod", "1", "em")))
  report <- suppressWarnings(r4ss::SS_output(file.path("D0-E102-F0-cod", "1", "em"),
                            covar = FALSE, ncols = 400, NoCompOK = TRUE,
                            verbose = FALSE, printstats = FALSE))
  get_results_all()
  res <- read.csv("ss3sim_scalar.csv", header = TRUE)
  expect_equal(res$LnQ_base_Survey_2_em, 0.7)
  expect_equal(res$SR_sigmaR_em, 0.001)
  #TODO: add expectation that shows that forecasting worked.
  unlink("D0-E102-F0-cod", recursive = TRUE) # clean up
})
