context("run_ss3sim and get-results functions work across a range of scenarios")

# Note that these tests are skipped on CRAN since the SS3 executable won't
# be available (and some of these take a while to run).

# TODO: turn runs of ss3sim into actual expectations
# also add tests of ss3sim_base in addition to run_ss3sim.

temp_path <- file.path(tempdir(), "run-ss3sim-test")
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
  #check provides warning if skippint iteration.
  expect_warning(run_ss3sim(iterations = 1, scenarios = "D0-F0-cod",
                            case_folder = case_folder,
                            om_dir = om,
                            em_dir = em),
    "already exists", all = TRUE, fixed = TRUE)
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

test_that("run_ss3sim runs if conditional age at length used", {
  # when CAL implemented, will want to remove this test.
  skip_on_cran()
  scen <- "F1-D0-M0-E0-O0-cod"
  run_ss3sim(iterations  = 1,
                          scenarios = scen,
                          case_folder = file.path(d, "eg-cases"),
                          om_dir = om,
                          em_dir = em,
                          case_files = list(F = "F",
                                            D = c("index", "lcomp", "agecomp",
                                                  "calcomp"),
                                            M = "M", E = "E", O = "O")
  )
  calcomp_args <- get_args(file.path(case_folder, "calcomp0-cod.txt"))
  EM_datfile <- r4ss::SS_readdat(file.path(scen, "1", "EM", "ss3.dat"))
  # check the length comps to make sure CAL consistent
  lengths <- EM_datfile$lencomp
  lengths <- lengths[lengths$Yr %in% calcomp_args$years[[1]] &
                lengths$FltSvy %in% calcomp_args$fleets, ]
  expect_true(all(calcomp_args$years[[1]] %in% lengths$Yr))
  expect_true(all(calcomp_args$fleets %in% lengths$FltSvy))
  # check the age comps (note that this is not a generalizable test and would
  # need to be refactored to apply to other cases.
  for (yr in calcomp_args$years[[1]]) {
    for (ft in calcomp_args$fleets) {
      tmp_agecomp <- EM_datfile$agecomp[EM_datfile$agecomp$Yr == yr &
                                        EM_datfile$agecomp$FltSvy, ]
      tmp_agecomp <- tmp_agecomp[tmp_agecomp$Lbin_lo != -1, ]
      expect_equivalent(sum(tmp_agecomp$Nsamp),  calcomp_args$Nsamp[[1]])
    }
  }
  unlink(file.path(temp_path, scen), recursive = TRUE)
})

case_files <- list(F = "F", D = c("index", "lcomp", "agecomp"), E = "E")
test_that("A basic run_ss3sim scenario with forecasting runs", {
  skip_on_cran()
  scen <- "D0-E102-F0-cod"
  suppressWarnings(run_ss3sim(iterations = 1, scenarios = scen,
             case_folder = case_folder, case_files = case_files,
             om_dir = om, em_dir = em))
  expect_true("control.ss_new" %in% list.files(file.path(scen, "1", "em")))
  report <- suppressWarnings(r4ss::SS_output(file.path(scen, "1", "em"),
                            covar = FALSE, ncols = 400, NoCompOK = TRUE,
                            verbose = FALSE, printstats = FALSE))
  get_results_all()
  res <- read.csv("ss3sim_scalar.csv", header = TRUE)
  expect_equal(res$LnQ_base_Survey_2_em, 0.7)
  expect_equal(res$SR_sigmaR_em, 0.001)
  #TODO: add expectation that shows that forecasting worked.
  unlink(scen, recursive = TRUE) # clean up
})
