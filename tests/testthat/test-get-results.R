temp_path <- file.path(tempdir(), "ss3sim-test")
dir.create(temp_path, showWarnings = FALSE)
wd <- getwd()
setwd(temp_path)
on.exit(setwd(wd), add = TRUE)
on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)

d <- system.file("extdata", package = "ss3sim")
om <- file.path(d, "models", "cod-om")
em <- file.path(d, "models", "cod-em")
simple <- system.file("extdata", "simple_small", package = "r4ss")

dir.create(file.path("scenario", "1", "om"), recursive = TRUE)
dir.create(file.path("scenario", "1", "em"), recursive = TRUE)
file.copy(dir(simple, full.names = TRUE), file.path("scenario", "1", "om"))
file.copy(dir(simple, full.names = TRUE), file.path("scenario", "1", "em"))

test_that("get_scenarios() finds correct folder", {
  expect_true(get_scenarios(getwd()) == "scenario")
  expect_true(basename(get_scenarios("..")) == "scenario")
  expect_error(get_scenarios("test"))
})

test_that("get_results_all() works", {
  res <- get_results_all(user_scenarios = "scenario")
  expect_true(file.exists("ss3sim_scalar.csv"))
  expect_true(file.exists("ss3sim_ts.csv"))
  expect_length(res, 3)
  expect_identical(names(res), c("scalar", "ts", "dq"))
  # make sure scenario is included in a column for each output. need to
  # to distinguish between scenarios in all results.
  expect_true("scenario" %in% colnames(res$scalar))
  expect_true("scenario" %in% colnames(res$ts))
  expect_true("scenario" %in% colnames(res$dq))
  expect_true(all(c("model_run", "iteration") %in% colnames(res$scalar)))
  expect_true(all(c("year", "model_run", "iteration") %in% colnames(res$ts)))
  expect_true(all(c("year", "model_run", "iteration") %in% colnames(res$dq)))
})

test_that("get_results_all() doesn't overwrite files if overwrite_files = FALSE", {
  unlink("ss3sim_scalar.csv")
  write.csv(1, file = "ss3sim_scalar.csv", row.names = FALSE) # fake data, 1 column
  expect_warning(get_results_all(user_scenarios = "scenario", overwrite_files = FALSE))
  fake_ss3sim_scalar <- read.csv("ss3sim_scalar.csv") # should be fake data
  expect_identical(1L, ncol(fake_ss3sim_scalar))

  d <- read.csv("scenario/results_scalar_scenario.csv")
  d$TotYield_MSY <- 3.141592
  write.csv(d, "scenario/results_scalar_scenario.csv", row.names = FALSE)
  expect_warning(get_results_all(user_scenarios = "scenario", overwrite_files = FALSE))
  d <- read.csv(file.path("scenario", "results_scalar_scenario.csv"))
  expect_identical(unique(d$TotYield_MSY), 3.141592)
})

test_that("get_results_scenario() doesn't overwrite files if overwrite_files = FALSE", {
  expect_error(
    get_results_scenario("scenario", overwrite_files = FALSE),
    "Files already exist for"
  )
})

test_that("get_results_scenario() does overwrite files if overwrite_files = TRUE", {
  write.csv(1, file.path("scenario", "results_scalar_scenario.csv"))
  get_results_scenario("scenario", overwrite_files = TRUE)
  ss3sim_scalar <- read.csv(file.path("scenario", "results_scalar_scenario.csv")) # should be real data
  expect_true(ncol(ss3sim_scalar) > 1)
})

test_that("get_results_all() works when some report files missing", {
  # get rid of old csv files
  all_files <- list.files(recursive = TRUE)
  to_rm <- grep("csv$", all_files, value = TRUE)
  file.remove(to_rm)
  # get rid ofreport files for the EMS
  file.remove("scenario/1/em/Report.sso")
  file.remove("scenario/1/em_2/Report.sso")
  # if in doubt, double check the commented out line returns "scenario/1/om/Report.sso"
  # ONLY.
  # grep("/Report\\.sso$", list.files(recursive = TRUE), value = TRUE)
  return <- get_results_all(user_scenarios = "scenario")
  expect_length(unique(return$scalar$model_run), 1)
  expect_true(unique(return$scalar$model_run) == "om")
  expect_true(unique(return$ts$model_run) == "om")
  expect_length(unique(return$ts$model_run), 1)
  expect_true(unique(return$dq$model_run) == "om")
  expect_length(unique(return$dq$model_run), 1)
})

test_that("get_results_all() works when all report files missing for 1 scenario", {
  # get rid of old csv files
  all_files <- list.files(recursive = TRUE)
  to_rm <- grep("csv$", all_files, value = TRUE)
  file.remove(to_rm)
  dir.create("scenario_2")
  file.copy(from = list.dirs("scenario", recursive = FALSE), to = "scenario_2", recursive = TRUE)
  file.remove("scenario_2/1/om/Report.sso")
  # if in doubt, double check the commented out line returns "scenario/1/om/Report.sso"
  # ONLY.
  # grep("/Report\\.sso$", list.files(recursive = TRUE), value = TRUE)
  return <- get_results_all(user_scenarios = c("scenario_2", "scenario"))
  expect_type(return, "list")
  expect_length(return, 3)
  expect_true(all(return$ts$scenario == "scenario"))
  expect_true(all(return$scalar$scenario == "scenario"))
  expect_true(all(return$dq$scenario == "scenario"))
})

test_that("get_results_all() works when 1 iteration of report files missing", {
  # get rid of old csv files
  all_files <- list.files(recursive = TRUE)
  to_rm <- grep("csv$", all_files, value = TRUE)
  file.remove(to_rm)
  dir.create("scenario/2")
  file.copy(from = list.files("scenario/1", recursive = F, include.dirs = T, full.names = T), to = "scenario/2", recursive = T)
  # get rid ofreport files for the EMS
  file.remove("scenario/1/om/Report.sso")
  # if in doubt, double check the commented out line returns "scenario/1/om/Report.sso"
  # ONLY.
  # grep("/Report\\.sso$", list.files(recursive = TRUE), value = TRUE)
  return <- get_results_all(user_scenarios = "scenario")
  expect_type(return, "list")
  expect_length(return, 3)
  expect_true(all(return$ts$iteration == "2"))
  expect_true(all(return$scalar$iteration == "2"))
  expect_true(all(return$dq$iteration == "2"))
})

test_that("get_results_all() works when all report files missing", {
  # get rid of old csv files
  all_files <- list.files(recursive = TRUE)
  to_rm <- grep("csv$", all_files, value = TRUE)
  file.remove(to_rm)
  # get rid ofreport files for the EMS
  file.remove("scenario/2/om/Report.sso")
  # if in doubt, double check the commented out line returns "scenario/1/om/Report.sso"
  # ONLY.
  # grep("/Report\\.sso$", list.files(recursive = TRUE), value = TRUE)
  return <- get_results_all(user_scenarios = "scenario")
  expect_type(return, "list")
  expect_length(return, 3)
  expect_true(is.null(return$ts))
  expect_true(is.null(return$scalar))
  expect_true(is.null(return$dq))
})

test_that("get_results_all() works when all report files missing for 2 scenarios", {
  # get rid of old csv files
  all_files <- list.files(recursive = TRUE)
  to_rm <- grep("csv$", all_files, value = TRUE)
  file.remove(to_rm)
  # if in doubt, double check the commented out line returns "scenario/1/om/Report.sso"
  # ONLY.
  # grep("/Report\\.sso$", list.files(recursive = TRUE), value = TRUE)
  return <- get_results_all(user_scenarios = c("scenario_2", "scenario"))
  expect_type(return, "list")
  expect_length(return, 3)
  expect_true(is.null(return$ts))
  expect_true(is.null(return$scalar))
  expect_true(is.null(return$dq))
})
