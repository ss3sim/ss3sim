context("get results")
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
simple <- tail(dir(system.file("extdata", package = "r4ss"),
  full.names = TRUE), 1)

dir.create(file.path("scenario", "1", "om"), recursive = TRUE)
dir.create(file.path("scenario", "1", "em"), recursive = TRUE)
ignore <- file.copy(dir(simple, full.names = TRUE), file.path("scenario", "1", "om"))
ignore <- file.copy(dir(simple, full.names = TRUE), file.path("scenario", "1", "em"))

test_that("get_results_iter() works", {
  res <- get_results_iter(file.path("scenario", "1"))
  expect_length(res, 3)
  expect_identical(names(res), c("scalar", "timeseries", "derived"))
  expect_true(NROW(res$scalar) == 2) # 1 for OM, 1 for EM
  expect_true(all(c("model_run", "iteration") %in% colnames(res$scalar))) # b/c need to distinguish runs
  expect_true(all(unique(res$scalar$model_run) == c("em", "om")))
  expect_true(all(c("year", "model_run", "iteration") %in% colnames(res$timeseries)))
  expect_true(all(unique(res$ts$model_run) == c("em", "om")))
  expect_true(all(c("year", "model_run", "iteration") %in% colnames(res$derived)))
  expect_true(all(unique(res$dq$model_run) == c("em", "om")))
})

test_that("get_results_iter() works with 3 models", {
   dir.create(file.path("scenario", "1", "em_2"))
   ignore <- file.copy(list.files(file.path("scenario", "1", "em"),
     recursive = TRUE, full.names = TRUE),
     file.path("scenario", "1", "em_2"), recursive = TRUE)
   res <- get_results_iter(file.path(temp_path, "scenario", "1"))
   expect_length(res, 3)
   expect_identical(names(res), c("scalar", "timeseries", "derived"))
   expect_true(NROW(res$scalar) == 3) # for em, em_2, and om
   expect_true(all(c("model_run", "iteration") %in% colnames(res$scalar))) # b/c need to distinguish runs
   expect_true(all(unique(res$scalar$model_run) == c("em","em_2", "om")))
   expect_true(all(c("year", "model_run", "iteration") %in% colnames(res$timeseries)))
   expect_true(all(unique(res$ts$model_run) == c("em","em_2","om")))
   expect_true(all(c("year", "model_run", "iteration") %in% colnames(res$derived)))
   expect_true(all(unique(res$dq$model_run) == c("em","em_2","om")))
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

test_that("convert_to_wide() works", {
  scalar <-  read.csv("ss3sim_scalar.csv")
  scalar <- scalar[scalar$model_run %in% c("om", "em"), ]
  scalar_wide <- convert_to_wide(scalar)
  ts <- read.csv("ss3sim_ts.csv")
  ts <- ts[ts$model_run %in% c("om", "em"), ]
  ts_wide <- convert_to_wide(ts)
  expect_true(ncol(scalar) < ncol(scalar_wide))
  expect_true(all(c("ID", "species") %in% colnames(scalar_wide)))
  expect_true(nrow(scalar_wide) == 1)
  expect_true(ncol(ts) < ncol(ts_wide))
  expect_true(nrow(ts_wide) == length(unique(ts$year)))
  expect_true(all(c("ID", "species") %in% colnames(ts_wide)))
  expect_equal("scenario", unique(c(scalar_wide$species, ts_wide$species)))
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
  expect_error(get_results_scenario("scenario", overwrite_files = FALSE),
               "Files already exist for")
})

test_that("get_results_scenario() does overwrite files if overwrite_files = TRUE", {
  write.csv(1, file.path("scenario", "results_scalar_scenario.csv"))
  get_results_scenario("scenario", overwrite_files = TRUE)
  ss3sim_scalar <- read.csv(file.path("scenario", "results_scalar_scenario.csv")) # should be real data
  expect_true(ncol(ss3sim_scalar) > 1)
})
