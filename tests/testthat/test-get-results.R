context("get results")
#TODO: rewrite so no need to run model.
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

test_that("get_results_iter() works", {
  skip_on_cran()
  suppressWarnings(run_ss3sim(iterations = 1, scenarios = "D0-F0-cod",
                              case_folder = case_folder, om_dir = om, em_dir = em))
  res <- get_results_iter(file.path(temp_path, "D0-F0-cod", "1"))
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
   dir.create(file.path("D0-F0-cod", "1", "em_2"))
   file.copy(list.files(file.path("D0-F0-cod", "1", "em"), recursive = TRUE, full.names = TRUE),
             file.path("D0-F0-cod", "1", "em_2"), recursive = TRUE)
   res <- get_results_iter(file.path(temp_path, "D0-F0-cod", "1"))
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
  skip_on_cran()
  res <- get_results_all()
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
  expect_true(all(c("ID", "D", "F", "species") %in% colnames(scalar_wide)))
  expect_true(nrow(scalar_wide) == 1)
  expect_true(ncol(ts) < ncol(ts_wide))
  expect_true(nrow(ts_wide) == 101)
  expect_true(all(c("ID", "D", "F", "species") %in% colnames(ts_wide)))
})


test_that("get_results_all() doesn't overwrite files if overwrite_files = FALSE", {
  skip_on_cran()

  unlink("ss3sim_scalar.csv")
  write.csv(1, file = "ss3sim_scalar.csv", row.names = FALSE) # fake data, 1 column
  expect_warning(get_results_all(overwrite_files = FALSE))
  fake_ss3sim_scalar <- read.csv("ss3sim_scalar.csv") # should be fake data
  expect_identical(1L, ncol(fake_ss3sim_scalar))

  d <- read.csv("D0-F0-cod/results_scalar_D0-F0-cod.csv")
  d$TotYield_MSY <- 3.141592
  write.csv(d, "D0-F0-cod/results_scalar_D0-F0-cod.csv", row.names = FALSE)
  expect_warning(get_results_all(overwrite_files = FALSE))
  d <- read.csv(file.path("D0-F0-cod", "results_scalar_D0-F0-cod.csv"))
  expect_identical(unique(d$TotYield_MSY), 3.141592)
})

test_that("get_results_scenario() doesn't overwrite files if overwrite_files = FALSE", {
  skip_on_cran()
  expect_error(get_results_scenario("D0-F0-cod", overwrite_files = FALSE),
               "Files already exist for")
})

test_that("get_results_scenario() does overwrite files if overwrite_files = TRUE", {
  skip_on_cran()
  write.csv(1, file.path("D0-F0-cod", "results_scalar_D0-F0-cod.csv"))
  get_results_scenario("D0-F0-cod", overwrite_files = TRUE)
  ss3sim_scalar <- read.csv(file.path("D0-F0-cod", "results_scalar_D0-F0-cod.csv")) # should be real data
  expect_true(ncol(ss3sim_scalar) > 1)
})
