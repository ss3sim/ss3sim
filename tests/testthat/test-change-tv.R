context("Function change_tv() works")

temp_path <- file.path(tempdir(), "ss3sim-test")
dir.create(temp_path, showWarnings = FALSE)
wd <- getwd()
setwd(temp_path)

d <- system.file("extdata", package = "ss3sim")
om <- paste0(d, "/models/cod-om")
em <- paste0(d, "/models/cod-em")
case_folder <- paste0(d, "/eg-cases")
a <- get_caseargs(folder = paste0(d, "/eg-cases"),
                  case_files = list(F = "F", D = c("index", "lcomp", "agecomp"),
                                    M = "M", E = "E"),
                  scenario = "F1-D0-M1-E0-cod")

test_that("change_tv() works for a MG param.", {
  out <- suppressWarnings(change_tv(a$tv_params,
                   ctl_file_in = file.path(om, "codOM.ctl"), ctl_file_out = NULL,
                   dat_file_in = file.path(om, "codOM.dat"), dat_file_out = NULL))

  expect_equal(out$dat_out$envdat$Value, a$tv_params$NatM_p_1_Fem_GP_1)
  expect_length(grep("0.01 1.8 0.2 0.1 0.8 0 -3 201 0 0 0 0 0 0 # NatM_p_1_Fem_GP_1",
                     out$ctl_out), 1)
  expect_length(grep("-2 2 1 0 99 0 -5 # TV_par_line for MG parameters" ,
                   out$ctl_out), 1)
})

test_that("change_tv() works for an MG param and an SR param", {
  # add SR param.
  a$tv_params$SR_BH_steep <- seq(0.05, 0.01, length.out = 100)
  out <- suppressWarnings(change_tv(a$tv_params,
                   ctl_file_in = file.path(om, "codOM.ctl"), ctl_file_out = NULL,
                   dat_file_in = file.path(om, "codOM.dat"), dat_file_out = NULL))
  expect_equal(out$dat_out$envdat$Value,
               c(a$tv_params$NatM_p_1_Fem_GP_1, a$tv_params$SR_BH_steep))
  expect_length(grep("0.01 1.8 0.2 0.1 0.8 0 -3 201 0 0 0 0 0 0 # NatM_p_1_Fem_GP_1",
                     out$ctl_out), 1)
  expect_length(grep("-2 2 1 0 99 0 -5 # TV_par_line for MG parameters" ,
                     out$ctl_out), 1)
  expect_length(grep("0.2 1 0.65 0.7 0.05 0 -4 202 0 0 0 0 0 0 # SR_BH_steep",
                     out$ctl_out), 1)
  expect_length(grep("-2 2 1 0 99 0 -5 # TV_par_line for SR parameters" ,
                     out$ctl_out), 1)
})

test_that("change_tv() works for an SR param", {
  # add SR param.
  a$tv_params <- NULL
  a$tv_params$SR_BH_steep <- seq(0.05, 0.01, length.out = 100)
  out <- suppressWarnings(change_tv(a$tv_params,
                   ctl_file_in = file.path(om, "codOM.ctl"), ctl_file_out = NULL,
                   dat_file_in = file.path(om, "codOM.dat"), dat_file_out = NULL))
  expect_equal(out$dat_out$envdat$Value, a$tv_params$SR_BH_steep)
  expect_length(grep("0.2 1 0.65 0.7 0.05 0 -4 201 0 0 0 0 0 0 # SR_BH_steep",
                     out$ctl_out, fixed = FALSE), 1)
  expect_length(grep("-2 2 1 0 99 0 -5 # TV_par_line for SR parameters" ,
                     out$ctl_out), 1)
})

test_that("change_tv() works for a q parm", {
  # add SR param.
  a$tv_params <- NULL
  a$tv_params$`LnQ_base_Survey(2)` <- seq(0.01, 0.05, length.out = 100)
  out <- suppressWarnings(change_tv(a$tv_params,
                   ctl_file_in = file.path(om, "codOM.ctl"), ctl_file_out = NULL,
                   dat_file_in = file.path(om, "codOM.dat"), dat_file_out = NULL))
  expect_equal(out$dat_out$envdat$Value, a$tv_params$`LnQ_base_Survey(2)`)
  expect_length(grep("-3 3 0 0 99 0 -5 201 0 0 0 0 0 0 # LnQ_base_Survey(2)",
                     out$ctl_out, fixed = TRUE), 1)
  expect_length(grep("-2 2 1 0 99 0 -5 # TV_par_line for Q parameters" ,
                     out$ctl_out), 1)
})

test_that("change_tv() works for a selectivity parm", {
  # add SR param.
  a$tv_params <- NULL
  a$tv_params$`SizeSel_P1_Fishery(1)` <- seq(0.01, 0.05, length.out = 100)

  out <- suppressWarnings(change_tv(a$tv_params,
                           ctl_file_in = file.path(om, "codOM.ctl"), ctl_file_out = NULL,
                           dat_file_in = file.path(om, "codOM.dat"), dat_file_out = NULL))
  expect_equal(out$dat_out$envdat$Value, a$tv_params$`SizeSel_P1_Fishery(1)`)
  expect_length(grep("20 199 50.8 50.8 0.05 0 2 201 0 0 0 0.5 0 0 # SizeSel_P1_Fishery(1)",
                     out$ctl_out, fixed = TRUE), 1)
  expect_length(grep("-2 2 1 0 99 0 -5 # TV_par_line for selex parameters" ,
                     out$ctl_out), 1)
})

test_that("change_tv() works when there are 2 MG parms", {
  a$tv_params <- NULL
  a$tv_params$NatM_p_1_Fem_GP_1 <- seq(0.01, 0.05, length.out = 100)
  a$tv_params$VonBert_K_Fem_GP_1 <- seq(0.02, 0.06, length.out = 100)
  out <- suppressWarnings(change_tv(a$tv_params,
    ctl_file_in = file.path(om, "codOM.ctl"), ctl_file_out = NULL,
    dat_file_in = file.path(om, "codOM.dat"), dat_file_out = NULL))
  expect_equal(out$dat_out$envdat$Value,
               c(a$tv_params$NatM_p_1_Fem_GP_1, a$tv_params$VonBert_K_Fem_GP_1))
  expect_length(grep("0.01 1.8 0.2 0.1 0.8 0 -3 201 0 0 0 0 0 0 # NatM_p_1_Fem_GP_1",
                     out$ctl_out, fixed = TRUE), 1)
  expect_length(grep("0.01 2 0.2 0.25 0.8 0 -2 202 0 0 0 0 0 0 # VonBert_K_Fem_GP_1" ,
                     out$ctl_out), 1)
  expect_length(grep("-2 2 1 0 99 0 -5 # TV_par_line for MG parameters" ,
                     out$ctl_out), 2)
})
