context("change_o works")

#set up temp directory
temp_path <- file.path(tempdir(), "ss3sim-test")
dir.create(temp_path, showWarnings = FALSE)
wd <- getwd()
setwd(temp_path)
on.exit(setwd(wd), add = TRUE)

d <- system.file("extdata", package = "ss3sim")
om <- paste0(d, "/models/cod-om")
# copy the ctl file over
file.copy(file.path(om, "codOM.ctl"), file.path("codOM.ctl"))

test_that("change_o works when passing change_o_list", {
  operat_params <- list("SR_LN(R0)" = 16.8) #correct named and length inputs.
  report <- suppressWarnings(change_o(change_o_list = operat_params,
                                      ctl_file_in   = "codOM.ctl",
                                      ctl_file_out  = "codOM_modified.ctl"))
  expect_equal(report$`SR_LN(R0)`, operat_params$`SR_LN(R0)`)
})

test_that("change_o works when passing par_name and par_int",{
  report <- suppressWarnings(change_o(par_name = "SR_LN(R0)", par_int = 16.8,
                                      ctl_file_in   = "codOM.ctl",
                                      ctl_file_out  = "codOM_modified.ctl"))
  expect_equal(report$`SR_LN(R0)`, 16.8)
})

test_that("change_o warns appropriately", {
  operat_params <- list("SX_LN(R0)" = 16.8) #incorrectly spelled parameter name
  suppressWarnings(expect_error(change_o(change_o_list = operat_params,
                                ctl_file_in   = "codOM.ctl",
                                ctl_file_out  = "codOM_modified.ctl")))
  operat_params <- list("SR_LN(R0)" = c(16.8, 17.1)) # too many values supplied
  suppressWarnings(expect_error(change_o(change_o_list = operat_params,
                                ctl_file_in   = "codOM.ctl",
                                ctl_file_out  = "codOM_modified.ctl")))
  # expect error when pasing bad inputs via par_name and par_int:
  suppressWarnings(expect_error(change_o(par_name = c("SR_LN(R0)", "x"), par_int = 16.8,
                                      ctl_file_in   = "codOM.ctl",
                                      ctl_file_out  = "codOM_modified.ctl")))
  suppressWarnings(expect_error(change_o(par_name = "SR_LN(R0)", par_int = c(16.8, 1),
                                         ctl_file_in   = "codOM.ctl",
                                         ctl_file_out  = "codOM_modified.ctl")))
  suppressWarnings(expect_error(change_o(par_name = "SR_LN(R0)", par_int = NULL,
                                         ctl_file_in   = "codOM.ctl",
                                         ctl_file_out  = "codOM_modified.ctl")))
  suppressWarnings(expect_error(change_o(par_name = NULL, par_int = 16.8,
                                         ctl_file_in   = "codOM.ctl",
                                         ctl_file_out  = "codOM_modified.ctl")))
})
