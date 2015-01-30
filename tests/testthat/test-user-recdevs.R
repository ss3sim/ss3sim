context("User recdevs are the right dimensions")

test_that("run_ss3sim fails if ncol(user_recdevs) is too small for iterations", {
 
  temp_path <- file.path(tempdir(), "ss3sim-test")
  dir.create(temp_path, showWarnings = FALSE)
  wd <- getwd()
  setwd(temp_path)
  
  d <- system.file("extdata", package = "ss3sim")
  om <- paste0(d, "/models/cod-om")
  em <- paste0(d, "/models/cod-em")
  case_folder <- paste0(d, "/eg-cases")
  
  urd <- matrix(data = 0, ncol = 1, nrow = 100)
  
  # one too many iterations for user_recdevs:
  expect_error(run_ss3sim(iterations = 2, 
    scenarios = "D0-E0-F0-R0-M0-cod", case_folder = case_folder, 
    om_dir = om, em_dir = em, user_recdevs = urd))
})
