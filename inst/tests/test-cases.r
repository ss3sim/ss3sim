context("Case parsing")

test_that("Single digit cases get picked up", {
  expect_equal(get_caseval("M1-D2", "M"), 1L)
  expect_equal(get_caseval("M2-D2", "M"), 2L)
})

test_that("Double and triple digit cases get picked up", {
  expect_equal(get_caseval("M10-D2", "M"), 10L)
  expect_equal(get_caseval("M100-D2", "M"), 100L)
})

test_that("Delimiters error", {
  expect_error(get_caseval("M1D2", "M"))
})

test_that("No time varying OK", {
    d <- system.file("extdata", package = "ss3sim")
    d <- paste0(d, "/time-varying-case-tests")
    out <- get_caseargs(folder = d, scenario = "Z0-K0-cod", case_vals
      = c("Z", "K"), case_files = c("Z", "K"))
    expect_equal(out$time_varying, NULL)
})

test_that("Time varying parsed", {
    out <- get_caseargs(folder = d, scenario = "X0-Y0-Z0-cod",
      case_vals = c("X", "Y", "Z"), case_files = c("X", "Y", "Z"))
    expect_equal(length(out$time_varying), 2)
    expect_equal(names(out$time_varying)[1], "NatM_p_1_Fem_GP_1")
})

