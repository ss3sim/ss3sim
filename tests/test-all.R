# A framework for more thoroughly testing the ss3sim package
#
# Instructions:
# 1. Set your working directory to the 'tests' folder in the ss3sim repository
# 2. load_all("..") the package or build and reload
# 3. Source this file
# 4. Add new tests as test_ss3sim("your-scenario-id") at the end of the file
# 5. If something about the package has changed in a way that you wish to reset
#    your expectations about the output, then set reset_expectations = TRUE
#    when running test_ss3sim(). This will cache fresh expectations as .rds
#    files in the 'expectations' folder.

# A function that may be eventually pulled into the package:
#
#' Test ss3sim with expected results from previous runs
#'
#' This function facilitates rapid testing of expected results from ss3sim
#' simulations. It is intended for developer use and is not exported in the
#' ss3sim namespace. Therefore, it is expected to be accessed using
#' \code{devtools::load_all()}.
#'
#' @param reset_expectations Logical. If FALSE then previous expectations will
#'   be loaded from the \code{ss3sim/tests/expectations} folder. If TRUE then
#'   the new results will be saved as expectations for future runs.
#' @param scenario A single scenario ID to run. Should correspond to models
#'   in the \code{ss3sim/inst/extdata} folder and case files in
#'   \code{ss3sim/tests/cases} folder.
#' @param ... Anything extra to pass to \code{run_ss3sim}.
#' @author Sean C. Anderson
#' @examples
#' \donttest{
#' test_ss3sim("D0-E1-F0-R0-M1-cod")
#' }

test_ss3sim <- function(scenario, reset_expectations = FALSE, ...) {

  library(testthat)
  library(dplyr)

  d <- '../inst/extdata'
  om <- paste0(d, "/models/cod-om")
  em <- paste0(d, "/models/cod-em")
  case_folder <- "cases"

  run_ss3sim(iterations = 1, scenarios = scenario, case_folder = case_folder,
    om_dir = om, em_dir = em, ss_mode = "safe", seed = 1, ...)

  get_results_all(user_scenarios = scenario)
  results_scalar <- read.csv("ss3sim_scalar.csv")
  results_ts <- read.csv("ss3sim_ts.csv")

  if(reset_expectations) {
    expect_scalar <- results_scalar
    expect_ts <- results_ts
    saveRDS(expect_scalar, paste0("expectations/", scenario, "-scalar.rds"))
    saveRDS(expect_ts, paste0("expectations/", scenario, "-ts.rds"))
  } else {
    expect_scalar <- readRDS(paste0("expectations/", scenario, "-scalar.rds"))
    expect_ts <- readRDS(paste0("expectations/", scenario, "-ts.rds"))
  }

  # only match those with identical names:
  # (in case we've added columns to the output)
  m_scalar <- names(expect_scalar)[names(expect_scalar) %in%
      names(results_scalar)]
  m_ts <- names(expect_ts)[names(expect_ts) %in%
      names(results_ts)]
  if(length(m_scalar) != length(names(results_scalar))) {
    warning("It looks like there are new columns in results_scalar")
  }
  if(length(m_ts) != length(names(results_ts))) {
    warning("It looks like there are new columns in results_ts")
  }

  expect_equal(select(results_scalar[,m_scalar], ends_with("om")),
    select(expect_scalar[,m_scalar], ends_with("om")))
  expect_equal(select(results_scalar[,m_scalar], ends_with("em")),
    select(expect_scalar[,m_scalar], ends_with("em")))
  expect_equal(select(results_ts[,m_ts], ends_with("om")),
    select(expect_ts[,m_ts], ends_with("om")))
  expect_equal(select(results_ts[,m_ts], ends_with("em")),
    select(expect_ts[,m_ts], ends_with("em")))

  unlink(scenario, TRUE)
  file.remove("ss3sim_scalar.csv", "ss3sim_ts.csv")

  invisible(list(results_ts = results_ts, results_scalar = results_scalar))
}

# Time varying and estimation:
# - block change in M at year 76
# - fix M during estimation to 130% of M at MSY
test_ss3sim("D0-E1-F0-R0-M1-cod")

# Data sampling: index, lcomp, and agecomp
# Fishing mortality
# Tail compression
# Retrospective
# Soon: binning
# Soon: robustification constant
