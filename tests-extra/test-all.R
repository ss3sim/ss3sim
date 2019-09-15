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
    om_dir = om, em_dir = em, seed = 1, ...)

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


# ```
# > devtools::load_all()
# > setwd("tests")
# > source("test-all.R")
#
# Error:
#   Component “NLL_TOTAL_om”: Mean relative difference: 0.2374637
# Component “NLL_Length_comp_om”: Mean relative difference: 64.40874
# Component “NLL_Age_comp_om”: Mean relative difference: 82.59101
#
# > sc <- read.csv("ss3sim_scalar.csv")
# > sc.old <- readRDS("expectations/D0-E1-F0-R0-M1-cod-scalar.rds")
# > rbind(sc, sc.old)
# X SSB_MSY_om TotYield_MSY_om SSB_Unfished_om depletion_om params_on_bound_om
# 1 1 2101810000       242709000      4084470000    0.5279975                  0
# 2 1 2101810000       242709000      4084470000    0.5279975                  0
# NatM_p_1_Fem_GP_1_om L_at_Amin_Fem_GP_1_om L_at_Amax_Fem_GP_1_om
# 1                  0.2                    20                   132
# 2                  0.2                    20                   132
# VonBert_K_Fem_GP_1_om CV_young_Fem_GP_1_om CV_old_Fem_GP_1_om Wtlen_1_Fem_om
# 1                   0.2                  0.1                0.1        6.8e-06
# 2                   0.2                  0.1                0.1        6.8e-06
# Wtlen_2_Fem_om Mat50._Fem_om Mat_slope_Fem_om Eggs.kg_inter_Fem_om
# 1          3.101         38.18           -0.276                    1
# 2          3.101         38.18           -0.276                    1
# Eggs.kg_slope_wt_Fem_om RecrDist_GP_1_om RecrDist_Area_1_om RecrDist_Seas_1_om
# 1                       0                0                  0                  0
# 2                       0                0                  0                  0
# CohortGrowDev_om NatM_p_1_Fem_GP_1_ENV_add_om SR_LN_R0_om SR_BH_steep_om
# 1                1                            1        18.7           0.65
# 2                1                            1        18.7           0.65
# SR_sigmaR_om SR_envlink_om SR_R1_offset_om SR_autocorr_om InitF_1Fishery_om
# 1          0.4             0               0              0                 0
# 2          0.4             0               0              0                 0
# LnQ_base_1_Fishery_om LnQ_base_2_Survey_om LnQ_base_3_CPUE_om
# 1                     0                    0                  0
# 2                     0                    0                  0
# SizeSel_1P_1_Fishery_om SizeSel_1P_2_Fishery_om SizeSel_2P_1_Survey_om
# 1                   38.18                   10.63                  30.54
# 2                   38.18                   10.63                  30.54
# SizeSel_2P_2_Survey_om Catch_endyear_om NLL_TOTAL_om NLL_Catch_om NLL_Equil_catch_om
# 1                  10.63        243412000     10939.60      8927.79                  0
# 2                  10.63        243412000      8840.34      8927.79                  0
# NLL_Survey_om NLL_Discard_om NLL_Mean_body_wt_om NLL_Length_comp_om NLL_Age_comp_om
# 1      -156.617             NA                  NA           860.2230       1267.3400
# 2      -156.617             NA                  NA            13.1515         15.1612
# NLL_Size_at_age_om NLL_SizeFreq_om NLL_Morphcomp_om NLL_Tag_comp_om
# 1                 NA              NA               NA              NA
# 2                 NA              NA               NA              NA
# NLL_Tag_negbin_om NLL_Recruitment_om NLL_Forecast_Recruitment_om NLL_Parm_priors_om
# 1                NA            40.6079                           0                  0
# 2                NA            40.6079                           0                  0
# NLL_Parm_softbounds_om NLL_Parm_devs_om NLL_Crash_Pen_om SSB_MSY_em TotYield_MSY_em
# 1                      0                0                0 1768400000       177357000
# 2                      0                0                0 1792560000       179765000
# SSB_Unfished_em depletion_em params_on_bound_em NatM_p_1_Fem_GP_1_em
# 1      5088840000    0.3212264                  0                0.165
# 2      5158320000    0.3716733                  0                0.165
# L_at_Amin_Fem_GP_1_em L_at_Amax_Fem_GP_1_em VonBert_K_Fem_GP_1_em
# 1               19.7733               129.419              0.208867
# 2               19.7704               129.444              0.208835
# CV_young_Fem_GP_1_em CV_old_Fem_GP_1_em Wtlen_1_Fem_em Wtlen_2_Fem_em Mat50._Fem_em
# 1             0.101726           0.107995        6.8e-06          3.101         38.18
# 2             0.101752           0.107898        6.8e-06          3.101         38.18
# Mat_slope_Fem_em Eggs.kg_inter_Fem_em Eggs.kg_slope_wt_Fem_em RecrDist_GP_1_em
# 1           -0.276                    1                       0                0
# 2           -0.276                    1                       0                0
# RecrDist_Area_1_em RecrDist_Seas_1_em CohortGrowDev_em SR_LN_R0_em SR_BH_steep_em
# 1                  0                  0                1     18.5386           0.65
# 2                  0                  0                1     18.5517           0.65
# SR_sigmaR_em SR_envlink_em SR_R1_offset_em SR_autocorr_em ForeRecr_2012_em
# 1          0.4             0               0              0                0
# 2          0.4             0               0              0                0
# Impl_err_2012_em InitF_1Fishery_em LnQ_base_1_Fishery_em LnQ_base_2_Survey_em
# 1                0                 0                     0            0.0577813
# 2                0                 0                     0           -0.0164503
# LnQ_base_3_CPUE_em SizeSel_1P_1_Fishery_em SizeSel_1P_2_Fishery_em
# 1                  0                 38.1638                 10.5571
# 2                  0                 38.1640                 10.5561
# SizeSel_2P_1_Survey_em SizeSel_2P_2_Survey_em Catch_endyear_em NLL_TOTAL_em
# 1                29.6390                9.31084        162923000      2631.87
# 2                29.6684                9.34873        190809000      2626.38
# NLL_Catch_em NLL_Equil_catch_em NLL_Survey_em NLL_Discard_em NLL_Mean_body_wt_em
# 1  1.10185e-13                  0      -29.3723             NA                  NA
# 2  9.16529e-14                  0      -36.3981             NA                  NA
# NLL_Length_comp_em NLL_Age_comp_em NLL_Size_at_age_em NLL_SizeFreq_em
# 1            2007.68         686.084                 NA              NA
# 2            2007.55         686.547                 NA              NA
# NLL_Morphcomp_em NLL_Tag_comp_em NLL_Tag_negbin_em NLL_Recruitment_em
# 1               NA              NA                NA           -32.5286
# 2               NA              NA                NA           -31.3223
# NLL_Forecast_Recruitment_em NLL_Parm_priors_em NLL_Parm_softbounds_em
# 1                           0                  0             0.00613909
# 2                           0                  0             0.00613243
# NLL_Parm_devs_em NLL_Crash_Pen_em bias1 bias2 bias3 bias4 bias5 bias.converged
# 1                0                0    NA    NA    NA    NA    NA             NA
# 2                0                0    NA    NA    NA    NA    NA             NA
# bias.tried           scenario iteration  D  E  F  R  M species    max_grad version
# 1         NA D0-E1-F0-R0-M1-cod         1 D0 E1 F0 R0 M1     cod 0.001306860 #V3.24O
# 2         NA D0-E1-F0-R0-M1-cod         1 D0 E1 F0 R0 M1     cod 0.000848066 #V3.24O
# RunTime hessian                   ID
# 1 0.9166667   FALSE D0-E1-F0-R0-M1-cod-1
# 2 0.8166667   FALSE D0-E1-F0-R0-M1-cod-1
# ```
