context("sample_index() is working")



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
# maybe also read in an data.ss_new file example ?
#exp_vals <- file.path(d, "example-om", data.ss_new)

exp_vals <- SS_readdat(file.path(d, "example-om", "ss3_expected_values.dat"),
                       version = NULL, verbose = FALSE)

test_that("sample_index() works for 1 fleet", {
set.seed(123)
sampled_index <- with(a$index,
                      sample_index(dat_list        = exp_vals,
                                   outfile         = NULL,
                                   fleets          = fleets,
                                   years           = years,
                                   sds_obs         = sds_obs,
                                   write_file      = FALSE))
# check SD filled in correctly
expect_equal(sampled_index$CPUE$se_log,
             rep(a$index$sds_obs[[1]], length.out = nrow(sampled_index$CPUE)))
#check only correct fleets used.
expect_equal(order(unique(sampled_index$CPUE$index)),
             order(unique(a$index$fleets))
             )
# check input values sampled correctly (not generalizable to other cases)
set.seed(123)
samples <- exp_vals$CPUE$obs *
  exp(rnorm(length(exp_vals$CPUE$obs), mean = 0, sd = a$index$sds_obs[[1]]) - a$index$sds_obs[[1]]^2 / 2)
expect_equal(sampled_index$CPUE$obs,samples)
})
