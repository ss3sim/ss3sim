context("sample_index() is working")

temp_path <- file.path(tempdir(), "ss3sim-test")
dir.create(temp_path, showWarnings = FALSE)
wd <- getwd()
setwd(temp_path)
on.exit(setwd(wd), add = TRUE)

d <- system.file("extdata", package = "ss3sim")
a <- list("index" = list("fleets" = 2,
  "years" = list(seq(76, 100, by = 2)),
  "sds_obs" = list(0.2)))

exp_vals <- r4ss::SS_readdat(file.path(d, "example-om", "ss3_expected_values.dat"),
                       version = NULL, verbose = FALSE)

test_that("sample_index() works for 1 fleet", {
set.seed(123)
sampled_index <- with(a$index,
                      sample_index(dat_list        = exp_vals,
                                   outfile         = NULL,
                                   fleets          = fleets,
                                   years           = years,
                                   seas = list(unique(
                                     exp_vals[["CPUE"]][, "seas"]
                                   )),
                                   sds_obs         = sds_obs))
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
  exp(rnorm(length(exp_vals$CPUE$obs),
            mean = 0,
            sd = a$index$sds_obs[[1]]) - a$index$sds_obs[[1]]^2 / 2)
expect_equal(sampled_index$CPUE$obs,samples)
})
