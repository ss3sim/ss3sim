context("tests functions for sampling composition data")

d <- system.file("extdata", package = "ss3sim")
exp_dat_path <- file.path(d, "example-om", "ss3_exp_vals_comps.dat")

#values to use in tests
exp_dat <- SS_readdat(exp_dat_path, verbose  = FALSE)
# Note: these variable should already be in the exp_dat.
lcomp <- list(fleets = unique(exp_dat$lencomp$FltSvy)[1],
              Nsamp = list(100),
              years = list(exp_dat$lencomp$Yr),
              cpar = NA)

acomp <-  list(fleets = unique(exp_dat$agecomp$FltSvy)[1],
               Nsamp = list(100),
               years = list(exp_dat$agecomp$Yr),
               cpar = NA)

test_that("sample_lcomp runs and changes values" ,{
  set.seed(123)
  new_dat <- sample_lcomp(dat_list = exp_dat,
                          outfile          = NULL,
                          fleets           = lcomp$fleets,
                          Nsamp            = lcomp$Nsamp,
                          years            = lcomp$years,
                          cpar             = lcomp$cpar,
                          ESS              = NULL)
  expect_equal(exp_dat$lencomp$Yr, new_dat$lencomp$Yr)
  expect_equal(dim(exp_dat$lencomp), dim(new_dat$lencomp))
  expect_false(any(exp_dat$lencomp[ ,7:ncol(exp_dat$lencomp)] ==
                   new_dat$lencomp[ ,7:ncol(new_dat$lencomp)]))
  new_dat_d <- sample_lcomp(dat_list = exp_dat,
                           outfile          = NULL,
                           fleets           = lcomp$fleets,
                           Nsamp            = lcomp$Nsamp,
                           years            = lcomp$years,
                           cpar             = 2,
                           ESS              = NULL)
  expect_equal(exp_dat$lencomp$Yr, new_dat_d$lencomp$Yr)
  expect_equal(dim(exp_dat$lencomp), dim(new_dat_d$lencomp))
  expect_false(any(exp_dat$lencomp[ ,7:ncol(exp_dat$lencomp)] ==
                     new_dat_d$lencomp[ ,7:ncol(new_dat_d$lencomp)]))
})

test_that("sample_lcomp gives errors as expected", {
  # bad input (incorrect dimensions)
  set.seed(123)
  expect_error(new_dat <- sample_lcomp(dat_list = exp_dat,
                          outfile          = NULL,
                          fleets           = c(1,2),
                          Nsamp            = lcomp$Nsamp,
                          years            = rep(lcomp$years, 3),
                          cpar             = lcomp$cpar,
                          ESS              = NULL),
               "Need >=1 year per fleet in years")
  expect_error(new_dat <- sample_lcomp(dat_list = exp_dat,
                                       outfile          = NULL,
                                       fleets           = lcomp$fleets,
                                       Nsamp            = 100:101,
                                       years            = lcomp$years,
                                       cpar             = lcomp$cpar,
                                       ESS              = NULL),
               "Nsamp needs to be the same length as fleets")
  expect_error(new_dat <- sample_lcomp(dat_list = exp_dat,
                                       outfile          = NULL,
                                       fleets           = lcomp$fleets,
                                       Nsamp            = lcomp$Nsamp,
                                       years            = c(50,51),
                                       cpar             = lcomp$cpar,
                                       ESS              = NULL),
               "Need >=1 year per fleet in years")

})

test_that("sample_agecomp runs and changes values" ,{
  set.seed(123)
  new_dat <- sample_agecomp(dat_list   = exp_dat,
                            outfile    = NULL,
                            fleets     = acomp$fleets,
                            Nsamp      = acomp$Nsamp,
                            years      = acomp$years,
                            cpar       = acomp$cpar,
                            ESS        = NULL)
  expect_equal(exp_dat$agecomp$Yr, new_dat$agecomp$Yr)
  expect_equal(dim(exp_dat$agecomp), dim(new_dat$agecomp))
  expect_false(any(exp_dat$agecomp[ ,10:ncol(exp_dat$agecomp)] ==
                  new_dat$agecomp[ ,10:ncol(new_dat$agecomp)]))
  # dirchlet
  new_dat_d <- sample_agecomp(dat_list   = exp_dat,
                            outfile    = NULL,
                            fleets     = acomp$fleets,
                            Nsamp      = acomp$Nsamp,
                            years      = acomp$years,
                            cpar       = 2,
                            ESS        = NULL)
  expect_equal(exp_dat$agecomp$Yr, new_dat_d$agecomp$Yr)
  expect_equal(dim(exp_dat$agecomp), dim(new_dat_d$agecomp))
  expect_false(any(exp_dat$agecomp[ ,10:ncol(exp_dat$agecomp)] ==
                     new_dat_d$agecomp[ ,10:ncol(new_dat_d$agecomp)]))
})

test_that("sample_comp produces less variable results with higher Nsamp" ,{
  set.seed(2)
  test <- ss3sim:::sample_comp(exp_dat$agecomp,
    fleets = unique(exp_dat$agecomp$FltSvy),
    Nsamp = 2, years = list(exp_dat$agecomp$Yr))
  expect_true(mean(apply(test[, -(1:9)], 2, var)) > 0.015)
  test <- ss3sim:::sample_comp(exp_dat$agecomp,
    fleets = unique(exp_dat$agecomp$FltSvy),
    Nsamp = 1000, years = list(exp_dat$agecomp$Yr))
  expect_true(mean(apply(test[, -(1:9)], 2, var)) < 0.005)
})

test_that("sample_comp produces less variable compositions with lower cpar" ,{
  set.seed(2)
  test <- ss3sim:::sample_comp(exp_dat$agecomp,
    fleets = unique(exp_dat$agecomp$FltSvy),
    Nsamp = 1000, years = list(exp_dat$agecomp$Yr), cpar = 1)
  test2 <- ss3sim:::sample_comp(exp_dat$agecomp,
    fleets = unique(exp_dat$agecomp$FltSvy),
    Nsamp = 1000, years = list(exp_dat$agecomp$Yr), cpar = 6)
  expect_true(mean(apply(test[, -(1:9)], 1, var)) <
    mean(apply(test2[, -(1:9)], 1, var)))
})

test_that("sample_calcomp works", {
  #TODO: write an expectation to ensure that sampling is being done correctly
  # (this could be a regression test)
  set.seed(2)
  # add calcomp expected values to the agecomp dataset.
  calcomp <-  exp_dat$agecomp
  len_bins <- colnames(exp_dat$lencomp)[7:ncol(exp_dat$lencomp)]
  len_bins <- unlist(lapply(strsplit(len_bins, split = "l"),
                                function(x) x[2]))
  age_colnames <- colnames(calcomp)[10:ncol(calcomp)]
  #Add CAL for 96
  calcomp_96 <- data.frame(Yr = 96, Seas = 1, FltSvy = 1, Gender = 0, Part = 0,
                           Ageerr = 1, Lbin_lo = len_bins,
                           Lbin_hi = len_bins, Nsamp = 5)
  calcomp_96 <- cbind(calcomp_96,
                          data.frame(matrix(1, ncol = ncol(calcomp) - 9,
                                            nrow = NROW(calcomp_96))))
  colnames(calcomp_96)[10:ncol(calcomp_96)] <- age_colnames
  calcomp <- calcomp[calcomp$Yr != 96, ]
  calcomp <- rbind(calcomp, calcomp_96)
  exp_dat$agecomp <- calcomp
  exp_dat$lencomp$Nsamp <- 50
  test <- sample_calcomp(exp_dat,
                         exp_dat,
                         fleets = c(1),
                         years = list(96),
                         Nsamp_lengths = list(50),
                         Nsamp_ages = list(10),
                         ESS_lengths = NULL,
                         ESS_ages = NULL)
expect_error(sample_calcomp(exp_dat,
                            exp_dat,
                            fleets = c(1),
                            years = list(96),
                #expect error b/c Nsamp_lengths is too low for the Nsamp_ages
                            Nsamp_lengths = list(10),
                            Nsamp_ages = list(20)),
             "More age samples specified than fish collected for", fixed = TRUE)
# expect error b/c there is no fleet 4.
expect_error(sample_calcomp(dat_list = exp_dat,
                            exp_vals_list = exp_dat,
                            fleets = c(4),
                            years = list(96),
                            Nsamp_lengths = list(10),
                            Nsamp_ages = list(20)),
             "does not match input file", fixed = TRUE)
#expect error b/c wrong years
expect_error(sample_calcomp(dat_list = exp_dat,
                            exp_vals_list = exp_dat,
                            fleets = c(1),
                            years = list(150),
                            Nsamp_lengths = list(10),
                            Nsamp_ages = list(5)),
            "A year specified in years was not found in the input file for fleet",
            fixed = TRUE)
# Make sure when fleet is NULL returns no CAL data.
dat <- sample_calcomp(dat_list = exp_dat,
                      exp_vals_list = exp_dat,
                       fleets = NULL,
                       years = NULL,
                       Nsamp_lengths = NULL,
                       Nsamp_ages = NULL)
# should be no CAL data, but the marginal age comps should be left untouched.
expect_equivalent(NROW(dat$agecomp[dat$agecomp$Lbin_lo != -1, ]), 0)
expect_equivalent(exp_dat$agecomp[exp_dat$agecomp$Lbin_lo == -1, ],
                  dat$agecomp)

# try using different bins where Lbin_lo and Lbin_hi are not equal. Note that
# this cannot be done the way the sample_calcomp function is currently written.
cal_yr <- 96
test_dif_bins <- data.frame(Yr = cal_yr, Seas = 1, FltSvy = 1, Gender = 0,
                             Part = 0, Ageerr = 1,
                             Lbin_lo = len_bins[seq(1, length(len_bins), by = 2)],
                             Lbin_hi = len_bins[c(seq(2, length(len_bins), by = 2),
                                                  length(len_bins))],
                             Nsamp = 5)
test_dif_bins <- cbind(test_dif_bins, data.frame(matrix(1, ncol = ncol(calcomp) - 9,
                    nrow = NROW(test_dif_bins))))
colnames(test_dif_bins)[10:ncol(test_dif_bins)] <- age_colnames
calcomp <- calcomp[calcomp$Yr != cal_yr, ]
calcomp <- rbind(calcomp, test_dif_bins)
dat_diff_bins <- exp_dat
dat_diff_bins$agecomp <- calcomp
expect_error(sample_calcomp(dat_list = dat_diff_bins,
                            exp_vals_list = dat_diff_bins,
                      fleets = c(1),
                      years = list(cal_yr),
                      Nsamp_lengths = list(10),
                      Nsamp_ages = list(5)),
           "In order to use sample_calcomp, for each row of conditional age at",
            fixed = TRUE)
})
