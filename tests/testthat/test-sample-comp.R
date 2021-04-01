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
               "Bad input to ss3sim sampling function")
  expect_error(new_dat <- sample_lcomp(dat_list = exp_dat,
                                       outfile          = NULL,
                                       fleets           = lcomp$fleets,
                                       Nsamp            = 100:101,
                                       years            = lcomp$years,
                                       cpar             = lcomp$cpar,
                                       ESS              = NULL),
               "Bad input to ss3sim sampling function")
  expect_error(new_dat <- sample_lcomp(dat_list = exp_dat,
                                       outfile          = NULL,
                                       fleets           = lcomp$fleets,
                                       Nsamp            = lcomp$Nsamp,
                                       years            = c(50,51),
                                       cpar             = lcomp$cpar,
                                       ESS              = NULL),
               "Bad input to ss3sim sampling function")

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
  # Multiple fleets with fleet 2 first, which is not typical
  exp_dat[["agecomp"]][1:10, "FltSvy"] <- 2
  new_dat_d <- sample_agecomp(dat_list = exp_dat,
    outfile    = NULL,
    fleets     = 2:1,
    Nsamp      = list(100, 10),
    years      = list(60:78, 80:100),
    cpar       = 2,
    ESS        = NULL)
  expect_equivalent(
    new_dat_d[["agecomp"]] %>% dplyr::group_by(.data[["FltSvy"]]) %>% 
    dplyr::summarize(dplyr::n()),
    tibble::tibble(1:2, 11:10))
  # Added column that is not standard, e.g., Part
  exp_dat[["agecomp"]][1:10, "Part"] <- 1
  new_dat_d <- sample_agecomp(dat_list = exp_dat,
    outfile    = NULL,
    fleets     = 2:1,
    Nsamp      = list(100, 10),
    years      = list(60:78, 80:100),
    cpar       = 2,
    ESS        = NULL,
    Part = list(1, 0)
    )
  expect_equivalent(
    new_dat_d[["agecomp"]] %>% dplyr::group_by(.data[["Part"]]) %>% 
    dplyr::summarize(dplyr::n()),
    tibble::tibble(0:1, 11:10))
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

  # make 1 more data set with 2 years of calcomp data for test with more complexity
  calcomp_98 <- calcomp_96
  calcomp_98$Yr <- 98
  calcomp_2 <- rbind(calcomp, calcomp_98)
  exp_dat_2 <- exp_dat
  exp_dat_2$agecomp <- calcomp_2

# test a basic case
test <- sample_calcomp(exp_dat,
                       exp_dat,
                       fleets = c(1),
                       years = list(96),
                       Nsamp_lengths = list(50),
                       Nsamp_ages = list(10))
# expectations for length comp
expect_true(nrow(test[["lencomp"]]) == 1)
expect_true(test[["lencomp"]][,"Yr"] == 96)
expect_true(test[["lencomp"]][,"FltSvy"] == 1)
expect_true(test[["lencomp"]][, "Nsamp"] == 50)
# expectations for age comp
tmp_age <- test[["agecomp"]]
tmp_age <- tmp_age[tmp_age$Lbin_lo != -1 & tmp_age$Lbin_hi != - 1, ]
expect_true(all(tmp_age[,"Yr"] %in% 96))
expect_true(all(tmp_age[, "FltSvy"] %in% 1))
expect_true(sum(tmp_age$Nsamp) == 10)

# test a basic case where the marginals are kept
test_keep_marginals <- sample_calcomp(exp_dat,
                       exp_dat,
                       fleets = c(1),
                       years = list(96),
                       Nsamp_lengths = list(50),
                       Nsamp_ages = list(10),
                       lcomps_sampled = TRUE) # keep len comps
# expectations for length comp
expect_true(nrow(test_keep_marginals[["lencomp"]]) > 1)
# expectations for age comp
tmp_age <- test_keep_marginals[["agecomp"]]
tmp_age <- tmp_age[tmp_age$Lbin_lo != -1 & tmp_age$Lbin_hi != - 1, ]
expect_true(all(tmp_age[,"Yr"] %in% 96))
expect_true(all(tmp_age[, "FltSvy"] %in% 1))
expect_true(sum(tmp_age$Nsamp) == 10)


#test a basic case, but change the ESS.
test_add_ess <- sample_calcomp(exp_dat,
                       exp_dat,
                       fleets = c(1),
                       years = list(96),
                       Nsamp_lengths = list(50),
                       Nsamp_ages = list(10),
                       ESS_lengths = list(30),
                       ESS_ages = list(20))
# expectations for length comp
expect_true(nrow(test_add_ess[["lencomp"]]) == 1)
expect_true(test_add_ess[["lencomp"]][,"Yr"] == 96)
expect_true(test_add_ess[["lencomp"]][,"FltSvy"] == 1)
expect_true(test_add_ess[["lencomp"]][, "Nsamp"] == 30)
# expectations for age comp
tmp_age <- test_add_ess[["agecomp"]]
tmp_age <- tmp_age[tmp_age$Lbin_lo != -1 & tmp_age$Lbin_hi != - 1, ]
expect_true(all(tmp_age[,"Yr"] %in% 96))
expect_true(all(tmp_age[, "FltSvy"] %in% 1))
expect_true(sum(tmp_age$Nsamp) == 20)

#test a basic case, but use ESS and keep the len comps
test_add_ess_keep_marg <- sample_calcomp(exp_dat,
                               exp_dat,
                               fleets = c(1),
                               years = list(96),
                               Nsamp_lengths = list(50),
                               Nsamp_ages = list(10),
                               ESS_lengths = list(30),
                               ESS_ages = list(20),
                               lcomps_sampled = TRUE)
# expectations for length comp
expect_true(nrow(test_add_ess_keep_marg[["lencomp"]]) > 1)
expect_true(test_add_ess_keep_marg[["lencomp"]][1, "Nsamp"] == 30)
# the following should hold unless the value in exp_dat for Nsamp is 30 for
# any of the observations
expect_true(all(test_add_ess_keep_marg[["lencomp"]][-1, "Nsamp"] != 30))
# expectations for age comp
tmp_age <- test_add_ess_keep_marg[["agecomp"]]
tmp_age <- tmp_age[tmp_age$Lbin_lo != -1 & tmp_age$Lbin_hi != - 1, ]
expect_true(all(tmp_age[,"Yr"] %in% 96))
expect_true(all(tmp_age[, "FltSvy"] %in% 1))
expect_true(sum(tmp_age$Nsamp) == 20)

# test with multiple years of CAL
test_2_yrs_CAL <- sample_calcomp(exp_dat_2,
                       exp_dat_2,
                       fleets = c(1),
                       years = list(c(96, 98)),
                       Nsamp_lengths = list(50),
                       Nsamp_ages = list(10),
                       ESS_lengths = list(30),
                       ESS_ages = list(20))
# expectations for length comp
expect_true(nrow(test_2_yrs_CAL[["lencomp"]]) == 2)

expect_true(all(test_2_yrs_CAL[["lencomp"]][,"Yr"] %in% c(96, 98)))
expect_true(all(test_2_yrs_CAL[["lencomp"]][,"FltSvy"] == 1))
expect_true(all(test_2_yrs_CAL[["lencomp"]][, "Nsamp"] == 30))
# expectations for age comp
tmp_age <- test_2_yrs_CAL[["agecomp"]]
tmp_age <- tmp_age[tmp_age$Lbin_lo != -1 & tmp_age$Lbin_hi != - 1, ]
expect_true(all(tmp_age[,"Yr"] %in% c(96,98)))
expect_true(all(tmp_age[, "FltSvy"] %in% 1))
expect_true(sum(tmp_age$Nsamp) == 40) # because 2 years of 20 samples each.

#test with more complex sampling structure
test_2_yrs_CAL_complex <- sample_calcomp(exp_dat_2,
                                 exp_dat_2,
                                 fleets = c(1),
                                 years = list(c(96, 98)),
                                 Nsamp_lengths = list(c(50, 30)),
                                 Nsamp_ages = list(c(10, 10)),
                                 ESS_lengths = list(c(30, 20)),
                                 ESS_ages = list(c(20, 20)))
# expectations for length comp
expect_true(nrow(test_2_yrs_CAL_complex[["lencomp"]]) == 2)
expect_true(all(test_2_yrs_CAL_complex[["lencomp"]][,"Yr"] %in% c(96, 98)))
expect_true(all(test_2_yrs_CAL_complex[["lencomp"]][,"FltSvy"] == 1))
expect_true(all(test_2_yrs_CAL_complex[["lencomp"]][, "Nsamp"] == c(30, 20)))
# expectations for age comp
tmp_age <- test_2_yrs_CAL[["agecomp"]]
tmp_age <- tmp_age[tmp_age$Lbin_lo != -1 & tmp_age$Lbin_hi != - 1, ]
expect_true(all(tmp_age[,"Yr"] %in% c(96,98)))
expect_true(all(tmp_age[, "FltSvy"] %in% 1))
expect_true(sum(tmp_age$Nsamp) == 40) # because 2 years of 20 samples each.

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
             "A fleet specified in fleets was not found", fixed = TRUE)
#expect error b/c wrong years
expect_error(sample_calcomp(dat_list = exp_dat,
                            exp_vals_list = exp_dat,
                            fleets = c(1),
                            years = list(150),
                            Nsamp_lengths = list(10),
                            Nsamp_ages = list(5)),
            "A year specified in years was not found",
            fixed = TRUE)

# expect error b/c of wrong years
expect_error(sample_calcomp(exp_dat_2,
                                 exp_dat_2,
                                 fleets = c(1),
                                 years = list(c(96, 97)),
                                 Nsamp_lengths = list(50),
                                 Nsamp_ages = list(10),
                                 ESS_lengths = list(30),
                                 ESS_ages = list(20)),
                            "A year specified in years was not found",
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
# Test error if bad input for ESS used
expect_error(sample_calcomp(exp_dat,
                            exp_dat,
                            fleets = c(1),
                            years = list(96),
                            Nsamp_lengths = list(50),
                            Nsamp_ages = list(10),
                            ESS_lengths = list(30,1),
                            ESS_ages = list(20)),
             "ESS_lengths did not have the correct dimensions",
             fixed = TRUE)
expect_error(sample_calcomp(exp_dat,
                            exp_dat,
                            fleets = c(1),
                            years = list(96),
                            Nsamp_lengths = list(50),
                            Nsamp_ages = list(10),
                            ESS_lengths = list(30),
                            ESS_ages = list(20,1)),
             "ESS_ages did not have the correct dimensions",
             fixed = TRUE)
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
