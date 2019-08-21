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
