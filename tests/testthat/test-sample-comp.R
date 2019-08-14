context("tests functions for sampling composition data")

d <- system.file("extdata", package = "ss3sim")
# om_dir <- paste0(d, "/models/cod-om")
# em_dir <- paste0(d, "/models/cod-em")
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
                          ESS              = NULL,
                          write_file       = FALSE)
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
                           ESS              = NULL,
                           write_file       = FALSE)
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
                          years            = lcomp$years,
                          cpar             = lcomp$cpar,
                          ESS              = NULL,
                          write_file       = FALSE),
               "The specified fleet number does not match input file")
  expect_error(new_dat <- sample_lcomp(dat_list = exp_dat,
                                       outfile          = NULL,
                                       fleets           = lcomp$fleets,
                                       Nsamp            = 100:101,
                                       years            = lcomp$years,
                                       cpar             = lcomp$cpar,
                                       ESS              = NULL,
                                       write_file       = FALSE),
               "Nsamp needs to be a list of same length as fleets")
  expect_error(new_dat <- sample_lcomp(dat_list = exp_dat,
                                       outfile          = NULL,
                                       fleets           = lcomp$fleets,
                                       Nsamp            = lcomp$Nsamp,
                                       years            = c(50,51),
                                       cpar             = lcomp$cpar,
                                       ESS              = NULL,
                                       write_file       = FALSE),
               "years needs to be a list of same length as fleets")

})

test_that("sample_agecomp runs and changes values" ,{
  set.seed(123)
  new_dat <- sample_agecomp(dat_list   = exp_dat,
                            outfile    = NULL,
                            fleets     = acomp$fleets,
                            Nsamp      = acomp$Nsamp,
                            years      = acomp$years,
                            cpar       = acomp$cpar,
                            ESS        = NULL,
                            write_file = FALSE)
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
                            ESS        = NULL,
                            write_file = FALSE)
  expect_equal(exp_dat$agecomp$Yr, new_dat_d$agecomp$Yr)
  expect_equal(dim(exp_dat$agecomp), dim(new_dat_d$agecomp))
  expect_false(any(exp_dat$agecomp[ ,10:ncol(exp_dat$agecomp)] ==
                     new_dat_d$agecomp[ ,10:ncol(new_dat_d$agecomp)]))
})

test_that("sample_agecomp gives errors as expected", {
  set.seed(123)
  expect_error(new_dat <- sample_agecomp(dat_list   = exp_dat,
                            outfile    = NULL,
                            fleets     = c(1,2),
                            Nsamp      = acomp$Nsamp,
                            years      = acomp$years,
                            cpar       = acomp$cpar,
                            ESS        = NULL,
                            write_file = FALSE),
               "The specified fleet number does not match input file")
  expect_error(new_dat <- sample_agecomp(dat_list   = exp_dat,
                                         outfile    = NULL,
                                         fleets     = acomp$fleets,
                                         Nsamp      = 100:101,
                                         years      = acomp$years,
                                         cpar       = acomp$cpar,
                                         ESS        = NULL,
                                         write_file = FALSE),
               "Nsamp needs to be a list of same length as fleets")
  expect_error(new_dat <- sample_agecomp(dat_list   = exp_dat,
                                         outfile    = NULL,
                                         fleets     = acomp$fleets,
                                         Nsamp      = acomp$Nsamp,
                                         years      = c(50,51),
                                         cpar       = acomp$cpar,
                                         ESS        = NULL,
                                         write_file = FALSE),
               "years needs to be a list of same length as fleets")
})

#TODO: add test for functions sampling correctly (might be easier to write if
# function is simplified.)
# test_that("sample_agecomp and samplelcomp sample correctly", {
#
# })


#future test for more general sample_comp function (to replace sample_lcomp and
# sample_agecomp
# test_that("sample_comp works" ,{
#
# })


