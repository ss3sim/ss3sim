context("Test clean_data()")

d <- system.file("extdata", package = "ss3sim")
om_dir <- file.path(d, "models", "cod-om")

# get the necessary inputs
dat <- SS_readdat(file.path(om_dir, "codOM.dat"),
  verbose = FALSE) # a data file

# Note: only the fleets and years list components matter for this function to
# work, so did not specify the other components.
agecomp_params <- lcomp_params <- list(fleets = 2, years = list(seq(50, 100, by = 10)))
mlacomp_params <- list(fleets = 1, years = list(c(2)))
calcomp_params <- list(fleets = 1, years = list(c(26, 27)))

test_that("clean_data is working for index, age comp, and length comp", {
  new_dat <- clean_data(dat,
                        agecomp_params = agecomp_params,
                        lcomp_params = lcomp_params)
  expect_false(nrow(new_dat$lencomp) == nrow(dat$lencomp))
  expect_false(nrow(new_dat$agecomp) == nrow(dat$agecomp))
  expect_equal(sort(new_dat$lencomp$Yr), sort(lcomp_params$years[[1]]))
  expect_equal(sort(new_dat$agecomp$Yr), sort(agecomp_params$years[[1]]))
})

test_that("clean_dat is working for mean size at age (mla_comp)",{
  # create some mla data to add to the data.
  dat$use_MeanSize_at_Age_obs <- 1
  dat$MeanSize_at_Age_obs <- data.frame(Yr = 1:10,
                                        Seas = 1,
                                        FltSvy = c(rep(1, times = 5),
                                                   rep(2, times = 5)),
                                        Gender = 3,
                                        Part = 0,
                                        AgeErr = 1,
                                        Ignore = 2,
                                        f1 = 30, f2 = 40,
                                        m1 = 20, m2 = 25,
                                        N_f1 = 20, N_f2 = 20,
                                        N_f3 = 20, N_f4 = 20)
  new_dat <- clean_data(dat,
                        mlacomp_params = mlacomp_params)
  expect_false(nrow(new_dat$MeanSize_at_Age_obs) == nrow(dat$MeanSize_at_Age_obs))
  expect_equal(sort(new_dat$MeanSize_at_Age_obs$Yr), sort(mlacomp_params$years[[1]]))
  expect_equal(new_dat$N_MeanSize_at_Age_obs, length(new_dat$MeanSize_at_Age_obs$Yr))
})

test_that("clean_dat works for conditional age at length", {
  new_bin_vec <- seq(min(dat$lbin_vector), max(dat$lbin_vector), by = 4)
  # add the max value if necessary.
  data_CAL <- dat
  # change approximately half of the obs to CAL
  a_col <- nrow(data_CAL$agecomp)
  max_change <- as.integer(a_col/2)
  data_CAL$agecomp$Lbin_lo[1:max_change] <- new_bin_vec[2]
  data_CAL$agecomp$Lbin_hi[1:max_change] <- new_bin_vec[length(new_bin_vec)-2]
  # this test can be removed when CAL is implemented
  new_dat <- clean_data(data_CAL,
                          calcomp_params = calcomp_params)
  expect_true(all(new_dat$agecomp$Yr %in% unlist(calcomp_params$years)))
  expect_true(all(new_dat$agecomp$FltSvy %in% unlist(calcomp_params$fleets)))
  expect_true(length(new_dat$agecomp$Yr) ==
                length(unlist(calcomp_params$years))*
                length(unlist(calcomp_params$fleets)))
  expect_true(all(new_dat$lencomp$Yr %in% unlist(calcomp_params$years)))
  expect_true(all(new_dat$lencomp$FltSvy %in% calcomp_params$fleets))
})

test_that("clean_data is working for calcomp and length comp used together", {
  new_dat <- clean_data(dat,
                        calcomp_params = calcomp_params,
                        lcomp_params = lcomp_params)
  expect_true(length(new_dat$lencomp$Yr) ==  8) #not a generic test. only works if calcomp and lencomp do not include any of the same fleets and years.
  expect_true(all(new_dat$lencomp$Yr %in% c(unlist(calcomp_params$years), unlist(lcomp_params$years))))
  expect_true(all(new_dat$lencomp$FltSvy %in% c(calcomp_params$fleets, lcomp_params$fleets)))

  #test if still works when the cal comp params and len comp params occur for same fleets and years
  lcomp_params_2 <- calcomp_params
  new_dat <- clean_data(dat,
                        calcomp_params = calcomp_params,
                        lcomp_params = lcomp_params_2)
  expect_true(length(new_dat$lencomp$Yr) ==  2)
  expect_true(all(new_dat$lencomp$Yr %in% c(unlist(calcomp_params$years), unlist(lcomp_params_2$years))))
  expect_true(all(new_dat$lencomp$FltSvy %in% c(calcomp_params$fleets, lcomp_params_2$fleets)))
})

test_that("clean_data fails when r4ss list object not used", {
  test_dat <- dat
  test_dat$type <- NULL
  expect_error(clean_data(dat_list = test_dat, lcomp_params = lcomp_params),
               "must be an r4ss data file")
  test_dat <- dat
  test_dat$type <- "Different File"
  expect_error(clean_data(dat_list = test_dat, lcomp_params = lcomp_params),
               "must be an r4ss data file")
})

test_that("clean_data fails when input is invalid", {
  # give bad lcomp_params values (too many fleets)
  lcomp_params$fleets <- c(1,2)
  expect_error(clean_data(dat_list = dat, lcomp_params = lcomp_params),
               "The structure of lcomp_params is not valid")
  # give bad year values for lcomp_params (out of range value)
  lcomp_params$fleets <- 2
  lcomp_params$years <- list(c(seq(50, 100, by = 10), 1000))
  expect_error(clean_data(dat_list = dat, lcomp_params =  lcomp_params),
               "Fleets or years specified in lcomp_params are not valid values in the datafile")
  # give bad input for lcomp (too many fleets)
  lcomp_params$years <- list(seq(50, 100, by = 10))
  lcomp_params <- list(fleets = c(1,2),
                       years  = list(seq(50, 100, by = 10)))
  expect_error(clean_data(dat_list = dat,
                          lcomp_params = lcomp_params), "The structure of lcomp_params is not valid")
})
