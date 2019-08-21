context("Test clean_data()")

d <- system.file("extdata", package = "ss3sim")
om_dir <- file.path(d, "models", "cod-om")
sim_dir <- file.path(d, "Simple")

# get the necessary inputs
dat <- SS_readdat(file.path(om_dir, "codOM.dat")) # a data file
dat_sim <- SS_readdat(file.path(sim_dir, "simple.dat"))

# Note: only the fleets and years list components matter for this function to
# work, so did not specify the other components.
index_params   <- list(fleets = 2, years = list(seq(50, 100, by = 10)))
lcomp_params   <- index_params
agecomp_params <-  index_params
mlacomp_params <- list(fleets = 1, years = list(c(1971)))
calcomp_params <- mlacomp_params

test_that("clean_data is working for index, age comp, and length comp", {
  new_dat <- clean_data(dat, index_params = index_params,
                        agecomp_params = agecomp_params,
                        lcomp_params = lcomp_params)
  expect_false(nrow(new_dat$CPUE) == nrow(dat$CPUE))
  expect_false(nrow(new_dat$lencomp) == nrow(dat$lencomp))
  expect_false(nrow(new_dat$agecomp) == nrow(dat$agecomp))
  expect_equal(sort(new_dat$CPUE$year), sort(index_params$years[[1]]))
  expect_equal(sort(new_dat$lencomp$Yr), sort(lcomp_params$years[[1]]))
  expect_equal(sort(new_dat$agecomp$Yr), sort(index_params$years[[1]]))
  expect_equal(sum(new_dat$NCPUEObs), length(new_dat$CPUE$year))
})

test_that("clean_dat is working for mean size at age (mla_comp)",{
  index_params <- list(fleets  = 2,
                       years   = list(c(1980,1986,1992,1998)),
                       sds_obs = list(0.2))
  new_dat <- clean_data(dat_sim,
                        index_params = index_params,
                        mlacomp_params = mlacomp_params)
  expect_false(nrow(new_dat$MeanSize_at_Age_obs) == nrow(dat_sim$MeanSize_at_Age_obs))
  expect_equal(sort(new_dat$MeanSize_at_Age_obs$Yr), sort(mlacomp_params$years[[1]]))
  expect_equal(new_dat$N_MeanSize_at_Age_obs, length(new_dat$MeanSize_at_Age_obs$Yr))
})

test_that("clean_dat is working for conditional length at age", {
  index_params <- list(fleets  = 2,
                       years   = list(c(1980,1986,1992,1998)),
                       sds_obs = list(0.2))
  new_dat <- clean_data(dat_sim,
                        index_params = index_params,
                        calcomp_params = calcomp_params)
  # TODO: create this test.
})

test_that("clean_data fails when r4ss list object not used", {
  test_dat <- dat
  test_dat$type <- NULL
  expect_error(clean_data(dat_list = test_dat, index_params = index_params),
               "must be an r4ss data file")
  test_dat <- dat
  test_dat$type <- "Different File"
  expect_error(clean_data(dat_list = test_dat, index_params = index_params),
               "must be an r4ss data file")
})

test_that("clean_data fails when input is invalid", {
  # give bad index_params values (too many fleets)
  index_params$fleets <- c(1,2)
  expect_error(clean_data(dat_list = dat, index_params = index_params),
               "The structure of index_params is not valid")
  # give bad year values for index_params (out of range value)
  index_params$fleets <- 2
  index_params$years <- list(c(seq(50, 100, by = 10), 1000))
  expect_error(clean_data(dat_list = dat, index_params =  index_params),
               "Fleets or years specified in index_params are not valid values in the datafile")
  # give bad input for lcomp (too many fleets)
  index_params$years <- list(seq(50, 100, by = 10))
  lcomp_params <- list(fleets = c(1,2),
                       years  = list(seq(50, 100, by = 10)))
  expect_error(clean_data(dat_list = dat, index_params = index_params,
                          lcomp_params = lcomp_params), "The structure of lcomp_params is not valid")
})
