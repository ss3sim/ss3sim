context("test change_data.r functions")
#includes calculate_data_units, change_data,

d <- system.file("extdata", package = "ss3sim")
om <- file.path(d, "models", "cod-om")
dat <- SS_readdat(file.path(om, "codOM.dat"), verbose = FALSE)

test_that("calculate_data_units is working",{
  generic_list <- list(fleets = 2, years = list(seq(35, 95, by = 5)))
  #What is the point o fthis function? Does it still work as expected?
  result <- calculate_data_units(index_params = generic_list)
  expect_equal(result$fleets, generic_list$fleets)
  expect_equal(result$years, unlist(generic_list$years))
  expect_equal(result$types, "index")
  generic_list_2 <- list(fleets = 2, years = list(35, 37, 39))
  result_2 <- calculate_data_units(index_params = generic_list,
                                   lcomp_params = generic_list_2)
  expect_equal(result_2$fleets, generic_list$fleets)
  expect_equal(result_2$years, c(35, 37, 39, seq(40, 95, by = 5)))
  expect_equal(result$types, "index", "len")
  result_3 <- calculate_data_units(index_params  = generic_list,
                                   lcomp_params  = generic_list,
                                   agecomp_params = generic_list,
                                   calcomp_params = generic_list,
                                   mlacomp_params = generic_list,
                                   wtatage_params = generic_list)
  expect_equal(result_3$types, c("index", "len", "age", "cal", "mla", "wtatage"))
})

test_that("change_data is working with only types inputs", {
  yr_input <- seq(35, 100, by = 5)
  flt_input <- 2
  changed_dat <- change_data(dat, fleets =  flt_input, years = yr_input,
                             types = c("index", "len", "age", "mla"),
                              write_file = FALSE)
  expect_equal(changed_dat$CPUE$year,  yr_input)
  expect_equal(sort(unique(changed_dat$CPUE$index)), flt_input)
  expect_equal(changed_dat$NCPUEObs, c(0, length(yr_input)))
  expect_equal(changed_dat$lencomp$Yr, yr_input)
  expect_equal(sort(unique(changed_dat$lencomp$Flt)), flt_input)
  expect_equal(nrow(changed_dat$lencomp), changed_dat$N_lencomp)
  expect_equal(changed_dat$agecomp$Yr, yr_input)
  expect_equal(sort(unique(changed_dat$agecomp$Flt)), flt_input)
  expect_equal(nrow(changed_dat$agecomp), changed_dat$N_agecomp)
  expect_equal(changed_dat$MeanSize_at_Age_obs$Yr, yr_input)
  expect_equal(sort(unique(changed_dat$MeanSize_at_Age_obs$Flt)), flt_input)
  expect_equal(nrow(changed_dat$MeanSize_at_Age_obs), changed_dat$N_MeanSize_at_Age_obs)
})
test_that("change_data is working with conditional length at age", {
  #TODO: add test for conditional length at age.
  })

test_that("change_data() exits on error when incorrect input given",{
  yr_input <- seq(100, 125, by = 5)
  flt_input <- 5
  expect_error(change_data(dat, fleets =  2, years = yr_input,
                             types = "index",
                             write_file = FALSE),
               "Some years specified in years are not within the model years of dat_list")
  expect_error(change_data(dat, fleets =  flt_input, years = seq(5, 25, by = 5),
                           types = "index",
                           write_file = FALSE),
               "Some fleets specified in fleets are not included in dat_list")
  expect_error(change_data(dat, fleets = list(2), years = list(5,10,15),
                           types = "index", write_file = FALSE), "fleets and years input both need to be numeric vectors")
})

test_that("change_data() works with binning, tail compression, and lcomp constant", {
  #TODO: add tests()
})
