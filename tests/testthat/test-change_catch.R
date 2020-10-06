context("Function to change catch")

d <- system.file("extdata", package = "ss3sim")
om <- file.path(d, "models", "cod-om")
dat <- SS_readdat(file.path(om, "codOM.dat"), verbose = FALSE)


test_that("change_catch works with 1 fleet of catch", {
  # delete the length comps cols for fleet 1 so there are no comps or index
  # for fleet 1
  df <- setup_scenarios_defaults()
  params <- setup_scenarios(df)
  params <- params[[1]]

  new_dat <- change_catch(dat_list = dat, f_params = params$f_params)
  expect_true(all(new_dat[["catch"]][,"fleet"] == 1))
})

test_that("change_catch works with 2 fleets of catch", {
  # delete the length comps cols for fleet 1 so there are no comps or index
  # for fleet 1
  df <- setup_scenarios_defaults()
  df$cf.years.2 <- "25:100"
  df$cf.fvals.2 <- "rep(0.2, 75)"
  params <- setup_scenarios(df)
  params <- params[[1]]

  new_dat <- change_catch(dat_list = dat, f_params = params$f_params)
  expect_true(all(new_dat[["catch"]][,"fleet"] %in% c(1, 2)))
})
