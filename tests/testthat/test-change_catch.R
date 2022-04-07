
d <- system.file("extdata", "models", "cod-om", package = "ss3sim")
dat <- r4ss::SS_readdat(
  file = file.path(d, "codOM.dat"),
  verbose = FALSE
)
ctl <- r4ss::SS_readctl(
  file = file.path(d, "codOM.ctl"),
  verbose = FALSE,
  use_datlist = TRUE, datlist = dat
)

on.exit(rm(d, dat, ctl), add = TRUE)

test_that("change_catch works with ss3sim OM files", {
  new_dat <- change_catch(dat_list = dat, ctl_list = ctl)
  expect_true(all(new_dat[["catch"]][, "fleet"] == 1))
  expect_true(NROW(new_dat[["catch"]]) == 1)
  expect_equal(new_dat[["styr"]], 1)
})

test_that("change_catch works with 1 fleet of catch", {
  df <- setup_scenarios_defaults()
  params <- setup_scenarios(df)
  params <- params[[1]]

  new_ctl <- change_f(years = params$f_params$years, fleets = 1, fvals = 0.1, ctl_list = ctl)
  new_dat <- change_catch(dat_list = dat, ctl_list = new_ctl)
  expect_true(all(new_dat[["catch"]][, "fleet"] == 1))
})

test_that("change_catch works with 2 fleets of catch", {
  df <- setup_scenarios_defaults()
  df$cf.years.2 <- "26:100"
  df$cf.fvals.2 <- "rep(0.2, 75)"
  params <- setup_scenarios(df)
  params <- params[[1]]

  new_ctl <- change_f(
    years = params$f_params$years, fleets = 1:2,
    fvals = params$f_params$fvals, ctl_list = ctl
  )
  new_dat <- change_catch(dat_list = dat, ctl_list = new_ctl)
  expect_true(all(new_dat[["catch"]][, "fleet"] %in% c(1, 2)))
})

test_that("Equilibrium catch works with change_catch", {
  df <- setup_scenarios_defaults()
  df$cf.years.1 <- "c(-999, 20:100)"
  df$cf.years.2 <- "c(-999, 26:100)"
  df$cf.fvals.1 <- "rep(0.1, 82)"
  df$cf.fvals.2 <- "rep(0.2, 76)"
  params <- setup_scenarios(df)
  params <- params[[1]]

  new_ctl <- change_f(
    years = params$f_params$years, fleets = 1:2,
    fvals = params$f_params$fvals, ctl_list = ctl
  )
  new_dat <- change_catch(dat_list = dat, ctl_list = new_ctl)
  expect_true(all(new_dat[["catch"]][, "fleet"] %in% c(1, 2)))
  expect_equal(sum(new_dat[["catch"]][, "year"] == -999), 2)
})
