



test_that("Default setup_scenarios work", {
  input <- setup_scenarios_defaults()
  scenario_list <- setup_scenarios(input)
  expect_equal(
    eval(parse(text = input$cf.fvals.1[1])),
    scenario_list[[1]]$f_params$fvals$cf.fvals.1
  )
  expect_equal(
    eval(parse(text = input$si.years.2[1])),
    scenario_list[[1]]$index_params$years$si.years.2
  )
  expect_equal(
    input$sl.Nsamp.1[1],
    scenario_list[[1]]$lcomp_params$Nsamp$sl.Nsamp.1
  )
  expect_equal(
    eval(parse(text = input$sl.years.2[1])),
    scenario_list[[1]]$lcomp_params$years$sl.years.2
  )
  expect_equal(
    length(grep("sa.Nsamp\\.", colnames(input))),
    length(scenario_list[[1]]$agecomp_params$fleets)
  )
})

test_that("setup_scenarios work with multiple rows, NAs", {
  df <- setup_scenarios_defaults()
  df <- rbind(df, df)
  df[2, "sl.years.2"] <- NA # b/c can't put as null
  df[2, "sl.Nsamp.2"] <- NA # b/c can't put as null
  df[, "scenarios"] <- c("ctl", "no_lencomps_2")
  df[, "bias_adjust"] <- FALSE
  df[, "hess_always"] <- FALSE
  scenario_list <- setup_scenarios(df)
  expect_length(scenario_list, 2)
  expect_equal(
    eval(parse(text = df$cf.fvals.1[1])),
    scenario_list[[1]]$f_params$fvals$cf.fvals.1
  )
  expect_equal(
    eval(parse(text = df$cf.fvals.1[2])),
    scenario_list[[2]]$f_params$fvals$cf.fvals.1
  )
  expect_equal(df[2, "scenarios"], scenario_list[[2]]$scenarios)
  expect_equal(df[1, "bias_adjust"], scenario_list[[1]]$bias_adjust)
  expect_null(scenario_list[[2]]$lcomp_params$Nsamp$sl.Nsamp.2)
  expect_null(scenario_list[[2]]$lcomp_params$years$sl.years.2)
  expect_equal(scenario_list[[2]]$lcomp_params$fleets, 1)
  expect_true(all(mapply(length, scenario_list[[2]]$lcomp_params) == 1))
  expect_true(is.na(scenario_list[[2]]$lcomp_params$cpar))
  expect_true(length(scenario_list[[2]]$agecomp_params$cpar) == 2)
  expect_true(all(is.na(scenario_list[[2]]$agecomp_params$cpar)))
  df[2, "sl.Nsamp.2"] <- 20
  scenario_list <- setup_scenarios(df)
  expect_equal(scenario_list[[2]]$lcomp_params$fleets, 1,
    label = "sl.years.2 = NA wasn't enough to remove fleet 2."
  )
})

test_that("Catches are removed from third fleet", {
  df <- setup_scenarios_defaults()
  df <- rbind(df, df)
  df[1, "cf.years.3"] <- df[1, "cf.years.1"]
  df[2, "cf.years.3"] <- NA
  expect_warning(scenario_list <- setup_scenarios(df))
  expect_equal(scenario_list[[1]]$f_params$fleets, c(1, 3),
    label = "Fleet 3 catches were not added in 1st scenario."
  )
  expect_equal(scenario_list[[2]]$f_params$fleets, 1,
    label = "Fleet 3 catches were not removed in 2nd scenario."
  )
})

test_that("setup_scenarios works without specifying fleet for years then Nsamp", {
  dat_list <- exp_vals <- r4ss::SS_readdat(file.path(
    system.file("extdata", package = "ss3sim"), "example-om", "ss3_expected_values.dat"
  ),
  version = NULL, verbose = FALSE
  )

  df <- setup_scenarios_defaults()
  df[, "sl.Nsamp.2"] <- NULL
  df[, "sl.Nsamp"] <- df[, "sl.Nsamp.1"]
  df[, "sl.Nsamp.1"] <- NULL
  df_2 <- setup_scenarios_defaults()
  df_2[, "sl.Nsamp.2"] <- df_2[, "sl.Nsamp.1"]
  scenario_list <- setup_scenarios(df)
  scenario_list_2 <- setup_scenarios(df_2)

  set.seed(2)
  test <- do.call(
    "sample_comp",
    c(data = list(dat_list$lencomp), scenario_list[[1]]$lcomp_params)
  )
  set.seed(2)
  test_2 <- do.call(
    "sample_comp",
    c(data = list(dat_list$lencomp), scenario_list_2[[1]]$lcomp_params)
  )
  expect_equivalent(test, test_2)

  df <- setup_scenarios_defaults()
  df[, "sl.years.2"] <- NULL
  df[, "sl.years"] <- df[, "sl.years.1"]
  df[, "sl.years.1"] <- NULL
  df_2 <- setup_scenarios_defaults()
  df_2[, "sl.years.2"] <- df_2[, "sl.years.1"]
  scenario_list <- setup_scenarios(df)
  scenario_list_2 <- setup_scenarios(df_2)

  set.seed(2)
  test <- do.call(
    "sample_comp",
    c(data = list(dat_list$lencomp), scenario_list[[1]]$lcomp_params)
  )
  set.seed(2)
  test_2 <- do.call(
    "sample_comp",
    c(data = list(dat_list$lencomp), scenario_list_2[[1]]$lcomp_params)
  )
  expect_equivalent(test, test_2)
})
