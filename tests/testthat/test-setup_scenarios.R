context("tests for functions in setup_scenarios")



test_that("Default setup_scenarios work",{
  input <- setup_scenarios_defaults()
    scenario_list <- setup_scenarios(input)
    expect_equal(eval(parse(text = input$cf.fvals.1[1])),
                 scenario_list[[1]]$f_params$fvals$cf.fvals.1)
    expect_equal(eval(parse(text = input$si.years.2[1])),
                 scenario_list[[1]]$index_params$years$si.years.2)
    expect_equal(input$sl.Nsamp.1[1],
                 scenario_list[[1]]$lcomp_params$Nsamp$sl.Nsamp.1)
    expect_equal(eval(parse(text = input$sl.years.2[1])),
                 scenario_list[[1]]$lcomp_params$years$sl.years.2)
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
  expect_equal(eval(parse(text = df$cf.fvals.1[1])),
               scenario_list[[1]]$f_params$fvals$cf.fvals.1)
  expect_equal(eval(parse(text = df$cf.fvals.1[2])),
               scenario_list[[2]]$f_params$fvals$cf.fvals.1)
  expect_equal(df[2,"scenarios"], scenario_list[[2]]$scenarios)
  expect_equal(df[1, "bias_adjust"], scenario_list[[1]]$bias_adjust)
  expect_null(scenario_list[[2]]$lcomp_params$Nsamp$sl.Nsamp.2)
  expect_null(scenario_list[[2]]$lcomp_params$years$sl.years.2)
  expect_equal(scenario_list[[2]]$lcomp_params$fleets, 1)
})
