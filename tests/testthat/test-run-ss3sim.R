context("run_ss3sim and get-results functions work across a range of scenarios")

# Tests are skipped on CRAN because the SS executable is no available.

temp_path <- file.path(tempdir(), "run-ss3sim-test")
dir.create(temp_path, showWarnings = FALSE)
wd <- getwd()
setwd(temp_path)
on.exit(setwd(wd), add = TRUE)
on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)

test_that("A basic run_ss3sim scenario runs", {
  skip_on_cran()

  #check run_ss3sim and get_results_all
  df <- data.frame(
    bias_adjust = TRUE,
    ce.par_name = "c('NatM_p_1_Fem_GP_1', 'L_at_Amin_Fem_GP_1')",
    ce.par_int = "c(0.2, 19.9)",
    ce.par_phase = "c(-1, 4)",
    cf.years.1 = "26:100", cf.fval.1 = "rep('0.1052', 75)",
    si.years.2 = "seq(26,100,1)", si.sds_obs.2 = 0.01,
    sl.years.1 = "seq(26,100,4)", sl.Nsamp.1 = 200, sl.cpar.1 = "NULL",
    sl.years.2 = "seq(26,100,1)", sl.Nsamp.2 = 201, sl.cpar.2 = "NULL",
    sa.years.1 = "seq(26,100,4)", sa.Nsamp.1 = 202, sa.cpar.1 = "NULL",
    sa.years.2 = "seq(26,100,1)", sa.Nsamp.2 = 203, sa.cpar.2 = "NULL")
  scname <- run_ss3sim(iterations = 1, simdf = df)
  expect_true("control.ss_new" %in% list.files(file.path(scname,"1", "em")))
  success <- get_success(file.path(scname, "1", "em"))
  expect_equal(success["ran"], c("ran" = 1),
    label = "Sucess vector for the Report file is")
  expect_equal(success["hess"], c("hess" = 1),
    label = "Sucess vector for the hessian file is")
  suppressWarnings(get_results_all(user_scenarios = scname,
    overwrite_files = TRUE))
  expect_true(file.exists("ss3sim_scalar.csv"))
  expect_true(file.exists("ss3sim_ts.csv"))
  scalar <- read.csv("ss3sim_scalar.csv")

  # check OM specs
  ssom <- r4ss::SS_output(file.path(scname, "1", "om"),
    verbose = FALSE, printstats = FALSE, covar = FALSE)
  expect_equal(ssom$startyr, 1)
  expect_equal(ssom$endyr, 100)
  expect_equal(ssom$N_estimated_parameters, 1)
  expect_equal(ssom$parameters[grep("NatM", ssom$parameters$Label), "Value"], 0.2)
  om_line <- which(scalar$model_run == "om")
  expect_equal(scalar[om_line, "depletion"], 0.4242,
    tolerance = 0.0001, label = "OM depletion")
  expect_equal(scalar[om_line, "ForeRecr_101"], 0.09666,
    tolerance = 0.0001, label = "OM forecast recruitment in year 101")
  expect_equal(as.numeric(scalar[om_line, "SSB_MSY"]), 1417980000,
    label = "OM SSB at MSY")
  expect_equal(as.numeric(scalar[om_line, "Catch_endyear"]), 180383000,
    label = "OM terminal catch")

  # check EM specs
  ssem <- r4ss::SS_output(file.path(scname, "1", "em"),
    verbose = FALSE, printstats = FALSE, covar = TRUE)
  expect_equal(ssem$startyr, 26)
  expect_equal(ssem$endyr, 100)
  expect_equal(ssem$N_estimated_parameters, 112)
  expect_equal(ssem$N_forecast_yrs, 1)
  expect_equal(ssem$Max_phase, 5)
  expect_equal(ssem$log_det_hessian, 632.866, tolerance = 0.1)
  expect_equal(ssem$nseasons, 1)
  expect_equivalent(data.frame(Value = 0.2, Phase = -1, Init = 0.2),
    ssem$parameters[ssem$parameters$Label == "NatM_p_1_Fem_GP_1", c("Value", "Phase", "Init")])
  expect_equivalent(data.frame(Phase = 4, Init = 19.9),
    ssem$parameters[ssem$parameters$Label == "L_at_Amin_Fem_GP_1", c("Phase", "Init")])
  expect_equal(c(table(ssem$age_comp_fit_table[, c("Fleet", "Nsamp_in")])),
    c(19, 0, 0, 75))
  expect_equal(unique(ssem$age_comp_fit_table[, c("Nsamp_in")]), 202:203)
  expect_equal(unique(ssem$len_comp_fit_table[, c("Nsamp_in")]), 200:201)
  em_line <- which(scalar$model_run == "em")
  expect_equal(scalar[em_line, "depletion"],
    scalar[om_line, "depletion"],
    tolerance = 0.012, label = "OM depletion")
  expect_equal(type.convert(scalar[em_line, "SSB_MSY"], as.is = TRUE),
    type.convert(scalar[om_line, "SSB_MSY"], as.is = TRUE),
    scale = 1000000000000000, label = "EM SSB at MSY")
  expect_true(1 >
    abs((type.convert(scalar[em_line, "Catch_endyear"], as.is = TRUE) -
         type.convert(scalar[om_line, "Catch_endyear"], as.is = TRUE)) /
         type.convert(scalar[em_line, "Catch_endyear"], as.is = TRUE)) * 100,
    label = "EM terminal catch has less than 1 percent error")
  expect_equal(scalar[em_line, "SR_LN_R0"], 18.7,
    tolerance = 0.001, label = "EM R_0")
  #check provides warning if skippint iteration.
  expect_warning(run_ss3sim(iterations = 1,
    simdf = data.frame(df, scenarios = scname)),
    "already exists")
  expect_equal(table(ssem$timeseries$Era)["FORE"], c("FORE" = 1),
    label = "Number of forecast years")

  biasdir <- file.path(scname, "1", "em")
  setwd(biasdir)
  expect_true(file.exists("bias_00"))
  expect_false(file.exists("bias_01"))
  bias_list <- calculate_bias(getwd(), "em.ctl")
  expect_equal(bias_list$df$value[5], 0.98, tolerance = 0.01,
    label = "Estimated max adjust")
  bias_list <- suppressWarnings(calculate_bias(getwd(), "em.ctl"))
  expect_true(file.exists("bias_01"),
    label = "The bias_01 folder is present after calling bias 2x")
  unlink(dir("bias_01", pattern = "covar", full.names = TRUE))
  expect_warning(calculate_bias(dir = "bias_01", "em.ctl"),
    label = "With no hessian calculate_bias")
  setwd(temp_path)
  unlink(scname, recursive = TRUE) # clean up
  unlink("ss3sim_*", recursive = TRUE)
})

test_that("run_ss3sim works with multiple scenarios without estimation", {
  skip_on_cran()
  df <- data.frame(admb_options = "-maxfn 0",
    co.par_name = "c('NatM_p_1_Fem_GP_1', 'L_at_Amin_Fem_GP_1')",
    co.par_int = "c(0.20001, 19.0001)",
    cf.years.1 = "26:100",
    cf.fval.1 = c("rep('0.1052', 75)", "rep(0.1, 75)"),
    cd.age_bins = "1:10", cd.len_bins = "seq(10, 190, by=5)",
    cd.pop_binwidth = 1, cd.pop_minimum_size = 10,
    cd.pop_maximum_size = 190, cd.lcomp_constant = 1e-10,
    cb.lbin_method = 2, cb.pop_binwidth = 1,
    cb.pop_minimum_size = 1, cb.pop_maximum_size = 200,
    cb.bin_vector = "seq(10,190,by=10)",
    si.years.2 = "seq(90,100,1)", si.sds_obs.2 = 0.01,
    sl.years.1 = "seq(90,100,4)", sl.Nsamp.1 = 20, sl.cpar.1 = "NULL",
    sl.years.2 = "seq(90,100,1)", sl.Nsamp.2 = 20, sl.cpar.2 = "NULL",
    # sm.years.2 = "seq(90,100,1)", sm.Nsamp.2 = 10, sm.cpar.2 = "NULL",
    sa.years.1 = "seq(90,100,4)", sa.Nsamp.1 = 20, sa.cpar.1 = "NULL")
  scname <- run_ss3sim(iterations = 1:2, simdf = df)
  ssom <- r4ss::SS_output(file.path(scname[1], "1", "om"),
    verbose = FALSE, printstats = FALSE, covar = FALSE)
  expect_equal(ssom$parameters[grep("NatM", ssom$parameters$Label), "Value"], 0.20001)
  expect_equivalent(
    c(4, 4),
    vapply(lapply(scname, dir, recursive = TRUE, pattern = "control.ss_new"),
      FUN.VALUE = 1L, length)
  )

  ssom <- r4ss::SS_readdat(file.path(scname[1], "1", "om", "ss3.dat"),
    verbose = FALSE)
  ssem <- r4ss::SS_readdat(file.path(scname[1], "1", "em", "ss3.dat"),
    verbose = FALSE)
  expect_equal(ssom$lbin_vector_pop, seq(10, 191, by = 1))
  expect_equal(ssem$lbin_vector_pop, seq(1, 201, by = 1))
  expect_equal(ssem$agebin_vector, seq(1, 10, by = 1))
  expect_equal(ssem$lbin_vector, seq(10, 190, by = 10))
  expect_true(all(ssem$len_info$addtocomp == 1e-10))
  expect_true(all(ssem$len_info$mintailcomp == -1))
  sapply(scname, unlink, recursive = TRUE)
})
