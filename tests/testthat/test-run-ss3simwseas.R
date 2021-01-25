context("run_ss3sim with season functionality")

# Tests are skipped on CRAN because the SS executable is no available.

temp_path <- file.path(tempdir(), "run-ss3sim-test")
dir.create(temp_path, showWarnings = FALSE)
wd <- getwd()
setwd(temp_path)
on.exit(setwd(wd), add = TRUE)
on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)

test_that("Survey with all months runs", {
  skip_on_cran()

  #check run_ss3sim and get_results_all
  set.seed(3)
  df <- data.frame(
    bias_adjust = FALSE,
    ce.par_name = "c('NatM_p_1_Fem_GP_1', 'L_at_Amin_Fem_GP_1')",
    ce.par_int = "c(NA, 19.9)",
    ce.par_phase = "c(-1, 4)",
    cf.years.1 = "26:100", cf.fval.1 = "rep('0.1052', 75)",
    si.years.2 = "seq(26,100,1)", si.sds_obs.2 = 0.01,
    si.seas.2 = "rep(1:12, length.out = length(26:100))",
    sl.years.1 = "seq(26,100,4)", sl.Nsamp.1 = 200, sl.cpar.1 = "NULL",
    sl.years.2 = "seq(26,100,1)", sl.Nsamp.2 = 201, sl.cpar.2 = "NULL",
    sa.years.1 = "seq(26,100,4)", sa.Nsamp.1 = 202, sa.cpar.1 = "NULL",
    sa.years.2 = "seq(26,100,1)", sa.Nsamp.2 = 203, sa.cpar.2 = "NULL")
  scname <- run_ss3sim(iterations = 1, simdf = df)
  dat <- r4ss::SS_readdat(file.path(scname, "1", "em", "ss3.dat"),
    verbose = FALSE)
  expect_true(all(1:12 %in% dat[["CPUE"]][, "seas"]))
  success <- get_success(file.path(scname, "1", "em"))
  expect_equal(success["ran"], c("ran" = 1),
    label = "Sucess vector for the Report file is")

  # check OM specs
  ssom <- r4ss::SS_output(file.path(scname, "1", "om"),
    verbose = FALSE, printstats = FALSE, covar = FALSE)
  expect_equal(min(ssom[["cpue"]][, "Yr"]), 26)
  expect_equal(max(ssom[["cpue"]][, "Yr"]), 100)
  expect_equivalent(
    ssom[["cpue"]] %>%
      dplyr::arrange(.data[["Fleet_name"]], .data[["Yr"]], .data[["Month"]]) %>%
      dplyr::select(.data[["Month"]]),
    data.frame(eval(expr = parse(text=df[,"si.seas.2"])))
  )
  expect_equal(ssom[["derived_quants"]]["SSB_unfished", "Value"], 4084180000)
  expect_equal(ssom[["cpue"]][ssom[["cpue"]][, "Yr"] == 99, "Exp"], 1619250000)
  # check OM specs
  ssem <- r4ss::SS_output(file.path(scname, "1", "em"),
    verbose = FALSE, printstats = FALSE, covar = FALSE)
  expect_equal(ssem[["derived_quants"]]["SSB_unfished", "Value"], 4025680000)
  expect_equal(ssem[["cpue"]][ssem[["cpue"]][, "Yr"] == 99, "Exp"], 1622760000)

  setwd(temp_path)
  unlink(scname, recursive = TRUE) # clean up
  unlink("ss3sim_*", recursive = TRUE)
})

