# Tests are skipped on CRAN because the Stock Synthesis executable is no available.

temp_path <- file.path(tempdir(), "run-ss3sim-test")
dir.create(temp_path, showWarnings = FALSE)
wd <- getwd()
setwd(temp_path)
on.exit(setwd(wd), add = TRUE)
on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)

dat <- r4ss::SS_readdat(
  dir(
    path = system.file("extdata", "models", "cod-om", package = "ss3sim"),
    pattern = "codOM.dat",
    full.names = TRUE,
    recursive = TRUE
  ),
  verbose = FALSE
)

# This section was developed when
# month, sex, and part were added to the sampling schemes
test_that("A season-specific data file looks right.", {
  df <- data.frame(
    bias_adjust = FALSE,
    ce.par_name = "c('NatM_uniform_Fem_GP_1', 'L_at_Amin_Fem_GP_1')",
    ce.par_int = "c(0.2, 19.9)",
    ce.par_phase = "c(-1, -4)",
    cf.years.1 = "26:100", cf.fval.1 = "rep('0.1052', 75)",
    si.years.2 = "seq(26,100,1)", si.sds_obs.2 = 0.01,
    si.month.2 = "rep(1:12, length.out = length(26:100))",
    sl.years.1 = "seq(26,100,4)", sl.Nsamp.1 = 200, sl.cpar.1 = "NULL",
    sl.years.2 = "seq(26,100,1)", sl.Nsamp.2 = 201, sl.cpar.2 = "NULL",
    sl.month.1 = 3, sl.month.2 = 2,
    sa.years.1 = "seq(26,100,4)", sa.Nsamp.1 = 202, sa.cpar.1 = "NULL",
    sa.years.2 = "seq(26,100,1)", sa.Nsamp.2 = 203, sa.cpar.2 = "NULL"
  )
  slist <- setup_scenarios(df)

  newdat <- change_comp(
    dat_list = dat,
    paramlist = slist[[1]][["lcomp_params"]]
  )
  expect_equivalent(
    tibble::as_tibble(slist[[1]][["lcomp_params"]][c("fleets", "month", "years")]) |>
      dplyr::mutate(years = lapply(years, length)) |> data.frame(),
    newdat[["lencomp"]] |> dplyr::group_by(fleet, month) |>
      dplyr::summarize(n = dplyr::n(), .groups = "keep") |>
      data.frame()
  )
})
test_that("Specifying month and part works.", {
  # todo fix if data frame has a mixture of characters to evaluate
  # and numeric, such as months here if month.2 was 2 instead of "2"
  df <- data.frame(
    bias_adjust = FALSE,
    ce.par_name = "c('NatM_uniform_Fem_GP_1', 'L_at_Amin_Fem_GP_1')",
    ce.par_int = "c(0.2, 19.9)",
    ce.par_phase = "c(-1, -4)",
    cf.years.1 = "26:100", cf.fval.1 = "rep('0.1052', 75)",
    si.years.2 = "seq(26,100,1)", si.sds_obs.2 = 0.01,
    si.month.2 = "rep(1:12, length.out = length(26:100))",
    sl.years.1 = "seq(26,100,4)", sl.Nsamp.1 = 200, sl.cpar.1 = "NULL",
    sl.years.2 = "seq(26,100,1)", sl.Nsamp.2 = 201, sl.cpar.2 = "NULL",
    sl.month.1 = "rep(12, length(seq(26,100,4)))",
    sl.month.2 = "2",
    sl.part.1 = "rep(1, length(seq(26,100,4)))",
    sl.part.2 = "rep(0, length(seq(26,100,1)))",
    sa.years.1 = "seq(26,100,4)", sa.Nsamp.1 = 202, sa.cpar.1 = "NULL",
    sa.years.2 = "seq(26,100,1)", sa.Nsamp.2 = 203, sa.cpar.2 = "NULL"
  )
  slist <- setup_scenarios(df)

  newdat <- change_comp(
    dat_list = dat,
    paramlist = slist[[1]][["lcomp_params"]]
  )
  sumt <- tibble::as_tibble(
    slist[[1]][["lcomp_params"]][c("part", "month")]
  ) |>
    tidyr::unnest(dplyr::everything()) |>
    table() |>
    data.frame()
  expect_equivalent(
    subset(sumt, Freq > 0)[["Freq"]],
    newdat[["lencomp"]] |> dplyr::group_by(part, fleet, month) |>
      dplyr::summarize(n = dplyr::n(), .groups = "keep") |>
      data.frame() |> dplyr::pull(n)
  )
})
test_that("Repeat some years for each part.", {
  # todo fix if data frame has a mixture of characters to evaluate
  # and numeric, such as months here if month.2 was 2 instead of "2"
  df <- data.frame(
    bias_adjust = FALSE,
    ce.par_name = "c('NatM_uniform_Fem_GP_1', 'L_at_Amin_Fem_GP_1')",
    ce.par_int = "c(0.2, 19.9)",
    ce.par_phase = "c(-1, -4)",
    cf.years.1 = "26:100", cf.fval.1 = "rep('0.1052', 75)",
    si.years.2 = "seq(26,100,1)", si.sds_obs.2 = 0.01,
    si.month.2 = "rep(1:12, length.out = length(26:100))",
    sl.years.1 = "c(seq(26,100,4), 91:100)",
    sl.Nsamp.1 = 200, sl.cpar.1 = "NULL",
    sl.years.2 = "seq(26,100,1)", sl.Nsamp.2 = 201, sl.cpar.2 = "NULL",
    sl.month.1 = "rep(12, length(seq(26,100,4))+10)",
    sl.month.2 = "2",
    sl.part.1 = "c(rep(0, length(seq(26,100,4))),rep(1,10))",
    sl.part.2 = "rep(0, length(seq(26,100,1)))",
    sa.years.1 = "seq(26,100,4)", sa.Nsamp.1 = 202, sa.cpar.1 = "NULL",
    sa.years.2 = "seq(26,100,1)", sa.Nsamp.2 = 203, sa.cpar.2 = "NULL"
  )
  slist <- setup_scenarios(df)

  newdat <- change_comp(
    dat_list = dat,
    paramlist = slist[[1]][["lcomp_params"]]
  )
  sumt <- tibble::as_tibble(
    slist[[1]][["lcomp_params"]][c("month", "part")]
  ) |>
    tidyr::unnest(dplyr::everything()) |>
    table() |>
    data.frame()
  expect_equivalent(
    subset(sumt, Freq > 0)[["Freq"]],
    newdat[["lencomp"]] |> dplyr::group_by(month, part) |>
      dplyr::summarize(n = dplyr::n(), .groups = "keep") |>
      dplyr::pull(n)
  )
})

test_that("Survey with all months runs", {
  skip_on_cran()

  # check run_ss3sim and get_results_all
  set.seed(3)
  df <- data.frame(
    bias_adjust = FALSE,
    ce.par_name = "c('NatM_uniform_Fem_GP_1', 'L_at_Amin_Fem_GP_1')",
    ce.par_int = "c(0.2, 19.9)",
    ce.par_phase = "c(-1, 4)",
    cf.years.1 = "26:100", cf.fval.1 = "rep('0.1052', 75)",
    si.years.2 = "seq(26,100,1)", si.sds_obs.2 = 0.01,
    si.month.2 = "rep(1:12, length.out = length(26:100))",
    sl.years.1 = "seq(26,100,4)", sl.Nsamp.1 = 200, sl.cpar.1 = "NULL",
    sl.years.2 = "seq(26,100,1)", sl.Nsamp.2 = 201, sl.cpar.2 = "NULL",
    sa.years.1 = "seq(26,100,4)", sa.Nsamp.1 = 202, sa.cpar.1 = "NULL",
    sa.years.2 = "seq(26,100,1)", sa.Nsamp.2 = 203, sa.cpar.2 = "NULL"
  )
  scname <- run_ss3sim(iterations = 1, simdf = df)
  dat <- r4ss::SS_readdat(file.path(scname, "1", "em", "ss3.dat"),
    verbose = FALSE
  )
  expect_true(all(1:12 %in% dat[["CPUE"]][, "month"]))
  success <- get_success(file.path(scname, "1", "em"))
  expect_equal(success["ran"], c("ran" = 1),
    label = "Sucess vector for the Report file is"
  )

  # check OM specs
  ssom <- r4ss::SS_output(file.path(scname, "1", "om"),
    verbose = FALSE, printstats = FALSE, covar = FALSE
  )
  expect_equal(min(ssom[["cpue"]][, "Yr"]), 26)
  expect_equal(max(ssom[["cpue"]][, "Yr"]), 100)
  expect_equivalent(
    ssom[["cpue"]] |>
      dplyr::arrange(Fleet_name, Yr, Month) |>
      dplyr::select(Month),
    data.frame(eval(expr = parse(text = df[, "si.month.2"])))
  )
  expect_equal(ssom[["derived_quants"]]["SSB_unfished", "Value"], 4084180000)
  expect_equal(ssom[["cpue"]][ssom[["cpue"]][, "Yr"] == 99, "Exp"], 1619250000)
  # check OM specs
  ssem <- r4ss::SS_output(file.path(scname, "1", "em"),
    verbose = FALSE, printstats = FALSE, covar = FALSE
  )
  expect_equal(ssem[["derived_quants"]]["SSB_unfished", "Value"], 407724e4)
  expect_equal(ssem[["cpue"]][ssem[["cpue"]][, "Yr"] == 99, "Exp"], 16175e5)

  setwd(temp_path)
  unlink(scname, recursive = TRUE) # clean up
  unlink("ss3sim_*", recursive = TRUE)
})
