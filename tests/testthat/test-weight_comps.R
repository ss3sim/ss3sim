# test out the weight comps function

temp_path <- file.path(tempdir(), "test-weight_comps")
dir.create(temp_path, showWarnings = FALSE)
wd <- getwd()
setwd(temp_path)
on.exit(setwd(wd), add = TRUE)
on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)

d  <- system.file("extdata", package = "ss3sim")
em <- file.path(d, "models", "cod-em")
data <- file.path(d, "testing_em_cod.dat")

# create the folders.
dir.create(file.path(temp_path, "1"))
dir.create(file.path(temp_path, "2"))
dir.create(file.path(temp_path, "3"))

scen_path_MI <- file.path(temp_path, "1")
scen_path_Francis <- file.path(temp_path, "2")
scen_path_DM <- file.path(temp_path, "3")

file.copy(file.path(em, list.files(em)), scen_path_MI, recursive = TRUE)
file.copy(file.path(em, list.files(em)), scen_path_Francis, recursive = TRUE)
file.copy(file.path(em, list.files(em)), scen_path_DM, recursive = TRUE)
file.copy(data, scen_path_MI)
file.copy(data, scen_path_Francis)
file.copy(data, scen_path_DM)
# name the data file so it matches with what the starter file expects.
file.rename(file.path(scen_path_MI, "testing_em_cod.dat"), file.path(scen_path_MI,"ss3.dat"))
file.rename(file.path(scen_path_Francis, "testing_em_cod.dat"), file.path(scen_path_Francis,"ss3.dat"))
file.rename(file.path(scen_path_DM, "testing_em_cod.dat"), file.path(scen_path_DM,"ss3.dat"))

test_that("get_last_phase works", {
  start <- r4ss::SS_readstarter(file.path(em, "starter.ss"), verbose = FALSE)
  dat <- r4ss::SS_readdat(data, verbose = FALSE)
  ctl <- r4ss::SS_readctl(file.path(em, start$ctlfile), use_datlist = TRUE,
                          datlist = dat, verbose = FALSE)
  last_phase <- get_last_phase(ctl)
  expect_true(last_phase == 5) # based on last known value.
})

test_that("weight_comps works for MI method", {
  skip_on_cran()
  test <- weight_comps(method = "MI",
               dir = scen_path_MI,
               niters_weighting = 1,
               fleets = c(1,2))
  # create an expectation that arent dummy ones.
  dat <- r4ss::SS_readdat(file.path(scen_path_MI, "ss3.dat" ), verbose = FALSE)
  ctl <- r4ss::SS_readctl(file.path(scen_path_MI, "codEM.ctl"), verbose = FALSE,
                          use_datlist = TRUE, datlist = dat)
  expect_equivalent(ctl$Variance_adjustment_list, test[[length(test)]]) # only true if no adjustments initially.
  expect_true(all(test$Value <= 1))
  # any other better expectations?
})

test_that("weight_comps works for Francis", {
  skip_on_cran()
  test <- weight_comps(method = "Francis",
               dir = scen_path_Francis,
               niters_weighting = 1,
               fleets = c(1,2))
  dat <- r4ss::SS_readdat(file.path(scen_path_Francis, "ss3.dat" ), verbose = FALSE)
  ctl <- r4ss::SS_readctl(file.path(scen_path_Francis, "codEM.ctl"), verbose = FALSE,
                          use_datlist = TRUE, datlist = dat)
  expect_equivalent(ctl$Variance_adjustment_list, test[[length(test)]]) # only true if no adjustments initially.
  expect_true(all(test$Value <= 1)) # should always be true.
})

test_that("weight_comps works for DM", {
  skip_on_cran()
  test <- weight_comps(method = "DM",
               dir = scen_path_DM,
               fleets = c(1,2)
               )
  dat <- r4ss::SS_readdat(file.path(scen_path_DM, "ss3.dat" ), verbose = FALSE)
  ctl <- r4ss::SS_readctl(file.path(scen_path_DM, "codEM.ctl"), verbose = FALSE,
                          use_datlist = TRUE, datlist = dat)
  expect_true(!is.null(ctl[["dirichlet_parms"]]))
  comp_info <- rbind(dat$len_info, dat$age_info)
  expect_true(any(comp_info$CompError > 0))
  expect_true(any(comp_info$ParmSelect > 0))
})

test_that("run_ss3sim runs with data weighting", {
  df <- data.frame(admb_options = "-maxfn 0",
    cf.years.1 = "26:100",
    cf.fval.1 = "rep('0.1052', 75)",
    si.years.2 = "seq(90,100,1)", si.sds_obs.2 = 0.01,
    sl.years.1 = "seq(90,100,4)", sl.Nsamp.1 = 20, sl.cpar.1 = "NULL",
    sl.years.2 = "seq(90,100,4)", sl.Nsamp.2 = 20, sl.cpar.2 = "NULL",
    sa.years.1 = "seq(90,100,4)", sa.Nsamp.1 = 20, sa.cpar.1 = "NULL",
    wc.niters_weighting = 1, wc.method = "MI", wc.fleets = "1:2")
  scname <- run_ss3sim(iterations = 1, simdf = df)
  DW_dat <- r4ss::SS_readdat(file.path(scname, "1", "em", "ss3.dat"), verbose = FALSE)
  DW_ctl <- r4ss::SS_readctl(file.path(scname, "1", "em", "em.ctl"), use_datlist = TRUE,
                             datlist = DW_dat, verbose = FALSE)
  expect_true(DW_ctl$DoVar_adjust == 1)
  expect_true(all(DW_ctl$Variance_adjustment_list$Factor %in% c(4,5)))
})
