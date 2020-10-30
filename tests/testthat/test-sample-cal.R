context("run_ss3sim with conditional age-at-length data")

temp_path <- file.path(tempdir(), "run-CAAL-test")
dir.create(temp_path, showWarnings = FALSE)
wd <- getwd()
setwd(temp_path)
on.exit(setwd(wd), add = TRUE)
on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)

test_that("run_ss3sim runs with CAL data", {
  skip_on_cran()
  df <- data.frame(admb_options = "-maxfn 0",
    cf.years.1 = "26:100",
    cf.fval.1 = "rep('0.1052', 75)",
    si.years.2 = "seq(50,100,1)", si.sds_obs.2 = 0.01,
    sl.years.1 = "seq(50,100,4)", sl.Nsamp.1 = 20, sl.cpar.1 = "NULL",
    sa.years.1 = "seq(50,100,4)", sa.Nsamp.1 = 20, sa.cpar.1 = "NULL",
    sa.years.2 = "seq(50,100,1)", sa.Nsamp.2 = 20, sa.cpar.2 = "NULL",
    sc.years.2 = "seq(50,100,1)",
    sc.Nsamp_lengths.2 = 20, sc.Nsamp_ages.2 = 20,
    sc.ESS_lengths.2 = 20, sc.ESS_ages.2 = 20, sc.method = "simple_random")
  scname <- run_ss3sim(iterations  = 1, simdf = df)

  EM_datfile <- r4ss::SS_readdat(file.path(scname, "1", "em", "ss3.dat"),
                                 verbose = FALSE)
  # check the length comps to make sure CAL consistent
  NROW(EM_datfile$agecomp[EM_datfile$agecomp$Lbin_lo > -1, ])
  expect_equivalent(c(13, 51), c(table(EM_datfile$lencomp$FltSvy)))
  expect_true(all(aggregate(Nsamp ~ Yr, 
    data = EM_datfile$agecomp[EM_datfile$agecomp$Lbin_lo != -1, ], 
    FUN = sum)$Nsamp == 20))
})
