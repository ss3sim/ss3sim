# Not sure if the below test will work within the package build b/c
# of the use of the newest version of r4ss

context("Test standardize_bounds() with latest r4ss")

library(devtools)
install_github("r4ss/r4ss", ref = "master")
library(r4ss)
library(ss3sim)

temp_path <- file.path(tempdir(), "ss3sim-test")
dir.create(temp_path, showWarnings = FALSE)
wd <- getwd()
setwd(temp_path)

## Set to the path and filename of the OM and EM control files
file.copy(system.file("extdata", "models", "cod-em", package = "ss3sim"),
          ".", recursive = TRUE)
ctlom <- system.file("extdata", "models", "cod-om", "codOM.ctl",
  package = "ss3sim")


test_that("Bounds in EM are changed", {
file.copy(ctlom, "cod-em/om.ctl", overwrite = TRUE)
  # Use SS_parlines to get the proper names for parameters for the data frame
  om.pars <- SS_parlines(ctlfile = "cod-em/om.ctl")
  em.pars <- SS_parlines(ctlfile = "cod-em/codEM.ctl")
  #From the Johnson et al paper
  lo.percent<-c(rep(0.5, 7), rep(-20, 3))
  hi.percent<-c(500, 1000, 1000, rep(500, 4), rep(20, 3))
  #Populate data frame using EM parameter names and percentages from the
  # Johnson et al (2015) paper
  #Indices are the parameters you want to modify
  parlines <- c(1:6, 17, 24:26)
  percent.df <- data.frame(
    Label = as.character(em.pars[parlines, "Label"]),
    lo = lo.percent, hi = hi.percent)
  standardize_bounds(percent_df = percent.df, dir = "cod-em",
                     em_ctl_file = "codEM.ctl", om_ctl_file = "om.ctl")
  newpars <- SS_parlines(ctlfile = "cod-em/codEM.ctl")[parlines, ]
  expect_equal(newpars$LO[1:7],
               em.pars[parlines[1:7], "INIT"] * lo.percent[1:7],
               tolerance = 0.001)
})

unlink(temp_path, recursive = TRUE)
