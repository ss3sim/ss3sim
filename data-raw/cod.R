# Read in cod OM and EM files using {r4ss}
#
# todo:
# Need to think about how we can use a github action
# to deploy data if the files in cod-om, cod-em, or data-raw
# are edited.
d <- system.file("extdata", package = "ss3sim")
om_dir <- file.path(d, "models", "cod-om")
em_dir <- file.path(d, "models", "cod-em")
file.copy(
  from = system.file("bin", "Windows64", "ss.exe", package = "ss3sim"),
  to = file.path(om_dir, "ss.exe")
)
file.copy(
  from = system.file("bin", "Windows64", "ss.exe", package = "ss3sim"),
  to = file.path(em_dir, "ss.exe")
)
r4ss::run(
  dir = om_dir,
  extras = "-maxfun 0 -nohess"
)
file.copy(
  file.path(om_dir, "control.ss_new"),
  file.path(om_dir, "codOM.ctl"),
  overwrite = TRUE
)
file.copy(
  file.path(om_dir, "data_echo.ss_new"),
  file.path(om_dir, "codOM.dat"),
  overwrite = TRUE
)
file.copy(
  file.path(om_dir, "starter.ss_new"),
  file.path(om_dir, "starter.ss"),
  overwrite = TRUE
)
file.copy(
  file.path(om_dir, "forecast.ss_new"),
  file.path(om_dir, "forecast.ss"),
  overwrite = TRUE
)
file.remove(dir(om_dir, full.names = TRUE)[!grepl("cod|starter.ss$|forecast.ss$", dir(om_dir))])
file.copy(
  file.path(om_dir, "codOM.dat"),
  file.path(em_dir, "ss3.dat")
)
r4ss::run(
  dir = em_dir,
  extras = "-maxfun 0 -nohess -maxI 0"
)
file.copy(
  file.path(em_dir, "control.ss_new"),
  file.path(em_dir, "codEM.ctl"),
  overwrite = TRUE
)
file.copy(
  file.path(em_dir, "starter.ss_new"),
  file.path(em_dir, "starter.ss"),
  overwrite = TRUE
)
file.copy(
  file.path(em_dir, "forecast.ss_new"),
  file.path(em_dir, "forecast.ss"),
  overwrite = TRUE
)
file.remove(dir(em_dir, full.names = TRUE)[!grepl("cod|starter.ss$|forecast.ss$", dir(em_dir))])

codomdat <- r4ss::SS_readdat(
  file = dir(om_dir, pattern = "\\.dat", full.names = TRUE),
  verbose = FALSE
)

codomctl <- r4ss::SS_readctl(
  file = dir(om_dir, pattern = "\\.ctl", full.names = TRUE),
  verbose = FALSE,
  use_datlist = TRUE, datlist = codomdat
)

codemctl <- r4ss::SS_readctl(
  file = dir(em_dir, pattern = "\\.ctl", full.names = TRUE),
  verbose = FALSE,
  use_datlist = TRUE, datlist = codomdat
)

usethis::use_data(name = codomdat, overwrite = TRUE)
usethis::use_data(name = codomctl, overwrite = TRUE)
usethis::use_data(name = codemctl, overwrite = TRUE)
rm(list = ls())
