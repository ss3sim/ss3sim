# Read in cod OM and EM files using {r4ss}
#
# todo:
# Need to think about how we can use a github action
# to deploy data if the files in cod-om, cod-em, or data-raw
# are edited.
d <- system.file("extdata", package = "ss3sim")
om_dir <- file.path(d, "models", "cod-om")
em_dir <- file.path(d, "models", "cod-em")

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

rm(d, om_dir, em_dir)
usethis::use_data(name = codomdat, overwrite = TRUE)
usethis::use_data(name = codomctl, overwrite = TRUE)
usethis::use_data(name = codemctl, overwrite = TRUE)
