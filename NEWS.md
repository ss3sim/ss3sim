# ss3sim 0.9.0

* Add new data types: conditional age at length, mean length at age, and
  empirical weight at age.

* `sample_agecomp` and `sample_lcomp` now take new effective sample size
  (ESS) argument as inputs, so the user can weight the composition data as
  desired.

* Add functions to estimate von Bertalanffy growth. These are used internally
  by `change_e()`.

* Add `change_maturity()` to alter the SS3 maturity option

* Check that fleet name in the data frame matches the newest SS3 version
  (see `change_fltname()`)

* Add `change_data()`, which prepares the OM .dat files to have the correct
  data used for sampling that scenario. It also can manipulate length
  bin widths, the SS3 robustification constant, and the SS3 tail
  compression value. It takes care of calling the new
  `change_tail_compression()` and `change_lcomp_constant()` functions.

* Add `calculate_re()` to calculate relative errors on the result data frames.

* Add internal checking of the validity of SS3 .dat files (#203)

* The `get_results` function now returns run time, a list of parameters that were
  on bounds, and recruitment deviations.

* Switch vignette to R Markdown and HTML ouput (#194)

* Add re-binning within the estimation model of length and conditional
  age-at-length data (#201, #205)

* Add Kelli's `profile_fmsy()` function (#173)

* Merrill added parallel processing option to the get results functions (#171)

* Added parallel iterations processing option (#168)

* Function argument descriptions now indicate which ones need to be specified
  in case files with an asterisk (#161).

* Merrill updated the get-results functions to work without bias runs, to
  check for folders that look like ss3sim runs, and to issue a warning if
  there are problems reading the files (#155).

* Copy wtatage.ss_new from OM to EM (#163).

* Add Travis CI testing of the package https://travis-ci.org/ss3sim/ss3sim

* Added much more extensive testing of the package in the root `tests` folder.
  This is for internal testing only and not bundled with the package (#156).

* Allow for missing arguments in case files. This allows us to add new
  arguments to existing functions without breaking old code. A missing
  argument gets passed as `NULL` via the `add_nulls()` function (within #148).

* Re-write case file parsing to be more robust and allow for scientific
  notations (#157).

* Add the option to use the optimized SS executables (#138 and #147) and
  ability to have spaces in the path name to the executable.

* Add Cole Monnahan's ss3sim plotting functions (#134).

* `change_lcomp`, `change_agecomp`, and `change_index` are now `sample_lcomp`,
  `sample_agecomp`, and `sample_index`. The old functions point to the new
  functions with warnings that they are depreciated. Existing code should
  continue to work (#145).

* ss3sim now requires the SS3 binary to be named according to the version (#138
  and #147).

* Bump required version of r4ss to 1.22.1 since 1.22.0 did not export functions
  that ss3sim needs.

* ss3sim now requires R 3.1.0 since we cannot easily test on earlier versions.

# ss3sim 0.8.2

## New features

* Added ability to pass seed value to `get_recdevs()` (#125).

* Added dimension check of `user_recdevs` to ensure recruitment deviations are
  supplied for all iterations (#130).

* Vignette now uses a custom Solarized colour theme.

## Bug fixes

* Moved vignette data files to the `data` folder so they are user-accessible
  after installing the package (#126).

* Updated out-dated paper citations. All papers are now accepted.

* Removed multi-core code in vignette for CRAN checks.
