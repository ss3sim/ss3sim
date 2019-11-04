# ss3sim 1.0.2

* changed link in inst/doc/making-models.html to a more explicit link
using ../doc/introduction.html

# ss3sim 1.0.1

* fixed broken links in Readme.md and inst/doc/introduction.html
* confirmed that the links in inst/doc/making-models.html are working

# ss3sim 1.0.0

* Improved the documentation of many functions
* Updated, added, or removed examples increasing the number of functions with
helpful examples
* Update profile_fmsy to allow for relative or absolute paths to directories
and provide a better message if verbose = TRUE warning users when they may have
not run their model long enough to reach equilibrium
* Remove reliance on tidyverse packages
* Change to 'iteration' instead of 'replicate'
* Update the license to 2019
* Change readme from Rmd to md file
* Deleted the vignette on making functions and updated remaining vignettes
* Remove bias adjustment capability and sampling of conditional age-at-length
data; both will eventually be added back to the package with better code
* Remove write_file argument from many functions and rely on other input
arguments to determine if a file should be written to the disk, such as
if the name of the file is NULL then nothing is written
* Create change_o for the operating model so users can input INIT values
for parameters in their operating model rather than having to use a dev
vector and the change_tv function
* Implement check of q parameter such that a given q is created from a template
for each survey-like data set included after sampling
* Manipulate control file rather than par file and delete the par
file from the operating model folder
* Use merge rather than cbind for those instances that the names of the 
columns in the results file from the OM do not exactly match the EM.
* Fix get_results working directory issue
* Deprecate change_year, change_fltname, change_maturity, and pastef
* Reduce restrictions placed on the operating and estimation models used
by individual users to allow for multiple fleets, two-sexed models, and other
trivial changes such as different fleet names. Feel free to email the
developers if there is a feature that you want or one that isn't working as
expected because more work in planned in this area.
* Implement the use of more r4ss functions such as SS_readstarter and 
SS_writestarter to decrease the amount of original code in ss3sim
* Increase the number of functions with examples and update examples
to not change the users working directory when possible.
* Change to providing a single model inside the package rather than
maintaining models both in the package and in ss3sim/ss3models
* Kelli Faye Johnson is now the maintainer of the package
* updated authors to reflect current contributors
* reduced the complexity of sampling functions by initiating the same
protocol for ages and lengths, where each function calls a simple function
* Update functions to work with latest version of r4ss, which is compatible
with SS 3.3.0, and change to using the newer executable of SS without the
option to specify safe or optimized

# ss3sim 0.9.5

* Adds citation to Description field

# ss3sim 0.9.4

* Fix compatibility with dplyr 0.6.0
* Fix issue with SS3 binary in development version #239

# ss3sim 0.9.3

* Fix LICENSE for CRAN
* Update author list 
* Incorporate @cstawitz's additions to the `making-models` vignette
* Fixes to executable locating for Windows

# ss3sim 0.9.2

* Revise package description 

# ss3sim 0.9.1

* Fix compatibility with dplyr 0.4.3.9001.

# ss3sim 0.9.0

* Fix compatibility of vignette with ggplot2 2.0.0.

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

* Switch vignette to R Markdown and HTML output (#194)

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
