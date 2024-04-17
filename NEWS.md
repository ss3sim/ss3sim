# News

## ss3sim 1.21.0

* Added season to the time series results and stopped hard-coding to
  season 1.
* Added month to the time series results.
* Use better semantic versioning protocols where the minor number
  is reserved for features and the patch number (terminal) is for
  fixes.

## ss3sim 1.20.2

* Fixed bug in Introduction vignette regarding self-test estimation method to
  allow for changing multiple parameters

## ss3sim 1.20.1

## ss3sim 1.20.0

* Updates {ss3sim} to use Stock Synthesis v3.30.20.
* Removes `shQuote()` from `get_bin()` where file paths with spaces are no
  longer quoted.

## ss3sim 1.19.3

* Updates {ss3sim} to use {r4ss} version 1.46.1
  * `tune_comps()`
  * `run()`, which requires removal of `hess_always` call and instead uses
    `extras` call to `run()` to turn the hessian off.

## ss3sim 1.19.2

* Depends on {stringi}.
* Removes deprecated arguments in `change_e()`.
* Adds `sds_out` to `sample_index()` thanks to the request and review by Leire
  Citores. Now, bias in the estimation method regarding the uncertainty of the
  index can be tested.

## ss3sim 1.19.1

* Updates `change_q()` to more modern code and removes bugs. Errors are no
  longer generated when the fleet does not exist.
* `find_position()` simplifies the code to find the position of a fleet name in
  a vector. As well as, the code was moved to an exported function for others
  to use outside of `change_q()`.

## ss3sim 1.19.0

* Bump version to match current version of SS3, where version numbers of
  {ss3sim} includes three parts separated by full stops. The first number
  represents the major version of {ss3sim}. The second number is the minor
  version, i.e., third set of digits, of the SS3 executable used in {ss3sim}.
  The third number is the minor version of {ss3sim}.
* Remove dependency on {bbmle} to externally estimate growth.
* Deprecate functions that were not being used in the code base, i.e.,
  `cleanup_ss3()`, `get_bin_info()`, `change_rec_devs_par()`, ...
* Does not turn on empirical weight-at-age in EM if sampling weight-at-age
  data. Instead, users must specify to use empirical weight-at-age data in
  their control file for the estimation method allowing users to sample the
  data but not use it if they do not want to.
* Allows for no estimation method via `em_dir = NA`, creating a way to simulate
  data quickly.

## ss3sim 1.1.8

* Fix bug related to tidyverse in `sample_comp()` where the composition data
  were appended to the data frame as a matrix rather than integrated as
  additional columns.

## ss3sim 1.1.7

* Deprecate the use of the log file in `ss3sim_base()`. The argument
  `ss3sim_base(print_logfile = FALSE)` has a new default and will warn users if
  they have the value set to `TRUE`. Fully deprecated in {ss3sim} version
  1.19.3.
* Create data objects for the OM and EM control and data files. These files are
  often used in examples.

## ss3sim 1.1.6

* Deprecate `change_f_par()` which was not being used by any downstream code
  because {ss3sim} uses the control file instead of the par file.
* Move Anderson et al. (2014) to a dedicated repository
  ss3sim/ss3sim_andersonetal.
* Allow for seas and partition in sampling.

## ss3sim 1.1.5

## ss3sim 1.1.4

## ss3sim 1.1.3

* Drop fleets from sampling regimes using `NA` in the scenario data frame.
* Upgrade to Stock Synthesis 3.30.16.
* Start allowing for seasons in data.
* Fixed bug in how q was being checked.
* Partition `change_catch()` from `change_data()`.
* Allow for fleet specific `cpar` and `ESS` values.
* Allow for no SPR in results file.
* Fix bad param argument in `weight_comps()` with DM.

## ss3sim 1.1.2

* Control height and width of jittering in `plot_points()` with `jitter.height`
  and `jitter.width`.

## ss3sim 1.1.1

* Allow for custom name of results files.

## ss3sim 1.1.0

* Upgrades to Stock Synthesis 3.30.15.03.
* Deprecates case files in favor of `run_ss3sim(simdf =)`.
* Uses more templates for parameter arguments, e.g., `par_int`.
* Simplifies scenarios, iterations, OM/EM to `dir` to increase the usability of
  functions outside of {ss3sim}.
* Adds `type.convert(as.is = TRUE)` because of R 4.0.
* Uses maxfn 0 to increase speed of tests and run more tests on CRAN check.
* Figures
  * Uses better labels for x and y axes in `ggplot2::ggplots()`;
  * Changes RE line to dashed black;
  * Passes coloration of lines in a better way;
  * Uses `plot_ss3sim()` as back-end code for plotting functions;
  * Uses character strings as input arguments with calls to data;
  * Adds cumulative mean figure; and
  * Allows for fill in boxplots;.
* Results:
  * Creates long-data format for results to allow functions to be used by
    {SSMSE};
  * Removes columns that are not needed;
  * Removes row names in summary files;
  * Renames rec_dev to increase clarity;
  * Creates better grep of management quantities;
  * Removes parallel capacity;
  * Only reads comp file if present by changing CompFile to
    `r4ss::SS_output()`;
  * Allows for relative directories for {SSMSE}.
* Allows for multiple fisheries in `change_f()` and change fisheries to fleets.
* Increases robustness of `get_success()`.
* Break tests into multiple chunks to pass Travis-ci checks.
* Allows weighting of composition data.
* Starts EM in first year with non-zero catches.
* Reinstate bias adjustment, but once for every iteration.

## ss3sim 1.0.4

* Add `create_logo()` to generate GitHub logo.
* change [travis
  configuration](https://towardsdatascience.com/travis-ci-for-r-advanced-guide-719cb2d9e0e5)
  to use Xenial.

## ss3sim 1.0.3

* Realized broken links are because of case not full path and changed all links
  back to shorter path with lower-case first letter.
* Includes NOAA disclaimer in README.

## ss3sim 1.0.2

* Changes link in inst/doc/making-models.html to a more explicit link using
  ../doc/introduction.html.

## ss3sim 1.0.1

* Fixes broken links in README.md and inst/doc/introduction.html.
* Confirmed that the links in inst/doc/making-models.html are working.

## ss3sim 1.0.0

* Improves  documentation.
* Updates, adds, or removes examples.
* Updates `profile_fmsy()` to allow for relative or absolute paths to
  directories and provide a better message if `verbose = TRUE` when users may
  have not run their model long enough to reach equilibrium.
* Removes reliance on {tidyverse}.
* Changes to 'iteration' instead of 'replicate'.
* Updates the license to 2019.
* Changes README from Rmd to md file.
* Deletes the vignette on making functions and updated remaining vignettes.
* Removes bias adjustment capability and sampling of conditional age-at-length
  data; both will eventually be added back to the package with better code
* Removes `write_file` argument from many functions and rely on other input
  arguments to determine if a file should be written to the disk, such as if
  the name of the file is `NULL` then nothing is written.
* Creates `change_o()` for the OM so users can input INIT values for parameters
  in their OM rather than having to use a dev vector and `change_tv()`.
* Implements check of q parameter such that a given q is created from a
  template for each survey-like data set included after sampling.
* Manipulate control file rather than par file and delete the par file from the
  OM folder.
* Use `merge()` rather than `cbind()` for those instances that the names of the
  columns in the results file from the OM do not exactly match the EM.
* Fixes `get_results()` working directory issue.
* Deprecates
  * `change_year()`,
  * `change_fltname()`,
  * `change_maturity()`, and
  * `pastef()`.
* Reduces restrictions placed on the OMs and EMs used by individual users to
  allow for multiple fleets, two-sexed models, and other trivial changes such
  as different fleet names.
* Implements the use of more {r4ss} functions.
* Increases the number of functions with examples and update examples to not
  change the users working directory when possible.
* Provides a single model inside the package rather than maintaining models
  both in the package and in ss3sim/ss3models.
* Kelli F. Johnson is now the maintainer of the package.
* Updates authors to reflect current contributors.
* Reduces the complexity of sampling functions by initiating the same protocol
  for ages and lengths, where each function calls a simple function.
* Updates functions to work with latest version of {r4ss}, which is compatible
  with Stock Synthesis 3.3.0
* Removes option to specify safe or optimized version of SS3.

## ss3sim 0.9.5

* Adds citation to Description.

## ss3sim 0.9.4

* Fixes compatibility with {dplyr} 0.6.0.
* Fixes issue with SS3 binary in development version #239.

## ss3sim 0.9.3

* Fixes LICENSE for CRAN.
* Updates author list.
* Incorporate @cstawitz's additions to the `making-models` vignette.
* Fixes to executable locating for Windows.

## ss3sim 0.9.2

* Updates DESCRIPTION.

## ss3sim 0.9.1

* Fixes compatibility with {dplyr} 0.4.3.9001.

## ss3sim 0.9.0

* Fixes compatibility of vignette with {ggplot2} 2.0.0.
* Adds new data types:
  * conditional age at length,
  * mean length at age, and
  * empirical weight at age.
* `sample_agecomp()` and `sample_lcomp()` now take new effective sample size
  (ESS) argument as inputs, so the user can weight the composition data as
  desired.
* Add functions to estimate von Bertalanffy growth. These are used internally
  by `change_e()`.
* Add `change_maturity()` to alter the SS3 maturity option.
* Check that fleet name in the data frame matches the newest SS3 version (see
  `change_fltname()`).
* Add `change_data()`, which prepares the OM .dat files to have the correct
  data used for sampling that scenario. It also can manipulate length bin
  widths, the SS3 robustification constant, and the SS3 tail compression value.
  It takes care of calling the new `change_tail_compression()` and
  `change_lcomp_constant()` functions.
* Add `calculate_re()` to calculate relative errors on the result data frames.
* Add internal checking of the validity of SS3 .dat files (#203).
* The `get_results()` function now returns the following:
  * run time,
  * a list of parameters that were on bounds, and
  * recruitment deviations.
* Switches vignette to R Markdown and HTML output (#194).
* Adds re-binning within the EM for length and conditional age-at-length data
  (#201, #205).
* Add Kelli's `profile_fmsy()` function (#173).
* Merrill added parallel processing option to the get results functions (#171).
* Adds parallel iterations processing option (#168).
* Function argument descriptions now indicate which ones need to be specified
  in case files with an asterisk (#161).
* Merrill updated the get-results functions to work without bias runs, to check
  for folders that look like ss3sim runs, and to issue a warning if there are
  problems reading the files (#155).
* Copy wtatage.ss_new from OM to EM (#163).
* Adds Travis CI testing of the package <https://travis-ci.org/ss3sim/ss3sim>.
* Adds much more extensive testing of the package in the root `tests` folder.
  This is for internal testing only and not bundled with the package (#156).
* Allow for missing arguments in case files. This allows us to add new
  arguments to existing functions without breaking old code. A missing argument
  gets passed as `NULL` via the `add_nulls()` function (within #148).
* Re-write case file parsing to be more robust and allow for scientific
  notations (#157).
* Adds the option to use the optimized Stock Synthesis executables (#138 and
  #147) and ability to have spaces in the path name to the executable.
* Adds Cole Monnahan's plotting functions (#134).
* `change_lcomp()`, `change_agecomp()`, and `change_index()` are now
  `sample_lcomp()`, `sample_agecomp()`, and `sample_index()` (#145).
* Requires the SS3 binary to be named according to the version (#138 and #147).
* Bump required version of {r4ss} to 1.22.1.
* Requires R 3.1.0.

## ss3sim 0.8.2

## New features

* Adds ability to pass seed value to `get_recdevs()` (#125).
* Adds dimension check of `user_recdevs` to ensure recruitment deviations are
  supplied for all iterations (#130).
* Uses a custom Solarized color theme in vignette.

## Bug fixes

* Moves vignette data files to the `data` folder so they are user-accessible
  after installing the package (#126).
* Updates out-dated paper citations. All papers are now accepted.
* Removes multi-core code in vignette for CRAN checks.
