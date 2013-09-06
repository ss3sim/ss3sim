The following is a table to keep track of the status of the functions in the
ss3sim package. When you complete a step, add your initials between the `[ ]`.
E.g. `[SA]`. Then commit your change (and push if you're working locally). This
table will be much easier to read if you work with a fixed-width font.

*Code cleaned* means that you've checked the code for:

- consistency of style (let's use [this style guide](http://adv-r.had.co.nz/Style.html))
- an appropriate level of commenting
- that appropriate sanity checks exist (e.g. with `stop` statements)

*Documented* means that the function has:

- a title (the first line of the file)
- a description `@description` (or a paragraph after the title and a blank
  line)
- arguments defined well enough that a new user could figure them out (most
  important part) `@param`
- a value section (what is returned) `@return`
- a details section if appropriate `@details`
- a see also section if appropriate `@seealso`
- any references that are appropriate `@references`

See `?roxygen2::rd_roclet` for a description of all the roxygen tags.

*Exampled* means that the function has a minimal working example (and that
you've tested all examples). See
[`change_e`](https://github.com/seananderson/ss3sim/blob/master/R/change_e.r)
for a good self-contained example section for one of the `change` functions.
If the example shouldn't be run when the package is checked or built then
surround the example in `\dontrun{}`. We probably don't want to run examples
if they create files, try and run SS, or take a long time. Otherwise, let
them run since they make an excellent form of automated error checking.


Function                | Code cleaned  | Documented    | Exampled
----------------------- | ------------- | ------------  | --------
bias_ss3                |  [ ]          |  [ ]          |  [ ]
change_agecomp          |  [ ]          |  [ ]          |  [ ]
change_e                |  [ ]          |  [ ]          |  [ ]
change_f                |  [ ]          |  [ ]          |  [ ]
change_growth           |  [ ]          |  [ ]          |  [ ]
change_index            |  [ ]          |  [ ]          |  [ ]
change_lcomp            |  [ ]          |  [ ]          |  [ ]
change_m                |  [ ]          |  [ ]          |  [ ]
change_rec_devs         |  [ ]          |  [ ]          |  [ ]
change_retro            |  [ ]          |  [ ]          |  [ ]
change_sel              |  [ ]          |  [ ]          |  [ ]
copy_ss3models          |  [ ]          |  [ ]          |  [ ]
create_argfiles         |  [ ]          |  [ ]          |  [ ]
create_new_rec_devs     |  [ ]          |  [ ]          |  [ ]
expand_scenarios        |  [ ]          |  [ ]          |  [ ]
get_caseargs            |  [ ]          |  [ ]          |  [ ]
get_fish600_casefolder  |  [ ]          |  [ ]          |  [ ]
get_fish600_modelfolder |  [ ]          |  [ ]          |  [ ]
get_results_all         |  [ ]          |  [ ]          |  [ ]
get_results_scalar      |  [ ]          |  [ ]          |  [ ]
get_results_scenario    |  [ ]          |  [ ]          |  [ ]
get_results_timeseries  |  [ ]          |  [ ]          |  [ ]
get_sigmar              |  [ ]          |  [ ]          |  [ ]
plot_scalar_boxplot     |  [ ]          |  [ ]          |  [ ]
plot_scalar_points      |  [ ]          |  [ ]          |  [ ]
plot_ts_boxplot         |  [ ]          |  [ ]          |  [ ]
plot_ts_points          |  [ ]          |  [ ]          |  [ ]
run_bias_ss3            |  [ ]          |  [ ]          |  [ ]
run_fish600             |  [ ]          |  [ ]          |  [ ]
run_ss3model            |  [ ]          |  [ ]          |  [ ]
run_ss3sim              |  [SCA]        |  [SCA]        |  [SCA]
ss3sim_base             |  [ ]          |  [ ]          |  [ ]
verify_input            |  [SCA]        |  [SCA]        |  [SCA]
