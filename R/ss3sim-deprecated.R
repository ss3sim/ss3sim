#' @title Deprecated functions in package \pkg{ss3sim}
#' @description The functions listed below are deprecated and will be
#' removed from the package in the near future.
#' When possible, alternative functions with similar functionality are
#' available at `help("ss3sim-deprecated")`.
#' @name ss3sim-deprecated
#' @keywords internal
NULL
# functions from case-write.R

#' @section `case_comp()`:
#' Casefiles have been deprecated in favor of using a dataframe.
#' @rdname ss3sim-deprecated
#' @export
case_comp <- function(...) {
  lifecycle::deprecate_stop(when = "1.1.4", what = "case_comp()")
}

#' @section `case_index()`:
#' Casefiles have been deprecated in favor of using a dataframe.
#' @rdname ss3sim-deprecated
#' @export
case_index <- function(...) {
  lifecycle::deprecate_stop(when = "1.1.4", what = "case_index()")
}

#' @section `case_tv()`:
#' Casefiles have been deprecated in favor of using a dataframe.
#' @rdname ss3sim-deprecated
#' @export
case_tv <- function(...) {
  lifecycle::deprecate_stop(when = "1.1.4", what = "case_tv()")
}

#' @section `case_fishing()`:
#' Casefiles have been deprecated in favor of using a dataframe.
#' @rdname ss3sim-deprecated
#' @export
case_fishing <- function(...) {
  lifecycle::deprecate_stop(when = "1.1.4", what = "case_fishing()")
}

#' @section `case_deparse()`:
#' Casefiles have been deprecated in favor of using a dataframe.
#' @rdname ss3sim-deprecated
case_deparse <- function(...) {
  lifecycle::deprecate_stop(when = "1.1.4", what = "case_deparse()")
}

# deprecate function in expand_scenarios
#' @section `expand_scenarios()`:
#' Casefiles have been deprecated in favor of using a dataframe.
#' @rdname ss3sim-deprecated
expand_scenarios <- function(...) {
  lifecycle::deprecate_stop(when = "1.1.4", what = "expand_scenarios()")
}

# deprecated functions in expand_scenarios case_parsing
#' @section `get_args()`:
#' Casefiles have been deprecated in favor of using a dataframe.
#' @rdname ss3sim-deprecated
get_args <- function(...) {
  lifecycle::deprecate_stop(when = "1.1.4", what = "get_args()")
}

#' @section `get_caseval()`:
#' Casefiles have been deprecated in favor of using a dataframe.
#' @rdname ss3sim-deprecated
get_caseval <- function(...) {
  lifecycle::deprecate_stop(when = "1.1.4", what = "get_caseval()")
}

#' @section `get_caseargs()`:
#' Casefiles have been deprecated in favor of using a dataframe.
#' @rdname ss3sim-deprecated
get_caseargs <- function(...) {
  lifecycle::deprecate_stop(when = "1.1.4", what = "get_caseargs()")
}

#' @section `get_weight_comps_args()`:
#' Casefiles have been deprecated in favor of using a dataframe.
#' @rdname ss3sim-deprecated
get_weight_comps_args <- function(...) {
  lifecycle::deprecate_stop(when = "1.1.4", what = "get_weight_comps_args()")
}

#' @section `create_argfiles()`:
#' Casefiles have been deprecated in favor of using a dataframe.
#' @rdname ss3sim-deprecated
create_argfiles <- function(...) {
  lifecycle::deprecate_stop(when = "1.1.4", what = "create_argfiles()")
}

#' Deprecate change_[a-z]*_par because control file is used in ss3sim
#'
#' @section `change_f_par()`:
#' Early versions of ss3sim used the par file to control parameter values.
#' It was easier to maintain ss3sim and implement features using the control
#' file as more functionality was included in r4ss to read and write the
#' control file compared to the par file. Thus, ss3sim will use the control
#' file as much as possible going forward. Functions were being maintained
#' for both the control and par files with _par added after the function
#' name for those pertaining to the par file. Starting with ss3sim version
#' 1.1.6, this dual functionality was deprecated and only control file
#' manuipulation will be supported going forward.
#'
#' @rdname ss3sim-deprecated
change_f_par <- function(...) {
  lifecycle::deprecate_stop(when = "1.1.6", what = "change_f_par()")
}
