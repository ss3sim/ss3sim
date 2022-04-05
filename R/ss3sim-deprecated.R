#' @title Deprecated functions in package \pkg{ss3sim}
#' @description The functions listed below are deprecated and will be
#' removed from the package in the near future.
#' When possible, alternative functions with similar functionality are
#' available at `help("ss3sim-deprecated")`.
#' @name ss3sim-deprecated
#' @keywords internal
NULL

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
