#' @title Deprecated functions in package \pkg{ss3sim}
#' @description The functions listed below are deprecated and will be
#' removed from the package in the near future.
#' When possible, alternative functions with similar functionality are
#' available at \code{help("-deprecated")}.
#' @name ss3sim-deprecated
#' @keywords internal
NULL
# functions from case-write.R

#' @section `case_comp()`
#' Casefiles have been deprecated in favor of using a dataframe.
#' @rdname ss3sim-deprecated
#' @export
case_comp <- function(...) {
  lifecycle::deprecate_stop(when = "1.1.4", what = "case_comp()")
}

#' @section `case_index()`
#' Casefiles have been deprecated in favor of using a dataframe.
#' @rdname ss3sim-deprecated
#' @export
case_index <- function(...) {
  lifecycle::deprecate_stop(when = "1.1.4", what = "case_index()")
}

#' @section `case_tv()`
#' Casefiles have been deprecated in favor of using a dataframe.
#' @rdname ss3sim-deprecated
#' @export
case_tv <- function(...) {
  lifecycle::deprecate_stop(when = "1.1.4", what = "case_tv()")
}

#' @section `case_fishing()`
#' Casefiles have been deprecated in favor of using a dataframe.
#' @rdname ss3sim-deprecated
#' @export
case_fishing <- function(...) {
  lifecycle::deprecate_stop(when = "1.1.4", what = "case_fishing()")
}

#' @section `case_deparse()`
#' Casefiles have been deprecated in favor of using a dataframe.
#' @rdname ss3sim-deprecated
case_deparse <- function(...) {
  lifecycle::deprecate_stop(when = "1.1.4", what = "case_deparse()")
}
