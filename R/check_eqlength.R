#' Check if input arguments have the same length
#'
#' Calculate the length of all input arguments to see if they
#' are equal. Entries that are `NULL`, and thus, have
#' a length of zero are ignored. An optional trigger to
#' [stop()] is provided with a tailored error message.
#'
#' @param ... Input arguments of unknown length.
#' @param keepgoing A logical value specifying if the function
#' should continue or terminate upon finding input arguments of
#' non-equal length. The default, `FALSE`, produces an
#' error and terminates the function.
#' @author Kelli F. Johnson
#' @return `TRUE` or `FALSE` depending on the result
#' of the test. Nothing is returned if the stop function is invoked.
#' @examples
#' \dontshow{
#' testthat::expect_error(ss3sim:::check_eqlength(),
#'   regex = "At least",
#'   label = "check_eqlength didn't error with zero input args"
#' )
#' testthat::expect_error(ss3sim:::check_eqlength(yes = 1:2, no = 3:5),
#'   regex = "Not all.+yes",
#'   label = "check_eqlength didn't error with unequal-lengthed vectors"
#' )
#' testthat::expect_true(ss3sim:::check_eqlength(yes = 1:2, no = 3:4),
#'   label = "check_eqlength didn't return TRUE."
#' )
#' testthat::expect_false(ss3sim:::check_eqlength(yes = 1:2, no = 3:5, keepgoing = TRUE),
#'   label = "check_eqlength didn't return TRUE."
#' )
#' testthat::expect_true(ss3sim:::check_eqlength(yes = NULL, no = NULL),
#'   label = "check-eqlength didn't return TRUE for all NULLs"
#' )
#' }
#'
check_eqlength <- function(..., keepgoing = FALSE) {
  if (length(list(...)) == 0) {
    stop("At least one object needs to be passed to ...", call. = FALSE)
  }
  lng <- lapply(list(...), length)
  if (all(lng == 0)) {
    return(TRUE)
  }
  lng <- lng[lng != 0]
  check <- ifelse(all(lng == lng[[1]]), TRUE, FALSE)
  if (!keepgoing & !check) {
    stop("Not all of the following input arguments ",
      "are of the same length:\n",
      paste(names(lng), collapse = ", "), ".",
      call. = FALSE
    )
  }
  return(check)
}
