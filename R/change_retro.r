#' Alter a starter file for a retrospective analysis
#'
#' A retrospective analysis tests the effect of peeling back the number of
#' operating model years observable to the estimation model. This function
#' alters the Stock Synthesis starter file to run a retrospective analysis.
#'
#' @details Note that the starter file is set up to run a single retrospective
#'   run. Therefore, if you would like to run retrospective analyses for, say,
#'   0, 1, 2, 3, 4, and 5 years, you will need to use this function to adjust
#'   the starter file 6 separate times.
#'
#' @template str_file_in
#' @template str_file_out
#' @param retro_yr Which retrospective year to enter into the starter file.
#'   Should be 0 (no retrospective analysis) or a negative value, which
#'   leads to the removal of data for the specified number of years.
#'   Positive values are not allowed.
#' @author Sean C. Anderson
#' @return A modified Stock Synthesis starter file.
#' @family change functions
#'
#' @examples
#' # Create a temporary folder for the output:
#' temp_path <- file.path(tempdir(), "ss3sim-retro-example")
#' dir.create(temp_path, showWarnings = FALSE)
#'
#' # Locate the package data:
#' starterfile <- system.file("extdata", "models", "cod-om",
#'   "starter.ss",
#'   package = "ss3sim"
#' )
#'
#' # No retrospective analysis:
#' change_retro(starterfile, paste0(temp_path, "/retro-0-starter.ss"),
#'   retro_yr = 0
#' )
#'
#' # A retrospective analysis of 5 years:
#' change_retro(starterfile, paste0(temp_path, "/retro-5-starter.ss"),
#'   retro_yr = -5
#' )
#' @export

change_retro <- function(str_file_in = "starter.ss", str_file_out =
                           "starter.ss", retro_yr = 0) {
  # Sanity checks:
  if (retro_yr > 0) {
    stop("retro_yr should be <= 0")
  }
  if (abs(retro_yr - round(retro_yr)) > .Machine$double.eps^0.5) {
    stop("retro_yr should be a whole number or integer")
  }

  starter <- r4ss::SS_readstarter(file = str_file_in, verbose = FALSE)

  starter$retro_yr <- retro_yr

  r4ss::SS_writestarter(
    mylist = starter,
    dir = dirname(str_file_out), file = basename(str_file_out),
    verbose = FALSE, overwrite = TRUE
  )
}
