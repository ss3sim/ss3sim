#' Alter a starter file for a retrospective analysis
#' 
#' @param startfile_in Input \code{starter.ss} file
#' @param startfile_out Output \code{starter.ss} file
#' @param retro_yr Which retrospective year to enter into the starter
#' file. Should be 0 (no retrospective analysis) or a negative value.
#' @references This function consists of three lines of code extracted
#' from Ian Taylor's \code{r4ss::SS_doRetro} function by Sean
#' Anderson.
#' @export

change_retro <- function(startfile_in = "starter.ss", startfile_out =
  "starter.ss", retro_yr = 0) {
  starter <- r4ss::SS_readstarter(startfile_in, verbose = FALSE)
  starter$retro_yr <- retro_yr
  r4ss::SS_writestarter(startfile_out, verbose = FALSE, overwrite = TRUE)
}


