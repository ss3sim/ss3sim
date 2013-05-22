#' Alter a starter file for a retrospective analysis
#' 
#' @param startfile_in Input \code{starter.ss} file
#' @param startfile_out Output \code{starter.ss} file
#' @param retro_yr Which retrospective year to enter into the starter
#' file. Should be 0 (no retrospective analysis) or a negative value.
#' @author Sean C. Anderson
#' @export

change_retro <- function(startfile_in = "starter.ss", startfile_out =
  "starter.ss", retro_yr = 0) {

  # Sanity checks:
  if(retro_yr > 0) 
    stop("retro_yr should be <= 0")
  if(abs(retro_yr - round(retro_yr)) > .Machine$double.eps^0.5) 
    stop("retro_yr should be a whole number or integer")

  starter <- readLines(startfile_in)

  starter_line <- grep("retrospective", starter)
  retro_dat <- strsplit(starter[starter_line], " ")[[1]]
  retro_dat[1] <- retro_yr
  starter[starter_line] <- paste(retro_dat, collapse = " ")

  writeLines(starter, startfile_out)
}


