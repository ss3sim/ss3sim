#' Get ths ss version (either 3.24 or 3.30) from a \code{dat_list} list.
#'
#' @description # Get the SS version from a list \code{dat_list} that was a
#'   originally read in using \code{\link[r4ss]{SS_readdat}}.
#' @template dat_list
get_ss_ver_dl <- function(dat_list){
  if(exists("SSversion", where = dat_list)){
    version <- dat_list$SSversion
  } else if(exists("ReadVersion", where = dat_list)){
    version <- dat_list$ReadVersion
  } else {
    stop("SS datafile version not found.") # Change to be more informative?
  }
  version
}

#' Get ths ss version (either 3.24 or 3.30) from an ss file
#'
#' @description # Get the SS version from the top line in an SS file.
#' as done in \code{\link[r4ss]{SS_readdat}}.
#' @param file Input SS3 control file, either a starter, control, or data SS file.
get_ss_ver_file <- function(file){
  # look for 3.24 or 3.30 at the top of the chosen control file
  nl <- 0
  version <- NULL
  while (length(version) == 0) {
    nl <- nl + 1
    version <- scan(file, what=character(), nlines=nl, quiet = TRUE)
  }
  version <- substring(version,3,6)
  if(version %in% c("3.24", "3.30")){ # perhaps too much output?
    #cat("assuming version", version, "based on first line of file\n")
  } else{
    stop(c("Input 'version' not found on first line of file. Must be 3.24 or 3.30. Please add comment on first line. If 3.24, first line must start with #V3.24 ; if 3.30, must start with #V3.30 ."))
  }
  version
}
