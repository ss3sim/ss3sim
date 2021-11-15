#' Get the Stock Synthesis version (either 3.24 or 3.30) from a list object
#'
#' @description Get the Stock Synthesis version from a data list, `dat_list`,
#' that was a originally read in using [r4ss::SS_readdat()].
#' @template dat_list
get_ss_ver_dl <- function(dat_list) {
  version <- dat_list[["SSversion"]]
  if (is.null(version)) {
    version <- dat_list[["ReadVersion"]]
  }
  if (is.null(version)) stop("SS datafile version not found")
  version
}

#' Get the Stock Synthesis version (either 3.24 or 3.30) from a Stock Synthesis file
#'
#' @description Get the Stock Synthesis version.
#' This information can be found on the top line in a Stock Synthesis file.
#' See code in [r4ss::SS_readdat()] for an example.
#' @param file Input Stock Synthesis control file, starter, or data file.
#' @return A character value with the version number.
get_ss_ver_file <- function(file) {
  # look for 3.24 or 3.30 at the top of the chosen control file
  nl <- 0
  version <- NULL
  while (length(version) == 0) {
    nl <- nl + 1
    version <- scan(file, what = character(), nlines = nl, quiet = TRUE)
  }
  version <- substring(version, 3, 6)
  if (version %in% c("3.24", "3.30")) { # perhaps too much output?
    # cat("assuming version", version, "based on first line of file\n")
  } else {
    stop(
      "Input 'version' not found on first line of file.",
      "\nMust be 3.24 or 3.30. Please add comment on first line.",
      "\nIf 3.24, first line must start with '#V3.24'; if 3.30, must start with '#V3.30'."
    )
  }
  return(version)
}
