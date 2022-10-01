#' Get Stock Synthesis binary/executable location
#'
#' @param bin_name A string providing the name of the binary/executable without
#'   the extension. The default is `"ss3"`, which is the name of the executable
#'   that is saved in {ss3sim} on GitHub.
#'
#' @return
#' A string providing the full path to a Stock Synthesis binary.
#' If using the GitHub version of {ss3sim}, this will be an internal binary.
#' Otherwise, `get_bin()` searches for a version of the binary in your path.
#' See the ss3sim vignette fore more information.
#'
#' @author Sean C. Anderson
#' @export
#'
#' @examples
#' \dontrun{
#' get_bin()
#' }
#'
get_bin <- function(bin_name = "ss3") {
  # code inspiration from glmmADMB package:
  if (.Platform$OS.type == "windows") {
    platform <- "Windows64"
    bin_name <- paste0(bin_name, ".exe")
    bit <- gsub("\\/", "", Sys.getenv("R_ARCH"))
    if (grepl("3", bit)) {
      if (!grepl("86", bit)) {
        platform <- "Windows32"
        warning(
          "Stock Synthesis binary is not available for 32-bit ",
          .Platform$OS.type, " within the package. ",
          "You must have an appropriate Stock Synthesis binary in your path. ",
          "See the ss3sim vignette."
        )
      }
    }
  } else {
    if (substr(R.version$os, 1, 6) == "darwin") {
      platform <- "MacOS"
    } else {
      if (R.version$os == "linux-gnu") {
        platform <- "Linux64"
      } else {
        warning(
          "Stock Synthesis binary is not available for ", R.version$os,
          " within the package.",
          "\nYou must have an appropriate Stock Synthesis binary in your ",
          "path. See the ss3sim vignette."
        )
      }
    }
  }
  loc <- system.file("bin", package = "ss3sim")
  if (loc != "") { # we found binaries in the package
    bin <- file.path(loc, platform, bin_name)
    if (!file.exists(bin)) bin <- ""
  } else {
    bin <- ""
  }

  if (bin == "") { # resort to binaries in path
    bin <- Sys.which(bin_name)[[1]]
    if (bin == "") {
      stop(paste0(
        "The expected Stock Synthesis executable, ", bin_name,
        ", was not found in your path. See the ss3sim vignette and ?r4ss::run",
        " for instructions."
      ))
    }
  }
  return(bin)
}
