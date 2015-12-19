#' Get SS3 binary/executable location in package
#'
#' @param bin_name Name of SS3 binary
#'
#' @return The path to an SS3 binary. If using the GitHub version of the
#'   package, this will be an internal binary. Otherewise, this function
#'   will search for a version of the binary in your path. See the
#'   ss3sim vignette.
#'
#' @export
#' @examples
#' \dontrun{
#' get_bin()
#' }

get_bin <- function(bin_name = "ss3_24o_opt") {
  # code inspiration from glmmADMB package:
  if (.Platform$OS.type == "windows") {
    platform <- "Windows64"
    bit <- gsub("\\/", "", Sys.getenv("R_ARCH"))
    if (grepl("3", bit)) {
      platform <- "Windows32"
      warning("SS3 binary is not available for 32-bit ", 
        .Platform$OS.type, " within the package.\n",
        "You must have an appropriate SS3 binary in your path.\n",
        "See the ss3sim vignette.")
    }
  } else {
    if (substr(R.version$os, 1, 6) == "darwin") {
      platform <- "MacOS"
    } else {
      if (R.version$os == "linux-gnu") {
        platform <- "Linux64"
      } else {
        warning("SS3 binary is not available for OS", R.version$os,
          "within the package. You must have an appropriate SS3 binary in your",
          "path. See the ss3sim vignette.")
      }
    }
  }
  loc <- system.file("bin", package = "ss3sim")
  if (loc != "") {
    bin <- file.path(loc, platform, bin_name)
    if (!file.exists(bin)) bin <- ""
  } else {
    bin <- ""
  }

  if (bin == "") { # resort to binaries in path
    bin <- Sys.which(bin_name)[[1]]
    if(bin == "") stop(paste0("The expected SS3 executable, ", bin_name,
      ", was not found in your path. See the ss3sim vignette and ?run_ss3model",
      " for instructions."))
  }
  if (grepl("[[:space:]]", bin)) {
    bin <- shQuote(bin)
  }
  bin
}
