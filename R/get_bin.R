#' Get SS3 binary/executable location in package
#'
#' @param filename Name of SS3 binary (without file extension)
#'
#' @export
#' @examples
#' get_bin()

get_bin <- function(bin_name = "ss3_24o_opt") {
  # code inspiration from glmmADMB package:
  if (.Platform$OS.type == "windows") {
    platform <- "Windows64"
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
    file.path(loc, platform, bin_name)
  } else {
    ""
  }
}
