#' Get Variability About Recruitment Deviations (\eqn{\sigma_R})
#'
#' Use the name of the operating model to open the ctl file and obtain the
#' INIT value for sigmaR (recruitment deviations sigma)
#'
#' @param om The name of the operating model, which should be the prefix of
#' the `.ctl` file, e.g., "myOM".
#' A full directory can be specified with the the prefix of the file name but
#' leaving off the '.ctl' portion.
#' @author Kelli F. Johnson
#' @export

get_sigmar <- function(om) {
  ctlFileName <- paste(om, ".ctl", sep = "")
  if (!file.exists(ctlFileName)) {
    stop("Cannot find the .ctl file for the specified operating model.")
  }
  pars <- r4ss::SS_parlines(
    ctlfile = ctlFileName, dir = NULL,
    verbose = FALSE, active = FALSE
  )
  sigmaRLoc <- grep("SR_sigmaR", pars$Label)
  return(pars[sigmaRLoc, "INIT"])
}
