#' Get variability about recruitment deviations (\eqn{\sigma_R})
#'
#' Use the name of the operating model to open the control file and obtain the
#' `INIT` value for recruitment deviations \eqn{sigma} (\eqn{\sigma_R}).
#'
#' @param om The name of the operating model, which should be the prefix of
#' the control file, e.g., `"myOM"`.
#' A full directory can be specified with the the prefix of the file name but
#' leaving off the file extension.
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
