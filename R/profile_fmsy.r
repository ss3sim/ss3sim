#' Determine Fmsy for a given operating model
#'
#' Runs an operating model over a range of fishing mortality levels to
#' determine the profile of F values from which Fmsy can be determined.
#'
#' @param om_in A directory for an \pkg{ss3sim} operating model.
#' @param results_out A directory to place the results
#' @param simlength Number of total years the \code{om_in} is set to run.
#' @param start Lower fishing mortality level
#' @param end Upper fishing mortality level
#' @param by_val Interval in which you wish to increment the fishing mortality
#'   level
#' @param ss_mode SS3 binary option. One of \code{"safe"} for the safe version
#'   of SS3 or \code{"optimized"} for the optimized version of SS3. The relevant
#'   binary needs to be in your system's path. See the vignette
#'   \code{vignette("ss3sim-vignette", package = "ss3sim")} for details and
#'   links to the binary files. Defaults to safe mode.
#' @importFrom r4ss SS_readdat SS_writedat
#' @return Creates a plot and a table with catches and F values (see the
#'   \code{results_out} folder). Also invisibly returns the Fmsy table as a data
#'   frame.
#' @export
#' @examples
#' \dontrun{
#' d <- system.file("extdata", package = "ss3sim")
#' omfolder <- paste0(d, "/models/cod-om")
#'
#'
#' fmsy.val <- profile_fmsy(om_in = omfolder, results_out = "fmsy",
#'   start = 0.1, end = 0.2, by_val = 0.05)
#' }

profile_fmsy <- function(om_in, results_out, simlength = 100,
  start = 0.00, end = 1.5, by_val = 0.01, ss_mode = c("safe", "optimized")) {
            # overM needs to be the value
            # you want to add or subtract from the trueM
            # or the case file you want to get the value
            # that you add to M in the last year, i.e. "M1"
            # used for + trueM
  origWD <- getwd()
  on.exit(expr = setwd(origWD), add = FALSE)

  if(ss_mode[1] == "optimized") ss_mode <- "opt"
  ss_bin <- paste0("ss3_24o_", ss_mode[1])

  fVector <- seq(start, end, by_val)
  fEqCatch <- NULL
  omModel <- om_in
  if(!file.exists(omModel)) {
    stop("OM folder does not exist")
  }
  newWD <- results_out
  dir.create(newWD, showWarnings = FALSE)
  setwd(newWD)
  file.copy(dir(omModel, full.names = TRUE), list.files(omModel))
  # remove recdevs from par
  parFile <- readLines("ss3.par")
  recDevLine <- grep("# recdev1", parFile) + 1
  sigmaRLine <- grep("# SR_parm[3]", parFile, fixed = TRUE) + 1
  parFile[recDevLine] <- paste(rep(0, simlength), collapse = ", ")
  parFile[sigmaRLine] <- 0.001
  writeLines(parFile, "ss3.par")
  for(i in seq(fVector)) {
    change_f(years = 1:simlength, years_alter = 1:simlength,
             fvals = rep(fVector[i], simlength),
             par_file_in = "ss3.par", par_file_out = "ss3.par" )
    system(paste(ss_bin, "-nohess"), show.output.on.console = FALSE,
           ignore.stdout=TRUE)

	temp_feq <- SS_readdat("data.ss_new", verbose = FALSE,
                            section = 2)$catch$Fishery[simlength]
	if(is.null(temp_feq))
	{
		fEqCatch[i] <- SS_readdat("data.ss_new", verbose = FALSE,
                            section = 2)$catch$fishery1[simlength]
	} else {
		fEqCatch[i] <- temp_feq
	}
  }
  pdf("Fmsy.pdf")
      par(mar = c(4, 6, 4, 4))
      plot(fVector, fEqCatch, las = 1,
           xlab = "Fishing mortality rate", ylab = "")
	  mtext(side = 2, text = "Yield at equilibrium", line = 4)
      maxFVal <- which.max(fEqCatch)
	  Fmsy <- fVector[maxFVal]
      abline(v = Fmsy)
      mtext(text = paste(om_in, "\n",
	                     "Fmsy \n", Fmsy, "\n",
                       "Catch at Fmsy \n", max(fEqCatch)),
               side = 1, line = -2, las = 1)
  dev.off()
  FmsyTable <- data.frame(fValues = fVector,
                          eqCatch = fEqCatch)
  write.table(FmsyTable, "Fmsy.txt")
  invisible(FmsyTable)
}
