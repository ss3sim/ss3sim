#' Determine Fmsy for a given operating model
#'
#' Runs an operating model over a range of fishing mortality levels to
#' to determine the profile of F values from which Fmsy can be determined.
#'
#' @param om_in A directory for an \code{ss3sim} operating model.
#' @param results_out A directory to place the results
#' @param simlength Number of total years the \code{om_in} is set to run.
#' @param start Lower fishing mortality level
#' @param end Upper fishing mortality level
#' @param by_val Interval in which you wish to increment 
#'   the fishing mortality level
#' @importFrom r4ss SS_readdat SS_writedat
#' @export
#' @examples
#' d <- system.file("extdata", package = "ss3sim")
#' omfolder <- paste0(d, "/models/cod-om")
#'
#' fmsy.val <- fmsy_profile(om_in = omfolder, 
#'                          results_out = "c:/ss/fmsy",
#'                          start = 0.1, end = 0.2, by_val = 0.001)

profile_fmsy <- function(om_in, results_out, simlength = 100,
                         start = 0.00, end = 1.5, by_val = 0.01) {
            # overM needs to be the value
            # you want to add or subtract from the trueM
            # or the case file you want to get the value
            # that you add to M in the last year, i.e. "M1"
            # used for + trueM 
  origWD <- getwd()
  on.exit(expr = setwd(origWD), add = FALSE)
  fVector <- seq(start, end, by_val)
  fEqCatch <- NULL  
  d <- system.file("extdata", package = "ss3sim")
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
    change_f(years = 1:simlength, years_alter = 26:simlength, 
             fvals = rep(fVector[i], 75), 
             file_in = "ss3.par", file_out = "ss3.par" ) 
    system("ss3 -nohess", show.output.on.console = FALSE)
	fEqCatch[i] <- SS_readdat("data.ss_new", verbose = FALSE, 
                            section = 2)$catch$Fishery[simlength]
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

