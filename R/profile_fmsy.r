#' Determine Fmsy for a given operating model
#'
#' Runs an operating model over a range of fishing mortality (F) levels to
#' determine the F at the maximum sustainable yield (Fmsy).
#'
#' @param om_in A directory for an \pkg{ss3sim} operating model
#' @param results_out A directory to save the results
#' @param start Lower fishing mortality level
#' @param end Upper fishing mortality level
#' @param by_val Interval in which you wish to increment the fishing mortality
#'   level from \code{start} to \code{end}
#' @importFrom r4ss SS_readdat SS_readforecast SS_changepars
#' @return Creates a plot and a table with catches and F values.
#' Also, invisibly returns a table of F and catch as a data frame.
#' @export
#' @details This function extracts the number of years from the model dat
#' file and then runs the model with a constant level of fishing for each year,
#' extracting the catch in the last year. This assumes the length of the
#' model is long enough to reach an equilibrium catch. The user is
#' responsible for ensuring this fact.
#' @examples
#' \dontrun{
#'   d <- system.file("extdata", "models", "cod-om", package = "ss3sim")
#'   fmsy.val <- profile_fmsy(om_in = d, results_out = "fmsy",
#'     start = 0.1, end = 0.2, by_val = 0.05)
#'   #cleanup
#'   unlink("fmsy", recursive = TRUE)
#' }

profile_fmsy <- function(om_in, results_out, dat_file_name = "ss3.dat",
  start = 0.00, end = 1.5, by_val = 0.01) {

  origWD <- getwd()
  on.exit(expr = setwd(origWD), add = FALSE)

  ss_bin <- get_bin("ss")
  dat_file_name <- dir(om_in, pattern = "\\.dat")

  fVector <- seq(start, end, by_val)
  fEqCatch <- NULL
  if(!file.exists(om_in)) {
    stop("OM folder does not exist")
  }
  dir.create(results_out, showWarnings = FALSE)
  setwd(results_out)
  ignore <- file.copy(dir(om_in, full.names = TRUE), list.files(om_in))
  ## read in dat file to get years of model
  datFile <- r4ss::SS_readdat(file= dat_file_name,
    version = NULL, verbose=FALSE)
  simlength <- datFile$endyr-datFile$styr+1
  forecast <- r4ss::SS_readforecast(file = dir(pattern = "forecast\\.ss$"),
    verbose = FALSE)
  ## remove recdevs
  change_rec_devs(rep(0, simlength + forecast$Nforecastyrs),
    ctl_file_in = dir(pattern = "ctl"),
    ctl_file_out = dir(pattern = "ctl"))
  r4ss::SS_changepars(ctlfile = dir(pattern = "ctl"),
    newctlfile = dir(pattern = "ctl"),
    strings = "SR_sigmaR", newvals = 0.001, newlos = 0,
    estimate = FALSE, verbose = FALSE)

  if (NROW((datFile$fleetinfo[datFile$fleetinfo$type == 1, ])) > 1) {
    stop("profile_fmsy is not meant to work with more than one fishery")
  }
  for(i in seq(fVector)) {
    change_f(years = 1:simlength,
      fisheries = as.numeric(row.names(datFile$fleetinfo[datFile$fleetinfo$type == 1, ])),
      fvals = rep(fVector[i], simlength),
      ctl_file_in = dir(pattern = "ctl"),
      ctl_file_out = dir(pattern = "ctl"))
    system(paste(ss_bin, "-nohess"), show.output.on.console = FALSE,
           ignore.stdout=TRUE)
    allcatch <- r4ss::SS_readdat("data.ss_new", 
      verbose = FALSE, version = NULL, section = 2)$catch$catch
    endcatch <- tail(allcatch, ceiling(datFile$Nages * 0.5))
    if (sd(endcatch) / mean(endcatch) > 1e-04) stop("The population doesn't", 
      " appear to be reaching equilibrium, \n", 
      "the model should be ran for more than ", simlength, " years.")
	  fEqCatch[i] <- allcatch[simlength]
  }
  pdf("Fmsy.pdf")
      plot(fVector, fEqCatch, type = "b",
           xlab = "Fishing mortality rate", ylab = "Yield at equilibrium")
      maxFVal <- which.max(fEqCatch)
	  Fmsy <- fVector[maxFVal]
      points(x = Fmsy, y = max(fEqCatch), 
        col = "red", pch = 19)
      mtext(text = paste(" OM = ", om_in, "\n",
	                     "Fishing mortality at maximum yield (Fmsy) = ",
                       Fmsy, "\n",
                       "Landings at Fmsy = ", max(fEqCatch), "(mt)"),
               side = 1, line = -1, las = 1, adj = 0)
  dev.off()
  FmsyTable <- data.frame(fValues = fVector,
                          eqCatch = fEqCatch)
  write.table(FmsyTable, "Fmsy.txt")
  invisible(FmsyTable)
}
