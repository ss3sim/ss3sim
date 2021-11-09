#' Determine Fmsy for a given operating model
#'
#' Runs an operating model over a range of fishing mortality, \eqn{F}, levels to
#' determine the F at maximum sustainable yield, \eqn{F_{MSY}}.
#'
#' @param om_in A full or relative path to a directory that contains an
#'   \pkg{ss3sim} operating model.
#' @param results_out A full or relative path to a directory where the results
#' will be saved. The directory will be created if it doesn't already exist.
#' @param start Lower fishing mortality levels that will be explored.
#' @param end Upper fishing mortality levels that will be explored.
#' @param by_val Interval at which F will be incremented between `start`
#'   and `end`.
#' @template verbose
#' @return Creates a plot and a table with catches and F values.
#' Also, invisibly returns a table of F and catch as a data frame.
#' @export
#' @details This function extracts the number of years from the data
#' file and then runs the model with a constant level of fishing for each year,
#' extracting the catch in the last year. This assumes the length of the
#' model is long enough to reach an equilibrium catch. The user is
#' responsible for ensuring this fact. If the function is run with
#' `verbose = TRUE`, which is not the default, users will be provided
#' with coefficient of variations of the catches in the terminal years of
#' the model. Here, terminal is defined as half as many years as there are
#' ages in the population dynamics of your model.
#' Thus, if the population plus group starts at age twenty,
#' then the standard deviation of the last ten years of catch
#' divided by the mean catch over that same time will be printed to the
#' screen for each model that is ran. For the default cod model provided within
#' the package, the CV is less than 1e-04 for all F levels explored.
#'
#' Ensure that the argument `om_in` leads to an operating model that is
#' configured for use within \pkg{ss3sim}. For example, the \eqn{F} type must
#' allow for an input vector of \eqn{Fs} rather than catches, along with other
#' specifications.
#'
#' @examples
#' \dontrun{
#'   d <- system.file("extdata", "models", "cod-om", package = "ss3sim")
#'   fmsy.val <- profile_fmsy(om_in = d, results_out = "fmsy",
#'     start = 0.1, end = 0.2, by_val = 0.05)
#'   #cleanup
#'   unlink("fmsy", recursive = TRUE)
#' }

profile_fmsy <- function(om_in, results_out,
  start = 0.00, end = 1.5, by_val = 0.01, verbose = FALSE) {

  origWD <- getwd()
  on.exit(expr = setwd(origWD), add = FALSE)

  ss_bin <- get_bin("ss")
  om_in <- normalizePath(om_in, mustWork = TRUE)
  dir.create(results_out, showWarnings = FALSE, recursive = TRUE)
  setwd(results_out)
  ignore <- file.copy(dir(om_in, full.names = TRUE), list.files(om_in),
    overwrite = TRUE)
  starter <- r4ss::SS_readstarter(verbose = FALSE)

  fVector <- seq(start, end, by_val)
  fEqCatch <- NULL
  CVs <- NULL

  ## read in dat file to get years of model
  datFile <- r4ss::SS_readdat(file= starter$datfile,
    version = NULL, verbose = FALSE)
  simlength <- datFile$endyr-datFile$styr+1
  forecast <- r4ss::SS_readforecast(file = "forecast.ss", verbose = FALSE)
  ## remove recdevs
  change_rec_devs(stats::setNames(
    rep(0, simlength + forecast$Nforecastyrs),
    datFile$styr:(simlength + forecast$Nforecastyrs)),
    ctl_file_in = starter$ctlfile,
    ctl_file_out = starter$ctlfile)
  r4ss::SS_changepars(ctlfile = starter$ctlfile,
    newctlfile = starter$ctlfile,
    strings = "SR_sigmaR", newvals = 0.001, newlos = 0,
    estimate = FALSE, verbose = FALSE)

  if (NROW((datFile$fleetinfo[datFile$fleetinfo$type == 1, ])) > 1) {
    stop("profile_fmsy is not meant to work with more than one fishery")
  }
  for(i in seq(fVector)) {
    ctl <- r4ss::SS_readctl(starter$ctlfile,
      use_datlist = TRUE, datlist = datFile, verbose = FALSE)
    ctl <- change_f(years = 1:simlength,
      fleets = as.numeric(row.names(datFile$fleetinfo[datFile$fleetinfo$type == 1, ])),
      fvals = rep(fVector[i], simlength),
      ctl_list = ctl)
    SS_writectl(ctllist = ctl, outfile = starter$ctlfile, overwrite = TRUE, verbose = FALSE)
    system(paste(ss_bin, "-nohess"), show.output.on.console = FALSE,
           ignore.stdout=TRUE)
    allcatch <- r4ss::SS_readdat("data.ss_new",
      verbose = FALSE, version = NULL, section = 2)$catch$catch
    endcatch <- utils::tail(allcatch, ceiling(datFile$Nages * 0.5))
	  CVs[i] <- round(stats::sd(endcatch) / mean(endcatch), 5)
    fEqCatch[i] <- allcatch[simlength]
  }
  if (verbose) message("The CVs of the catch in the last ",
    ceiling(datFile$Nages * 0.5), " years of each model that was run were\n",
    paste(CVs, collapse = " * "), "\nIf these are large, the model",
    " might not be reaching equilibrium and\n",
    "should be ran for more than ", simlength, " years.")

  pdf("Fmsy.pdf")
      plot(fVector, fEqCatch, type = "b",
           xlab = "Fishing mortality rate", ylab = "Yield at equilibrium")
      maxFVal <- which.max(fEqCatch)
	  Fmsy <- fVector[maxFVal]
      graphics::points(x = Fmsy, y = max(fEqCatch),
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
