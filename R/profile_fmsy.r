#' Determine Fmsy for a given operating model
#'
#' Runs an operating model over a range of fishing mortality, \eqn{F}, levels to
#' determine the F at maximum sustainable yield, \eqn{F_{MSY}}.
#'
#' @param om_in A full or relative path to a directory that contains an
#'   \pkg{ss3sim} operating model.
#' @param results_out A full or relative path to a directory where the results
#' will be saved. The directory will be created if it does not already exist.
#' @param start,end A single numerical value for each argument specifying the
#'   lowest and highest fishing levels that you want to explore for the fishing
#'   fleet in your model.
#' @param by_val Interval at which fishing mortality will be incremented
#'   between `start` and `end` using `seq(start, end, by_val)`.
#' @template verbose
#' @return A data frame of catch by fishing mortality is returned invisibly and
#' saved to the disk along with a figure, `Fmsy.pdf`.
#' @export
#' @details `profile_fmsy()` runs the operating model with a constant level of
#' fishing for each year and extracts the expected catch in the terminal year.
#' It is assumed that the model time series is long enough for the population
#' to come to equilibrium, and thus, the catch in the terminal year is
#' equivalent to equilibrium catch.
#'
#' If the function is run with `verbose = TRUE`, which is not the default,
#' the coefficient of variations of the catches in the terminal years of
#' the model will be printed to the screen.
#' Here, terminal is defined as half as many years as there are
#' ages in the population dynamics of your model.
#' Thus, if the population plus group starts at age twenty,
#' the standard deviation of the last ten years of catch
#' divided by the mean catch over that same time will be printed to the
#' screen for each model that is ran. For the default cod model provided within
#' the package, the CV is less than 1e-04 for all explored levels of fishing
#' mortality.
#'
#' Ensure that the argument `om_in` leads to an operating model that is
#' configured for use within \pkg{ss3sim}. For example, the \eqn{F} type must
#' allow for an input vector of \eqn{Fs} rather than catches, along with other
#' specifications.
#'
#' @examples
#' \dontrun{
#' d <- system.file("extdata", "models", "cod-om", package = "ss3sim")
#' fmsy.val <- profile_fmsy(
#'   om_in = d, results_out = "fmsy",
#'   start = 0.1, end = 0.2, by_val = 0.05
#' )
#' # cleanup
#' unlink("fmsy", recursive = TRUE)
#' }
#'
profile_fmsy <- function(om_in,
                         results_out,
                         start = 0.00,
                         end = 1.5,
                         by_val = 0.01,
                         verbose = FALSE) {
  om_in <- normalizePath(om_in, mustWork = TRUE)
  dir.create(results_out, showWarnings = FALSE, recursive = TRUE)
  file.copy(
    from = dir(om_in, full.names = TRUE),
    to = file.path(results_out, list.files(om_in)),
    overwrite = TRUE
  )
  starter <- r4ss::SS_readstarter(
    file = file.path(results_out, "starter.ss"),
    verbose = FALSE
  )

  f_in <- seq(start, end, by_val)
  catch_out <- NULL
  cv_out <- NULL

  ## read in dat file to get years of model
  dat_list <- r4ss::SS_readdat(
    file = file.path(results_out, starter$datfile),
    verbose = FALSE
  )
  simlength <- dat_list$endyr - dat_list$styr + 1
  forecast <- r4ss::SS_readforecast(
    file = file.path(results_out, "forecast.ss"),
    verbose = FALSE
  )
  ## remove recdevs
  change_rec_devs(
    recdevs = stats::setNames(
      rep(0, simlength + forecast$Nforecastyrs),
      dat_list$styr:(simlength + forecast$Nforecastyrs)
    ),
    ctl_file_in = file.path(results_out, starter$ctlfile),
    ctl_file_out = file.path(results_out, starter$ctlfile)
  )
  r4ss::SS_changepars(
    ctlfile = file.path(results_out, starter$ctlfile),
    newctlfile = file.path(results_out, starter$ctlfile),
    strings = "SR_sigmaR",
    newvals = 0.001,
    newlos = 0,
    estimate = FALSE,
    verbose = FALSE
  )

  fleetofinterest <- dat_list$fleetinfo[dat_list$fleetinfo$type == 1, ]
  if (NROW(fleetofinterest) > 1) {
    stop("profile_fmsy is not meant to work with more than one fishery")
  }
  for (i in seq(f_in)) {
    ctl <- r4ss::SS_readctl(
      file.path(results_out, starter$ctlfile),
      use_datlist = TRUE,
      datlist = dat_list,
      verbose = FALSE
    )
    ctl <- change_f(
      years = 1:simlength,
      fleets = as.numeric(row.names(fleetofinterest)),
      fvals = rep(f_in[i], simlength),
      ctl_list = ctl
    )
    r4ss::SS_writectl(
      ctllist = ctl,
      outfile = file.path(results_out, starter$ctlfile),
      overwrite = TRUE,
      verbose = FALSE
    )
    r4ss::run(
      dir = results_out,
      exe = get_bin(),
      extras = "-nohess",
      skipfinished = FALSE,
      show_in_console = FALSE,
      verbose = FALSE
    )

    allcatch <- r4ss::SS_readdat(
      file.path(results_out, "data_expval.ss"),
      verbose = FALSE
    )$catch$catch
    endcatch <- utils::tail(allcatch, ceiling(dat_list$Nages * 0.5))
    cv_out[i] <- round(stats::sd(endcatch) / mean(endcatch), 5)
    catch_out[i] <- allcatch[simlength]
  }
  if (verbose) {
    message(
      "The cv_out of the catch in the last ",
      ceiling(dat_list$Nages * 0.5), " years of each model that was run were\n",
      paste(cv_out, collapse = " * "), "\nIf these are large, the model",
      " might not be reaching equilibrium and\n",
      "should be ran for more than ", simlength, " years."
    )
  }

  grDevices::pdf(file.path(results_out, "Fmsy.pdf"))
  graphics::plot(
    x = f_in,
    y = catch_out,
    type = "b",
    xlab = "Fishing mortality rate",
    ylab = "Yield at equilibrium"
  )
  f_msy <- f_in[which.max(catch_out)]
  graphics::points(
    x = f_msy, y = max(catch_out),
    col = "red", pch = 19
  )
  graphics::mtext(
    text = paste(
      " OM = ", om_in, "\n",
      "Fishing mortality at maximum yield (Fmsy) = ",
      f_msy, "\n",
      "Landings at Fmsy = ", max(catch_out), "(mt)"
    ),
    side = 1, line = -1, las = 1, adj = 0
  )
  grDevices::dev.off()
  f_table <- data.frame(
    fValues = f_in,
    eqCatch = catch_out
  )
  utils::write.table(f_table, file.path(results_out, "Fmsy.txt"))
  invisible(f_table)
}
