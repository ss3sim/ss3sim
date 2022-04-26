#' Methods to alter the parameters estimated in a Stock Synthesis model
#'
#' @description Takes Stock Synthesis
#'   `.ctl` and `forecast.ss` files, along with
#'   a list structure which houses the data file as read in by
#'   [r4ss::SS_readdat()]
#'   and changes which parameters are estimated, how natural mortality is
#'   estimated, and if forecasts are performed. The function can be called by
#'   itself or within [run_ss3sim()] to alter an estimation model
#'   `.ctl` file.
#'
#' @template ctl_file_in
#' @template ctl_file_out
#' @template dat_list
#' @template for_file_in
#' @template par_name
#' @template par_int
#' @param par_phase A vector of phase values, one for each parameter in
#'   `par_name`.  Values can be `NA` if you do not wish to change
#'   the phase for a given parameter. Negative values will fix the parameter
#'   at the INIT value.
#' @param forecast_num Number of years to perform forecasts. For those years,
#'   the data will be removed from the `dat_list`, enabling Stock Synthesis to
#'   generate forecasts rather than use the data to fit the model.
#' @template verbose
#' @param natM_type Deprecated. Should have value NULL.
#' @param natM_n_breakpoints Deprecated. Should have value NULL.
#' @param natM_lorenzen Deprecated. Should have value NULL.
#' @param natM_val Deprecated. Should have value NULL.
#' @family change functions
#' @return
#' Altered versions of Stock Synthesis `.ctl` and `forecast.ss` files are written
#' to the disk and the altered `dat_list` is returned invisibly.
#'
#' @author Kelli F. Johnson
#' @export
#' @examples
#' d <- system.file("extdata", "models", "cod-om", package = "ss3sim")
#' data.old <- r4ss::SS_readdat(
#'   dir(d, pattern = ".dat", full.names = TRUE),
#'   verbose = FALSE
#' )
#' change_e(
#'   ctl_file_in = file.path(d, "codOM.ctl"),
#'   ctl_file_out = file.path(tempdir(), "change_e.ctl"),
#'   dat_list = data.old,
#'   for_file_in = file.path(d, "forecast.ss"),
#'   par_name = c("_steep", "SizeSel_P1_Fishery(1)"),
#'   par_int = c(0.3, 40), par_phase = c(3, 2),
#'   forecast_num = 0
#' )
#' # clean up the temporary files
#' file.remove(file.path(tempdir(), "change_e.ctl"))
change_e <- function(ctl_file_in = "em.ctl",
                     ctl_file_out = "em.ctl",
                     dat_list = NULL,
                     for_file_in = "forecasts.ss",
                     par_name = NULL,
                     par_int = "NA",
                     par_phase = "NA",
                     forecast_num = 0,
                     verbose = FALSE,
                     # below are deprecated parameters:
                     natM_type = NULL,
                     natM_n_breakpoints = NULL,
                     natM_lorenzen = NULL,
                     natM_val = NULL) {
  if (is.list(par_name)) par_name <- unlist(par_name)
  if (is.list(par_int)) par_int <- unlist(par_int)
  if (is.list(par_phase)) par_phase <- unlist(par_phase)
  check_eqlength(
    "par_name" = par_name,
    "par_int" = par_int, "par_phase" = par_phase
  )

  # Work with the out file b/c r4ss sometimes assumes the in and out files
  # will be in the same directory, so now we can specify the out directory
  # and out file and not overwrite the original file
  file.copy(ctl_file_in, ctl_file_out)

  # provide errors if deprecated parameters are used.
  lapply(
    list(
      natM_type = natM_type, natM_n_breakpoints = natM_n_breakpoints,
      natM_lorenzen = natM_lorenzen, natM_val = natM_val
    ),
    function(x) {
      if (!is.null(x)) {
        stop(
          "Parameters in change_e: natM_type, natM_n_breakpoints, ",
          "natM_lorenzen, and natM_val have been deprecated and cannot be ",
          "used. Instead, please make sure the default values NULL are used",
          " for deprecated parameters and specify your natural mortality ",
          "type within the EM model itself. If you wish to change the ",
          "value or phase of M parameter(s), please use parameters ",
          "par_name, par_init, and par_phase."
        )
      }
    }
  )
  ss3.ctl <- readLines(ctl_file_out)

  if (!is.null(par_name)) {
    par_name <- unlist(strsplit(par_name, split = ","))
    par_name_q <- grep("LnQ_", par_name, value = TRUE)
    if (length(par_name_q) > 0) {
      parsinmodel <- r4ss::SS_parlines(
        ctlfile = ctl_file_out,
        dir = NULL,
        verbose = verbose,
        active = FALSE
      )
      defaultq <- r4ss::SS_parlines(
        dir = NULL,
        ctlfile = dir(
          pattern = "\\.ctl",
          path = system.file("extdata", "models", "cod-em", package = "ss3sim"),
          full.names = TRUE
        ),
        verbose = verbose,
        active = FALSE
      )
      defaultq <- defaultq[grep("LnQ_", defaultq$Label), ]
      fleet_q <- sapply(
        strsplit(par_name_q, "\\(|\\)|_"),
        function(x) x[grepl("[0-9]+", x)]
      )
    }

    phasenochange <- is.na(par_phase)
    if (any(phasenochange)) {
      r4ss::SS_changepars(
        dir = dirname(ctl_file_out),
        ctlfile = basename(ctl_file_out),
        newctlfile = basename(ctl_file_out),
        linenums = NULL, strings = par_name[phasenochange],
        newvals = par_int[phasenochange], repeat.vals = verbose,
        newlos = NULL, newhis = NULL, estimate = NULL, verbose = verbose,
        newphs = par_phase[phasenochange]
      )
    }
    phaseneg <- which(par_phase < 0)
    if (length(phaseneg) > 0) {
      r4ss::SS_changepars(
        dir = dirname(ctl_file_out),
        ctlfile = basename(ctl_file_out),
        newctlfile = basename(ctl_file_out),
        linenums = NULL, strings = par_name[phaseneg],
        newvals = par_int[phaseneg], repeat.vals = verbose,
        newlos = NULL, newhis = NULL,
        estimate = rep(FALSE, times = length(par_name[phaseneg])), verbose = verbose,
        newphs = par_phase[phaseneg]
      )
    }
    pasepos <- which(par_phase >= 0)
    if (length(pasepos) > 0) {
      r4ss::SS_changepars(
        dir = dirname(ctl_file_out),
        ctlfile = basename(ctl_file_out),
        newctlfile = basename(ctl_file_out),
        linenums = NULL, strings = par_name[pasepos],
        newvals = par_int[pasepos], repeat.vals = verbose,
        newlos = NULL, newhis = NULL,
        estimate = rep(TRUE, times = length(par_name[pasepos])),
        verbose = verbose,
        newphs = par_phase[pasepos]
      )
    }
  }

  if (forecast_num > 0) {
    if (is.null(dat_list)) {
      stop(
        "A list object read in by r4ss::SS_readdat must be passed ",
        "to change_e using the dat_list argument if the user wishes to ",
        "implement or change the number of forecasts."
      )
    }
    if (!file.exists(for_file_in)) {
      stop("Forecast file for the estimation model does not exist.")
    }
    endyr_orig <- dat_list$endyr
    dat_list$endyr <- dat_list$endyr - forecast_num
    ss3.for <- r4ss::SS_readforecast(
      file = for_file_in,
      Nfleets = dat_list$Nfleet,
      Nareas = dat_list$N_areas,
      verbose = verbose,
      nseas = dat_list$nseas,
      readAll = TRUE
    )
    ss3.for <- check_forecast(ss3.for)
    ss3.for$Nforecastyrs <- forecast_num
    r4ss::SS_writeforecast(ss3.for,
      file = "forecast.ss", overwrite = TRUE,
      verbose = verbose
    )
  }
  if (!is.null(dat_list)) invisible(dat_list)
}
