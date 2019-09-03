#' Methods to alter which parameters are estimated in a SS3 \code{.ctl} file.
#'
#' @description Takes SS3 \code{.ctl} and \code{forecast.ss} files, along with
#'   a list structure which houses the data file as read in by
#'   \code{\link[r4ss]{SS_readdat}}
#'   and changes which parameters are estimated, how natural mortality is
#'   estimated, and if forecasts are performed. The function can be called by
#'   itself or within \code{\link{run_ss3sim}} to alter an estimation model
#'   \code{.ctl} file.
#'   If used with \code{\link{run_ss3sim}} the case file should be named
#'   \code{E}. A suggested (default) case letter is \code{E} for estimation.
#'
#' @template ctl_file_in
#' @template ctl_file_out
#' @template dat_list
#' @template for_file_in
#' @param par_name *A vector of values, separated by commas.  Each value
#'   corresponds to a parameter that you wish to turn on or off in the
#'   \code{ctl_file_in}. The values will later be turned into character values
#'   and used to search for specific lines for each parameter in the
#'   \code{ctl_file_in}, therefore it is best to use full parameter names as
#'   they are specified in \code{ctl_file_in}.
#' @param par_int *A vector of initial values, one for each parameter in
#'   \code{par_name}.  Values can be \code{NA} if you do not wish to change the
#'   initial value for a given parameter.
#' @param par_phase *A vector of phase values, one for each parameter in
#'   \code{par_name}.  Values can be \code{NA} if you do not wish to change
#'   the phase for a given parameter.
#' @param forecast_num *Number of years to perform forecasts. For those years,
#'   the data will be removed from the \code{dat_list}, enabling SS3 to
#'   generate forecasts rather than use the data to fit the model.
#' @template verbose
#' @param natM_type Deprecated. Should have value NULL.
#' @param natM_n_breakpoints Deprecated. Should have value NULL.
#' @param natM_lorenzen Deprecated. Should have value NULL.
#' @param natM_val Deprecated. Should have value NULL.
#' @details Turning parameters on and off is the main function of
#'   \code{change_e}.  \code{change_e} was not created with the capability of
#'   adding parameters to a \code{.ctl} file.  The function can only add
#'   parameters for age specific natural mortality, and only for models with
#'   one growth morph.  Furthermore, the function is designed to add complexity
#'   to the natural mortality type and not remove complexity.  Therefore, the
#'   function will fail if natural mortality in the \code{ctl_file_in} is not
#'   specified as \code{"1Param"} and \code{natM_type} is anything other than
#'   \code{NULL} or \code{"1Param"}.
#' @template casefile-footnote
#' @family change functions
#' @return
#' Altered versions of SS3 \code{.ctl} and \code{forecast.ss} files are written
#' to the disk and the altered \code{dat_list} is returned invisibly.
#'
#' @author Kelli Johnson
#' @importFrom r4ss SS_parlines SS_readforecast SS_writeforecast
#' @export
#' @examples
#' \dontrun{
#' library(r4ss)
#' # Create a temporary folder for the output and set the working directory:
#' temp_path <- file.path(tempdir(), "ss3sim-tv-example")
#' dir.create(temp_path, showWarnings = FALSE)
#' wd <- getwd()
#' setwd(temp_path)
#' on.exit(wd, add = TRUE)
#'
#' d <- system.file("extdata", package = "ss3sim")
#' ctl_file_orig <- paste0(d, "/models/cod-om/codOM.ctl")
#' ctl_file_copy <- file.path(temp_path, "codOM.ctl")
#' file.copy(from = ctl_file_orig, to = ctl_file_copy) # copy to temp_path
#' data.old <- r4ss::SS_readdat(file.path(d, "models", "cod-om", "codOM.dat"),
#'             version = NULL, verbose = FALSE)
#' change_e(ctl_file_in = "codOM.ctl", ctl_file_out = "change_e.ctl",
#'          dat_list = data.old, for_file_in = "forecast.ss",
#'          natM_type = NULL, natM_n_breakpoints = NULL,
#'          natM_lorenzen = NULL, natM_val = NULL,
#'          par_name = c("_steep", "SizeSel_P1_Fishery(1)"),
#'          par_int = c(0.3, 40), par_phase = c(3, 2),
#'          forecast_num = 0)
#' # clean up the temporary files
#' file.remove("change_e.ctl")
#' file.remove("codOM.ctl")
#' }

change_e <- function(ctl_file_in = "em.ctl",
                     ctl_file_out = "em.ctl",
                     dat_list = NULL,
                     for_file_in = "forecasts.ss",
                     par_name = NULL,
                     par_int = "NA",
                     par_phase = "NA",
                     forecast_num = 0,
                     verbose = FALSE,
                     #below are deprecated parameters:
                     natM_type = NULL,
                     natM_n_breakpoints = NULL,
                     natM_lorenzen = NULL,
                     natM_val = NULL) {

 # provide errors if deprecated parameters are used.
  lapply(list(natM_type = natM_type, natM_n_breakpoints = natM_n_breakpoints,
                natM_lorenzen = natM_lorenzen , natM_val = natM_val),
         function(x) if(!is.null(x)) {
          stop("Parameters in change_e: natM_type, natM_n_breakpoints, ",
               "natM_lorenzen, and natM_val have been deprecated and cannot be ",
               "used. Instead, please make sure the default values NULL are used",
               " for deprecated parameters and specify your natural mortality ",
               "type within the EM model itself. If you wish to change the ",
               "value or phase of M parameter(s), please use parameters ",
               "par_name, par_init, and par_phase.")
          })
  # get the ss_version from the control file to use with r4ss functions
  ss_version <- get_ss_ver_file(ctl_file_in)
  ss3.ctl <- readLines(ctl_file_in)
  #Run external estimator for growth if needed
  if(any(grepl("change_e_vbgf", par_int))) {
    if (length(dir(pattern = "vbgf")) != 1) {
      stop("The necessary file containing \"vbgf\" does not exist in ",
           getwd(), ". Please make sure the correct data is available for the ",
           "external estimator.")
    }
    data <- read.csv(dir(pattern = "vbgf"), header = TRUE)
  #Get start values
    pars <- SS_parlines(ctl_file_in, version = ss_version, verbose = FALSE)
    change_e_vbgf <- try(
      sample_fit_vbgf(length.data = data,
        start.L1 = with(pars, INIT[Label == "L_at_Amin_Fem_GP_1"]),
        start.L2 = with(pars, INIT[Label == "L_at_Amax_Fem_GP_1"]),
        start.k  = with(pars, INIT[Label == "VonBert_K_Fem_GP_1"]),
        start.cv.young = with(pars, INIT[Label == "CV_young_Fem_GP_1"]),
        start.cv.old = with(pars, INIT[Label == "CV_old_Fem_GP_1"]),
        lo.L1 = with(pars, LO[Label == "L_at_Amin_Fem_GP_1"]),
        lo.L2 = with(pars, LO[Label == "L_at_Amax_Fem_GP_1"]),
        lo.k  = with(pars, LO[Label == "VonBert_K_Fem_GP_1"]),
        lo.cv.young = with(pars, LO[Label == "CV_young_Fem_GP_1"]),
        lo.cv.old = with(pars, LO[Label == "CV_old_Fem_GP_1"]),
        hi.L1 = with(pars, HI[Label == "L_at_Amin_Fem_GP_1"]),
        hi.L2 = with(pars, HI[Label == "L_at_Amax_Fem_GP_1"]),
        hi.k  = with(pars, HI[Label == "VonBert_K_Fem_GP_1"]),
        hi.cv.young = with(pars, HI[Label == "CV_young_Fem_GP_1"]),
        hi.cv.old = with(pars, HI[Label == "CV_old_Fem_GP_1"]),
        a3 = min(data$age), A = max(data$age)), silent = TRUE)
    #Get par estimates and append them to par_name par_int and par_phase
    changeinits <- which(par_int == "change_e_vbgf")
    keep <- sapply(par_name[changeinits], grep, names(change_e_vbgf),
      ignore.case = TRUE)
    par_int[changeinits] <- unlist(change_e_vbgf)[keep]
    par_int[!par_int %in% c(NA, "NA", "Nan")] <-
      as.numeric(par_int[!par_int %in% c(NA, "NA", "Nan")])
  }
  file.copy(ctl_file_in, ctl_file_out)


if(!is.null(par_name)) {
  par_name <- unlist(strsplit(par_name, split = ","))
  par_name_q <- grep("LnQ_", par_name, value = TRUE)
  if (length(par_name_q) > 0) {
    parsinmodel <- SS_parlines(ctlfile = ctl_file_in, dir = NULL,
      version = ss_version, verbose = verbose, active = FALSE)
    defaultq <- SS_parlines(dir = NULL,
      ctlfile = dir(pattern = "\\.ctl",
        path = system.file("extdata", "models", "cod-em", package = "ss3sim"),
        full.names = TRUE),
        version = ss_version, verbose = verbose, active = FALSE)
    defaultq <- defaultq[grep("LnQ_", defaultq$Label), ]
    fleet_q <- sapply(strsplit(par_name_q, "\\(|\\)|_"),
      function(x) x[grepl("[0-9]+", x)])
  }

  phasenochange <- is.na(par_phase)
  if(any(phasenochange)) {
    SS_changepars(dir = NULL, ctlfile = ctl_file_in,
      newctlfile = ctl_file_out,
      linenums = NULL, strings = par_name[phasenochange],
      newvals = par_int[phasenochange], repeat.vals = verbose,
      newlos = NULL, newhis = NULL, estimate = NULL, verbose = verbose,
      newphs = par_phase[phasenochange])
  }
  phaseneg <- which(par_phase < 0)
  if(length(phaseneg) > 0) {
    SS_changepars(dir = NULL, ctlfile = ctl_file_in,
      newctlfile = ctl_file_out,
      linenums = NULL, strings = par_name[phaseneg],
      newvals = par_int[phaseneg], repeat.vals = verbose,
      newlos = NULL, newhis = NULL,
      estimate = rep(FALSE, times = length(par_name[phaseneg])), verbose = verbose,
      newphs = par_phase[phaseneg])
  }
  pasepos <- which(par_phase >= 0)
  if(length(pasepos) > 0) {
    SS_changepars(dir = NULL, ctlfile = ctl_file_in,
      newctlfile = ctl_file_out,
      linenums = NULL, strings = par_name[pasepos],
      newvals = par_int[pasepos], repeat.vals = verbose,
      newlos = NULL, newhis = NULL,
      estimate = rep(TRUE, times = length(par_name[pasepos])),
      verbose = verbose,
      newphs = par_phase[pasepos])
  }
}

 if(forecast_num > 0) {
   if(is.null(dat_list)) {
     stop("A list object read in by r4ss::SS_readdat must be passed ",
          "to change_e using the dat_list argument if the user wishes to ",
          "implement or change the number of forecasts.")
   }
 if(!file.exists(for_file_in)) {
   stop("Forecast file for the estimation model does not exist.")
 }
   endyr_orig <- dat_list$endyr
   dat_list$endyr <- dat_list$endyr - forecast_num

   if (all(c("nseas", "readAll") %in% names(formals(SS_readforecast)))) {
     ss3.for <- SS_readforecast(file = for_file_in, Nfleets = dat_list$Nfleet,
     Nareas = dat_list$N_areas, version = ss_version, verbose = verbose,
     nseas = 1, readAll = TRUE)
   } else {
      ss3.for <- SS_readforecast(file = for_file_in, Nfleets = dat_list$Nfleet,
        Nareas = dat_list$N_areas, version = ss_version, verbose = verbose)
   }
   ss3.for$Forecast <- 2 #Fish at F(MSY)
   ss3.for$Nforecastyrs <- forecast_num
   # change years in forecast file to make sure they are within the model bounds.
   # (warning provided to user if changed)
   ss3.for <- change_e_fcast_yrs(dat_list$styr, endyr_orig, dat_list$endyr, ss3.for)
   SS_writeforecast(ss3.for, file = "forecast.ss", overwrite = TRUE,
     verbose = verbose)
 }
if(!is.null(dat_list)) invisible(dat_list)
}

#' Check and change forecast file years if necessary
#'
#' Check if forecast years and benchmark years within the forecast file are
#' within the model start year and end year.
#' @param styr The model start year, an integer
#' @param endyr_orig The original end year that the forecast file assumed, an
#'  integer
#' @param endyr_new The new end year, an integer
#' @param fcast_list forecast file read in using r4ss (is a list)
#' @return A changed forecast list.
change_e_fcast_yrs <- function(styr = 0,
                               endyr_orig = 100,
                               endyr_new = 100,
                               fcast_list) {
  yrs_list <- list(bmark = fcast_list$Bmark_years, fcast = fcast_list$Fcast_years)
  names_list <- names(yrs_list)
  new_yrs <- mapply(
    function(yrs, name, styr, endyr_orig, endyr_new) {
      warn <- FALSE
      yrs_orig <- yrs
      # absolute
     if(any(yrs > endyr_new)){
      indices <-  which(yrs > endyr_new)
      to_change <- yrs[indices]
      diff_from_orig <- endyr_orig - to_change
      new_vals <- endyr_new - diff_from_orig
      warning(name, " had some years that were greater than the end year.",
              "Changing these values.")
      yrs[indices] <- new_vals
      warn <- TRUE
     }
     # relative
     nyrs <- endyr_new - styr
     if(any(yrs < -nyrs)) {
       indices <- which(yrs < -nyrs)
       warning(name, " had some relative years that were out of range.",
               "Changing these values.")
       # set so is still the same distance from start value.
       val_more_than_styr <- endyr_orig - styr + 1 + yrs[indices]
       new_vals <- endyr_new - styr + 1 - val_more_than_styr
       new_vals <- -new_vals # make negative.
       yrs[indices] <- new_vals
       warn <-  TRUE
     }
     if(warn) {
       warning(name, "was changed from ", yrs_orig, " to ", yrs, ". If this ",
               "behavior is undesirable, please make sure the benchmark years ",
               "and forecast years are within the EM's styr and endyr given ",
               "the E case chosen.")
     }
     yrs
  }, yrs_list, names_list, MoreArgs = list(styr = styr,
                                           endyr_orig = endyr_orig,
                                           endyr_new = endyr_new),
  SIMPLIFY = FALSE
  )
  # replace old values in forecast list
  fcast_list$Bmark_years <- new_yrs$bmark
  fcast_list$Fcast_years <- new_yrs$fcast
  fcast_list
}
