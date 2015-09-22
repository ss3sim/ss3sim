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
#' @param ctl_file_in Input SS3 control file
#' @param ctl_file_out Output SS3 control file
#' @template dat_list
#' @param for_file_in Input SS3 forecast file
#' @param natM_type *A character string corresponding to option 0:4 in SS3 (i.e.
#'   "1Parm", "n_breakpoints", "Lorenzen", "agespecific",
#'   "agespec_withseasinterpolate"). A value of \code{NA} will leave the
#'   configuration of natural mortality as specified in \code{ctl_file_in}.
#' @param natM_n_breakpoints *A vector of ages at which you want breakpoints.
#'   Only used if you specify \code{natM_type = "n_breakpoints"}.
#' @param natM_lorenzen *The reference age for the Lorenzen function.  Only used
#'   if you specify \code{natM_type = "Lorenzen"}. Length should be one even
#'   if the \code{.ctl} has two genders.
#' @param natM_val *A vector of numeric values. Interpretation of the values are
#'   dependent upon \code{natM_type}. If \code{natM_type = "agespecific"} or
#'   \code{natM_type = "agespec_withseasinterpolate"} the vector specifies the
#'   fixed natural mortality parameters for each integer age.  Specify values
#'   in the following order: female area 1, female area 2, male area 1, male
#'   area, etc. Ensure that there is one entry per integer age x area x gender.
#'   If \code{natM_type = "1Param"}, \code{natM_type = "n_breakpoints"}, or
#'   \code{natM_type = "Lorenzen"} the vector specifies the initial and phase
#'   values for each natM parameter (i.e. \code{c(int, phase, int, phase,
#'   etc.)}, where the first two values could correspond to ages 0-2 natural
#'   mortality and the third and fourth value could correspond to ages 3-8
#'   natural mortality).  For any specified initial value, the parameter bounds
#'   will be altered to 50 percent above and below the specified initial value,
#'   if the initial value lies above or below the original bounds.
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
#' @param run_change_e_full *If \code{FALSE} \code{change_e} will only
#'   manipulate for forecasting, if \code{TRUE} (default) the full function
#'   capability will be ran.
#' @template verbose
#'
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
#'
#' d <- system.file("extdata", package = "ss3sim")
#' ctl_file <- paste0(d, "/models/cod-om/codOM.ctl")
#' data.old <- r4ss::SS_readdat(file.path(d, "models", "cod-om", "codOM.dat"))
#' change_e(ctl_file_in = ctl_file, ctl_file_out = "change_e.ctl",
#'          dat_list = data.old, for_file_in = "forecast.ss",
#'          natM_type = "n_breakpoints", natM_n_breakpoints = c(1, 4),
#'          natM_lorenzen = NULL, natM_val = c(.2, 3, 0.4, 5),
#'          par_name = c("_steep", "SizeSel_1P_1_Fishery"),
#'          par_int = c(0.3, 40), par_phase = c(3, 2),
#'          forecast_num = 0, run_change_e_full = TRUE )
#' # clean up
#' file.remove("change_e.ctl")
#' setwd(wd)
#' }

change_e <- function(ctl_file_in = "em.ctl",
    ctl_file_out = "em.ctl", dat_list = NULL,
    for_file_in = "forecasts.ss", natM_type = "1Parm",
    natM_n_breakpoints = NULL, natM_lorenzen = NULL, natM_val = c(NA, NA),
    par_name = NULL, par_int = "NA", par_phase = "NA",
    forecast_num = 0, run_change_e_full = TRUE,
    verbose = FALSE) {

  if (!run_change_e_full & any(grepl("change_e_vbgf", par_int))) {
    run_change_e_full <- TRUE
  }
  if(run_change_e_full) {
  if(!file.exists(ctl_file_in)) {
    stop("Ctl file for the estimation model does not exist change_e failed.")
  }
  #Read in the ctl file for the estimation model
  ss3.ctl <- readLines(ctl_file_in)
  #Run external estimator for growth if needed
  if(any(grepl("change_e_vbgf", par_int))) {
    if (length(dir(pattern = "vbgf")) != 1) {
      stop(paste("The necessary file containing \"vbgf\" does not exist in",
        getwd(), "Please make sure the correct data is available for the",
        "external estimator."))
    }
    data <- read.csv(dir(pattern = "vbgf"), header = TRUE)
  #Get start values
    pars <- SS_parlines(ctl_file_in, verbose = FALSE)
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
  # Determine how many genders the model has
  gen <- grep("NatM", ss3.ctl, value = TRUE)
  male <- TRUE %in% grepl("Mal", gen)

  natM.val <- pmatch(tolower(natM_type),
                     c("1parm", "n_breakpoints", "lorenzen",
                       "agespecific", "agespec_withseasinterpolate")) - 1
  if(!is.na(natM.val)) {
    # change the natM type from 1Parm to correct type
    natM.type.line <- grep("_natM_type", ss3.ctl)
    natM.type.val <- strsplit(ss3.ctl[natM.type.line], "#")
    natM.type.org <- as.numeric(natM.type.val[[1]][1])
    natM.type.val[[1]][1] <- natM.val
    ss3.ctl[natM.type.line] <- paste(natM.type.val[[1]], collapse = " #")
    if(natM.type.org > 0) {
      stop(paste("change_e can only change a .ctl from a simple 1 parameter",
                 "natural mortality type to a more complicated parameter",
                 "setup and not the other way around."))
    }
    # specify natM optional lines according to type
  	natM.input <- grep("#_no additional input for selected M option",
                       ss3.ctl)
    if(natM.val == 1) {
      natM_breakpoints <- length(natM_n_breakpoints)
      if(male == FALSE & !(length(natM_val) == (natM_breakpoints * 2))) {
        stop(paste("Must specify a int and phase for each natural mortality",
                   "parameter in the vector natM_val."))
      }
      if(male == TRUE & !(length(natM_val) == (natM_breakpoints * 2 * 2))) {
        stop(paste("Must specify an int and phase for each M parameter",
                   "in the vector natM_val. This model has two genders",
                   "thus for each breakpoint there must be four entries in,",
                   "natM_val (i.e., int_female_1, phase_female_1, int_female_2,",
                   "phase_female_2, int_male_1, phase_male_1, etc.)."))
      }
  	  ss3.ctl[natM.input] <- paste(natM_breakpoints, "#_N_breakpoints")
  	  ss3.ctl <- append(ss3.ctl,
  	                   values = paste0(paste(natM_n_breakpoints, collapse = " "),
                                       " # age(real) at M breakpoints"),
  	                   after = natM.input)
  	}
    if(natM.val == 2) {
      if(length(natM_lorenzen) > 1) {
        stop(paste("SS3, version O, uses a single age value for the Lorenzen",
                   "function even if there is > 1 gender.",
                   "length(natM_lorenzen) == 1, not", length(natM_lorenzen)))
      }
      ss3.ctl[natM.input] <- paste(natM_lorenzen,
                                  "#_reference age for Lorenzen M")
    }
    if(natM.val == 3 | natM.val == 4) {
       ss3.ctl[natM.input] <- paste0(" #_Age_natmort_by gender x growthpattern")
       ss3.ctl <- append(ss3.ctl, values = paste(natM_val, collapse = " "),
                        after = natM.input)
       ss3.ctl <- ss3.ctl[-grep("# NatM_", ss3.ctl)]
      }
    # specify the initial and phase values for natM if used
    if(natM.val == 0 | natM.val == 1 | natM.val == 2) {
       natM.p.line1f <- grep("# NatM_p_1_Fem_GP_1", ss3.ctl)
   	   natM.p.valf <- unlist(strsplit(ss3.ctl[natM.p.line1f], split = " "))
       natM.p.valf <- natM.p.valf[which(nchar(natM.p.valf) > 0)]
       if(male) {
         natM.p.line1m <- grep("# NatM_p_1_Mal_GP_1", ss3.ctl)
   	     natM.p.valm <- unlist(strsplit(ss3.ctl[natM.p.line1m], split = " "))
         natM.p.valm <- natM.p.valm[which(nchar(natM.p.valm) > 0)]
       }
       counter <- 0
       seq.len <- length(natM_val)
       if(male) seq.len <- seq.len / 2
         for(i in 1:seq.len) {
   	      if(i %% 2 == 0) next
             if(!is.na(natM_val[i])) {
               natM.p.valf[3] <- natM_val[i]
   	           #check to ensure that int is between the bounds
   		       if(as.numeric(natM.p.valf[3]) < as.numeric(natM.p.valf[1])) {
                 natM.p.valf[1] <- natM_val[i] * 0.5
   		       }
               if(as.numeric(natM.p.valf[3]) > as.numeric(natM.p.valf[2])) {
                 natM.p.valf[2] <- natM_val[i] * 1.5
               }
   			 }
   		  if(!is.na(natM_val[i + 1])) {
            natM.p.valf[7] <- natM_val[i + 1]
   		  }
          if(male) {
            if(!is.na(natM_val[i + seq.len])) {
               natM.p.valm[3] <- natM_val[i + seq.len]
   	           #check to ensure that int is between the bounds
   		       if(as.numeric(natM.p.valm[3]) < as.numeric(natM.p.valm[1])) {
                 natM.p.valm[1] <- natM_val[i + seq.len] * 0.5
   		       }
               if(as.numeric(natM.p.valm[3]) > as.numeric(natM.p.valm[2])) {
                 natM.p.valm[2] <- natM_val[i + seq.len] * 1.5
               }
   			 }
   		  if(!is.na(natM_val[i + seq.len + 1])) {
            natM.p.valm[7] <- natM_val[i + seq.len + 1]
   		  }
          }
          counter <- counter + 1
   		  natM.p.valf[grep("GP", natM.p.valf)] <- paste0("NatM_p_1_Fem_GP_", counter)
          if(male) natM.p.valm[grep("GP", natM.p.valm)] <- paste0("NatM_p_1_Mal_GP_", counter)
  		  if(i == 1) {
            ss3.ctl[natM.p.line1f] <- paste(natM.p.valf, collapse = " " )
            if(male) ss3.ctl[natM.p.line1m] <- paste(natM.p.valm, collapse = " " )
   		  } else {
              ss3.ctl <- append(ss3.ctl, values = paste(natM.p.valf, collapse = " " ),
   		                       after = (natM.p.line1f + counter - 2))
              if(male) {
                natM.p.line1m <- grep("NatM_p_1_Mal_GP_1", ss3.ctl)
                ss3.ctl <- append(ss3.ctl, values = paste(natM.p.valm, collapse = " " ),
   		                         after = (natM.p.line1m + counter - 2))
              }
              }
   		}
  }
  writeLines(ss3.ctl, ctl_file_out)
} else {
  file.copy(ctl_file_in, ctl_file_out)
}

if(!is.null(par_name)) {
  par_name <- unlist(strsplit(par_name, split = ","))
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
      newlos = NULL, newhis = NULL, estimate = FALSE, verbose = verbose,
      newphs = par_phase[phaseneg])
  }
  pasepos <- which(par_phase >= 0)
  if(length(pasepos) > 0) {
    SS_changepars(dir = NULL, ctlfile = ctl_file_in,
      newctlfile = ctl_file_out,
      linenums = NULL, strings = par_name[pasepos],
      newvals = par_int[pasepos], repeat.vals = verbose,
      newlos = NULL, newhis = NULL, estimate = TRUE, verbose = verbose,
      newphs = par_phase[pasepos])
  }
}
}
 if(forecast_num > 0) {
   if(is.null(dat_list)) {
     stop(paste("A list object read in by r4ss::SS_readdat must be passed",
       "to change_e using the dat_list argument if the user wishes to",
       "implement or change the number of forecasts."))
   }
 if(!file.exists(for_file_in)) {
   stop("Forecast file for the estimation model does not exist.")
 }
   dat_list$endyr <- dat_list$endyr - forecast_num

   ss3.for <- SS_readforecast(file = for_file_in, Nfleets = dat_list$Nfleet,
     Nareas = dat_list$N_areas, verbose = verbose)
   ss3.for$Forecast <- 2 #Fish at F(MSY)
   ss3.for$Nforecastyrs <- forecast_num
   SS_writeforecast(ss3.for, file = "forecast.ss", overwrite = TRUE,
     verbose = verbose)
 }
if(!is.null(dat_list)) invisible(dat_list)
}
