#' Methods to alter which parameters are estimated in a SS3 \code{.ctl} file.
#'
#' @description Takes SS3 \code{.ctl}, \code{.dat}, and \code{forecast.ss} files
#' and changes which parameters are estimated, how natural mortality is
#' estimated, and if forecasts are performed. The function can be called by
#' itself or within \link{run_ss3sim} to alter an estimation model \code{.ctl}
#' file.
#'
#' @param ctl_file_in Input SS3 control file
#' @param ctl_file_out Output SS3 control file
#' @param dat_file_in Input SS3 data file
#' @param for_file_in Input SS3 forecast file
#' @param natM_type *A character string corresponding to option 0:4 in SS3 (i.e.
#'   "1Parm", "n_breakpoints", "Lorenzen", "agespecific",
#'   "agespec_withseasinterpolate").  A value of NA will leave the
#'   configuration of natural mortality as specified in \code{ctl_file_in}.
#' @param natM_n_breakpoints *A vector of ages at which you want breakpoints.
#'   Only used if you specify \code{natM_type = "n_breakpoints"}.
#' @param natM_lorenzen *The reference age for the Lorenzen function.  Only used
#'   if you specify \code{natM_type = "Lorenzen"}.  Length should be one even
#'   if the \code{.ctl} has two genders.
#' @param natM_val *A vector of numeric values. Interpretation of the values are
#'   dependent upon \code{natM_type}. If \code{natM_type = "agespecific"} or
#'   \code{natM_type = "agespec_withseasinterpolate"} the vector specifies the
#'   fixed natural mortality parameters for each integer age.  Specify values
#'   in the following order: female area 1, female area 2, male area 1, male
#'   area, etc. Ensure that there is one entry per integer age x area x gender.
#'   If \code{natM_type = "1Param"}, \code{natM_type = "n_breakpoints"}, or
#'   \code{natM_type = "Lorenzen"} the vector specifies the intitial and phase
#'   values for each natM parameter (i.e. \code{c(int, phase, int, phase,
#'   etc.)}, where the first two values could correspond to ages 0-2 natural
#'   mortality and the third and fourth value could correspond to ages 3-8
#'   natural mortality).  For any specified intial value, the parameter bounds
#'   will be altered to 50 percent above and below the specified initial value,
#'   if the initial value lies above or below the original bounds.
#' @param par_name *A vector of values, separated by commas.  Each value
#'   corresponds to a parameter that you wish to turn on or off in the
#'   \code{ctl_file_in}. The values will later be turned into character values
#'   and used to search for specific lines for each parameter in the
#'   \code{ctl_file_in}, therefore it is best to use full parameter names as
#'   they are specified in \code{ctl_file_in}.
#' @param par_int *A vector of intial values, one for each parameter in
#'   \code{par_name}.  Values can be NA if you do not wish to change the
#'   initial value for a given parameter.
#' @param par_phase *A vector of phase values, one for each parameter in
#'   \code{par_name}.  Values can be NA if you do not wish to change the phase
#'   for a given parameter.
#' @param forecast_num *Number of years to perform forecasts. For those years,
#'   the data will be removed from the \code{dat_file_in}, enabling SS3 to
#'   generate forecasts rather than use the data to fit the model.
#' @param run_change_e_full *If \code{FALSE} \code{change_e} will only
#'   manipulate for forecasting, if \code{TRUE} (default) the full function
#'   capability will be ran.
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
#' @return
#' Altered versions of SS3 \code{.ctl}, \code{.dat}, and, \code{forecast.ss}
#' files.
#'
#' @author Kelli Johnson
#' @export
#' @examples
#' \dontrun{
#' # Create a temporary folder for the output and set the working directory:
#' temp_path <- file.path(tempdir(), "ss3sim-tv-example")
#' dir.create(temp_path, showWarnings = FALSE)
#' wd <- getwd()
#' setwd(temp_path)
#'
#' d <- system.file("extdata", package = "ss3sim")
#' ctl_file <- paste0(d, "/models/cod-om/codOM.ctl")
#' change_e(ctl_file_in = ctl_file, ctl_file_out = "change_e.ctl",
#'          dat_file_in = "ss3.dat", for_file_in = "forecast.ss",
#'          natM_type = "n_breakpoints", natM_n_breakpoints = c(1, 4),
#'          natM_lorenzen = NULL, natM_val = c(.2, 3, 0.4, 5),
#'          par_name = c("_steep", "SizeSel_1P_1_Fishery"),
#'          par_int = c(0.3, 40), par_phase = c(3, 2),
#'          forecast_num = 0, run_change_e_full = TRUE )
#' # clean up
#' file.remove("change_e.ctl")
#' setwd(wd)
#' }

change_e <- function(ctl_file_in = pastef("em.ctl"),
    ctl_file_out = pastef("em.ctl"), dat_file_in = pastef("ss3.dat"),
    for_file_in = "forecasts.ss", natM_type = "1Parm",
    natM_n_breakpoints = NULL, natM_lorenzen = NULL, natM_val = c(NA,NA),
    par_name = NULL, par_int = "NA", par_phase = "NA",
    forecast_num = 0, run_change_e_full = TRUE) {

  if(run_change_e_full) {
  if(!file.exists(ctl_file_in)) {
    stop("Ctl file for the estimation model does not exist,
          change_e failed.")
  }
  #Read in the ctl file for the estimation model
  ss3.ctl <- readLines(ctl_file_in)
  #Run external estimator for growth if needed
  if(any(grepl("change_e_vbgf", par_int))) {
    data <- read.csv("vbgf_info.csv", header = TRUE)[, -1]
    data.all <- r4ss::SS_readdat(dat_file_in, verbose = FALSE)
    if(!"MeanSize_at_Age_obs" %in% names(data.all)) {
      stop("Error in change_e while computing external growth estimates: dat file does not contain mean size-at-age data.")
    }
    true.cv <- unlist(strsplit(grep("CV_young", ss3.ctl, value = TRUE), " "))
    true.cv <- as.numeric(true.cv[-(which(true.cv == ""))][3])
  #Get start values
    parsmatch <- data.frame("true" = c("L_at_Amin_Fem_GP_1", "L_at_Amax_Fem_GP_1",
                                       "VonBert_K_Fem_GP_1", "CV_young_Fem_GP_1",
                                       "CV_old_Fem_GP_1"),
                            "change_e_vbgf" = c("L1", "L2", "K", "cv.young", "cv.old"))
    pars <- r4ss::SS_parlines(ctl_file_in, verbose = FALSE)
    start.pars <- sapply(parsmatch$true, function(x) {
      # On 20141031 SA changed the following from subset() to avoid R CMD check warnings:
      # temp <- subset(pars, Label == x, select = INIT)
      temp <- pars[pars$Label == x, ]["INIT"]
      names(temp) <- parsmatch$change_e_vbgf[substitute(x)[[3]]]
      return(temp)
      })
    limitages <- list(a3 = grep("Growth_Age_for_L1", ss3.ctl, value = TRUE),
                      #A = grep("Growth_Age_for_L2", ss3.ctl, value = TRUE))
                       A = as.numeric(gsub("N_a", "", rev(colnames(data.all$MeanSize_at_Age_obs))[1])))
    limitages[1] <- lapply(limitages[1], function(x) {
      temp <- unlist(strsplit(x, split = " "))
      as.numeric(temp[which(nchar(temp) > 0)][1])
      })

    change_e_vbgf <-
      sample_fit_vbgf(length.data = data, start.L1 = start.pars$L1,
        start.L2 = start.pars$L2, start.k = start.pars$K,
        start.cv.young = start.pars$cv.young, start.cv.old = start.pars$cv.old,  a3 = limitages$a3, A = limitages$A)
    #TODO: do something if the estimation routine fails
    #Get par estimates and append them to par_name par_int and par_phase
    changeinits <- which(par_int == "change_e_vbgf")
    ss3names <- par_name[changeinits]
    #This is if the output is parameter names
    par_int[changeinits] <- as.numeric(unlist(change_e_vbgf[
      match(parsmatch$change_e_vbgf[sapply(ss3names, grep, parsmatch$true)],
            names(change_e_vbgf))]))

      par_int <- sapply(par_int, function(x) {
        if(!is.na(x)) {as.numeric(x)
        }else{x}
        })

    ####TODO: remove next three lines once sample function is written
    data.all$MeanSize_at_Age_obs <- NULL
    data.all$N_MeanSize_at_Age_obs <- 0
    r4ss::SS_writedat(data.all, dat_file_in, verbose = FALSE, overwrite = TRUE)
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
      stop("change_e can only change a .ctl from a simple
            1 parameter natural mortality type
            to a more complicated parameter setup
            and not the other way around.")
    }
    # specify natM optional lines according to type
  	natM.input <- grep("#_no additional input for selected M option",
                       ss3.ctl)
    if(natM.val == 1) {
      natM_breakpoints <- length(natM_n_breakpoints)
      if(male == FALSE & !(length(natM_val) == (natM_breakpoints * 2))) {
        stop("Must specify a int and phase for each natural mortality
              parameter in the vector natM_val.")
      }
      if(male == TRUE & !(length(natM_val) == (natM_breakpoints * 2 * 2))) {
        stop("Must specify a int and phase for each natural mortality
              parameter in the vector natM_val. This model has two genders,
              therefore for each breakpoint there must be four entries in
              natM_val (i.e. int_female_1, phase_female_1, int_female_2,
              phase_female_2, int_male_1, phase_male_1, etc.).")
      }
  	  ss3.ctl[natM.input] <- paste(natM_breakpoints, "#_N_breakpoints")
  	  ss3.ctl <- append(ss3.ctl,
  	                   values = paste0(paste(natM_n_breakpoints, collapse = " "),
                                       " # age(real) at M breakpoints"),
  	                   after = natM.input)
  	}
    if(natM.val == 2) {
      if(length(natM_lorenzen) > 1) {
        stop("SS3, version O, uses a single age value for the Lorenzen
              function even if there are multiple genders. Modify the
              function call so that natM_lorenzen is of length = 1")
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
}

changeMe <- function(grepChar, intVal, phaseVal, ctlIn = ss3.ctl) {
  val <- grep(pattern = grepChar, x = ctlIn, fixed = TRUE)[1]
    if(is.na(val)) {
      stop(paste("Could not locate the parameter",
                 grepChar, "in the .ctl file.",
                 "Check that the parameter is spelled",
                 "correctly and in the correct case.",
                 "Have you standardized your .ctl file",
                 "by running it through SS and used the control.ss_new file?"))
    }
  grepChar_line <- grep(grepChar, ss3.ctl, fixed = TRUE)
  grepChar_value <- unlist(strsplit(ss3.ctl[grepChar_line], split = " "))
  # remove white space
  grepChar_value <- grepChar_value[which(nchar(grepChar_value) > 0)]
  if(!is.na(intVal)) {
    if(class(intVal) == "character") intVal <- as.numeric(intVal)
    grepChar_value[3] <- intVal
  }
  	    if(as.numeric(grepChar_value[3]) > as.numeric(grepChar_value[2])) grepChar_value[2] <- intVal*1.5
 		    if(as.numeric(grepChar_value[3]) < as.numeric(grepChar_value[1])) grepChar_value[1] <- intVal*.5
  if(!is.na(phaseVal)) grepChar_value[7] <- phaseVal
  ss3.ctl[grepChar_line] <- paste(grepChar_value, collapse = " ")
  return(ss3.ctl)
}
if(!is.null(par_name)) {
   par_name <- unlist(strsplit(par_name, split = ","))
   for(y in seq(par_name)) {
   ss3.ctl <- changeMe(grepChar = par_name[y], intVal = par_int[y], phaseVal = par_phase[y])
   }
   writeLines(ss3.ctl, ctl_file_out)
}
}
 if(forecast_num > 0) {
 if(!file.exists(dat_file_in)) {
   stop("Data file for the estimation model does not exist.")
 }
 if(!file.exists(for_file_in)) {
   stop("Forecast file for the estimation model does not exist.")
 }
 ss3.dat <- readLines(dat_file_in)
      data_end_line <- grep("#_endyr", ss3.dat)
      data_end_value <- unlist(strsplit(ss3.dat[data_end_line], split = " "))
      data_end_value[1] <- as.numeric(data_end_value[1]) - forecast_num
      ss3.dat[data_end_line] <- paste(data_end_value, collapse = "")
      writeLines(ss3.dat, "ss3.dat")
 ss3.for <- readLines(for_file_in)
      for_forecast_line <- grep("Forecast: 0=none;", ss3.for)
      for_forecast_value <- unlist(strsplit(ss3.for[for_forecast_line], split = ""))
      for_forecast_value[1] <- 2
      ss3.for[for_forecast_line] <- paste(for_forecast_value, collapse = "")

      for_number_line <- grep("N forecast years", ss3.for)
      for_number_value <- unlist(strsplit(ss3.for[for_number_line], split = ""))
      for_number_value[1] <- forecast_num
      ss3.for[for_number_line] <- paste(for_number_value, collapse = "")

      writeLines(ss3.for, "forecast.ss")
 }

}
