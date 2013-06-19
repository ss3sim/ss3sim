#' Alter em.ctl to change which parameters are being estimated in the
#' estimation model
#'
#' @param ctl_file_in The operating model control file to manipulate
#' @param ctl_file_out Name of the resulting operating model control file
#' @param natM_type A character value corresponding to option 0:4 in
#'                  stock synthesis (i.e. "1Parm", "n_breakpoints",
#'                  "Lorenzen", "agespecific", "agespec_withseasinterpolate"),
#'                  NULL will leave natM as specified in em.ctl
#' @param natM_n_breakpoints A vector of ages at which you want breakpoints,
#'                           only used if you specify natM_type = "n_breakpoints".
#' @param natM_lorenzen The reference age for the Lorenzen function
#'                      should natM_type = "Lorenzen".
#' @param natM_val Vector of int and phase values for each natM parameter.
#' @param par_name Vector of values, separated by commas. The values will later be
#'                 turned into character values and used to search for
#'                 specific lines for each parameter in the ctl file 
#' @param par_int Vector of intial values, one for each parameter in par_name,
#'                value can be NA if you do not wish to change the int.
#' @param par_phase Vector of phase values, one for each parameter in par_name,
#'                  value can be NA if you do not wish to change the phase.
#' @author Kelli Johnson
#' @export
#' @examples
#' \dontrun{
#' d <- system.file("extdata", package = "ss3sim")
#' ctl_file <- paste0(d, "/run_ss3sim_eg/cod_em/simple_cod_em.ctl")
#' change_e(ctl_file_in = ctl_file, ctl_file_out = "change_e.ctl",
#'          natM_type = "n_breakpoints", natM_n_breakpoints = c(1,4),
#'          natM_lorenzen = NULL, natM_val = c(.2,3,.4,5),
#'          par_name = c("steep", "SizeSel_1P_1_Fishery"),
#'          par_int = c(.3, 40), par_phase = c(3,2)  )
#' # clean up
#' file.remove("change_e.ctl")
#' }

change_e <- function ( ctl_file_in = pastef("em.ctl"), ctl_file_out = pastef("em.ctl"), 
   natM_type = "1Parm", natM_n_breakpoints = NULL, natM_lorenzen = NULL, natM_val = c(NA,NA),
   par_name = NULL, par_int = "NA", par_phase = "NA"  ) {
if ( !file.exists ( ctl_file_in ) ) stop("Ctl file for the estimation model does not exist, change_e failed.")
#Read in the ctl file for the estimation model
SS_ctl <- readLines ( ctl_file_in )
  # Copy file to ensure information is not lost
  file.copy ( from = ctl_file_in, to = "control_pre_change_e.ss_new",
              overwrite = TRUE, copy.mode = TRUE )
  natM_value <- pmatch( tolower(natM_type),
                        c("1parm", "n_breakpoints", "lorenzen", "agespecific", "agespec_withseasinterpolate") ) - 1
if ( !is.na(natM_value) ) {
  # change the natM type from 1Parm to correct type
    natM_type_line <- grep ( "_natM_type", SS_ctl )
      natM_type_value <- unlist(strsplit ( SS_ctl[natM_type_line], split = "" ))
      natM_type_value[1] <- natM_value
    SS_ctl[natM_type_line] <- paste(natM_type_value, collapse = "")

  # specify natM optional lines according to type
	natM_additionalInput <- grep ( "#_no additional input for selected M option", SS_ctl )
  if ( natM_value == 1 ) {
    natM_breakpoints = length ( natM_n_breakpoints )
	 SS_ctl[natM_additionalInput] <- paste ( natM_breakpoints, "#_N_breakpoints" )
	 SS_ctl <- append ( SS_ctl, 
	                    values = paste0(paste(natM_n_breakpoints, collapse = " "), " # age(real) at M breakpoints"), 
	                    after = natM_additionalInput )
	}
  if ( natM_value == 2 ) SS_ctl[natM_additionalInput] <- paste ( natM_lorenzen, "#_reference age for Lorenzen M" )
  if ( natM_value == 3 | natM_value == 4 ) {
     SS_ctl[natM_additionalInput] <- paste0 ( " #_Age_natmort_by gender x growthpattern" )
     SS_ctl <- append ( SS_ctl, values = paste( natM_val, collapse = " " ), 
                        after = natM_additionalInput )
     SS_ctl <- SS_ctl[ -grep ( "# NatM_p_1_Fem_GP_1", SS_ctl ) ]
    }
  # specify the initial and phase values for natM if used
  if ( natM_value == 0 | natM_value == 1 | natM_value == 2 ) {
   	 natM_p_FirstLine <- grep ( "# NatM_p_1_Fem_GP_1", SS_ctl )
 	 natM_p_value <- unlist(strsplit ( SS_ctl[natM_p_FirstLine], split = " " ) )
     # remove white space
     natM_p_value <- natM_q_value <- natM_p_value[which(nchar(natM_p_value)>0)]
     counter <- 0
       for ( i in seq(natM_val) ) {
 	      if ( i %% 2 == 0 ) next
 		  natM_p_value <- natM_q_value
           if ( !is.na(natM_val[i]) ) {
             natM_p_value[3] <- natM_val[i]
 	        #check to ensure that int is between the bounds
 		    if ( as.numeric(natM_p_value[3]) > as.numeric(natM_p_value[2]) ) natM_p_value[2] <- natM_val[i]*1.3
 		    if ( as.numeric(natM_p_value[3]) < as.numeric(natM_p_value[1]) ) natM_p_value[1] <- natM_val[i]*.3
 			}
 		  if ( !is.na(natM_val[i+1]) ) natM_p_value[7] <- natM_val[i+1]
 		  counter <- counter + 1
 		  natM_p_value[grep ( "GP", natM_p_value )] <- paste0("NatM_p_1_Fem_GP_",counter)
		  if ( i == 1 ) SS_ctl[natM_p_FirstLine] <- paste ( natM_p_value, collapse = " " )
 		   else {SS_ctl <- append ( SS_ctl, values = paste ( natM_p_value, collapse = " " ),
 		                            after = (natM_p_FirstLine+counter-2) )}
 		}
}
}

changeMe <- function ( grepChar, intVal, phaseVal, ctlIn = SS_ctl ) {
  grepChar_line <- grep ( grepChar, SS_ctl )
  #_LO HI INIT PRIOR PR_type SD PHASE
  grepChar_value <- unlist(strsplit ( SS_ctl[grepChar_line], split = " " ) )
  # remove white space
  grepChar_value <- grepChar_value[which(nchar(grepChar_value)>0)]
  if ( !is.na(intVal) ) grepChar_value[3] <- intVal
   		    if ( as.numeric(grepChar_value[3]) > as.numeric(grepChar_value[2]) ) grepChar_value[2] <- intVal*1.3
 		    if ( as.numeric(grepChar_value[3]) < as.numeric(grepChar_value[1]) ) grepChar_value[1] <- intVal*.3
  if ( !is.na(phaseVal) ) grepChar_value[7] <- phaseVal
  SS_ctl[grepChar_line] <- paste( grepChar_value, collapse = " " )
  return(SS_ctl)
}
 par_name <- unlist(strsplit(par_name,split=","))
 for ( y in seq(par_name) ) {
 SS_ctl <- changeMe ( grepChar = par_name[y], intVal = par_int[y], phaseVal = par_phase[y] )
 }

 writeLines ( SS_ctl, ctl_file_out )
}
