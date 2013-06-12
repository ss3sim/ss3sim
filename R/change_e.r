#' Alter em.ctl to change which parameters are being estimated in the estimation model
#'
#' @param ctl_file_in The operating model control file to manipulate
#' @param ctl_file_out Name of the resulting operating model control file
#' @param natM_type A character value corresponding to option 1:4 in stock synthesis (i.e. "n_breakpoints", "Lorenzen", "agespecific" , "agespec_withseasinterpolate"), default is NULL which will leave the parameter as specified in the .ctl file (ie a single fixed parameter)
#' @param natM_vector A vector of ages at which you want breakpoints, only used if you specify natM_type = "n_breakpoints".
#' @param natM_int A vector of natural mortality rates for each natM parameter
#' @param natM_lorenzen The reference age for the Lorenzen function should natM_type = "Lorenzen".
#' @param natM_phase A vector of phases for each natM parameter. Leave at default if natM_type = "agespecific" or "agespec_withseasinterpolate".
#' @param steep_phase A single value that specifies the phase in which steepness is estimated.
#' @param qSurvey_phase A single value that specifies the phase in which catchability for the survey is estimated
#' @param qCPUE_phase A single value that specifies the phase in which catchability for CPUE is estimated
#' @param CV_young__phase A single parameter that specifies the phases in which to estimate the CV for size at age for young fish.
#' @param CV_old__phase A single parameter that specifies the phases in which to estimate the CV for size at age for old fish.
#' @author Kelli Johnson
#' @export
#' @examples
#' \dontrun{
#' d <- system.file("extdata", package = "ss3sim")
#' ctl_file <- paste0(d, "/run_ss3sim_eg/cod_em/simple_cod_em.ctl")
#' change_e(ctl_file_in = ctl_file, ctl_file_out = "change_e.ctl",
#'          natM_type = "n_breakpoints", natM_vector = c(1,4),
#'          natM_int = c(.2,.3), natM_lorenzen = NULL,
#'          natM_phase = c(2,2), steep_phase = -1, qSurvey_phase = NULL, qCPUE_phase = -1,
#'          CV_young_phase = NULL, CV_old_phase = NULL )
#' # clean up
#' file.remove("change_e.ctl")
#' }

change_e <- function ( ctl_file_in = "sardEM.ctl", ctl_file_out = "new.ctl",
                       natM_type = NULL, natM_vector = NULL, natM_int = NULL, 
  		       natM_lorenzen = NULL, 
		       natM_phase = -1, steep_phase = -1, qSurvey_phase = NULL, qCPUE_phase = -1,
                       CV_young_phase = NULL, CV_old_phase = NULL ) {
if ( !file.exists ( ctl_file_in ) ) stop("Ctl file for the estimation model does not exist, change_e failed.")
#Read in the ctl file for the estimation model
SS_ctl <- readLines ( ctl_file_in )
  # Copy file to ensure information is not lost
  file.copy ( from = ctl_file_in, to = "control_pre_change_e.ss_new",
              overwrite = TRUE, copy.mode = TRUE )
if ( ! natM_type == "NULL" ) {
  natM_value <- pmatch( tolower(natM_type),
                        c("n_breakpoints", "lorenzen", "agespecific", "agespec_withseasinterpolate") )
  if ( natM_value == 3 | natM_value == 4 ) {
    if ( length ( natM_phase ) > 1 ) stop ( "Cannot specify an estimation phase for natural mortality if using natM_type = agespecific or agespec_withseasinterpolate" ) }
  # change the natM type from 1Parm to correct type
    natM_type_line <- grep ( "_natM_type", SS_ctl )
    natM_type_value <- unlist(strsplit ( SS_ctl[natM_type_line], split = "" ))
    natM_type_value[1] <- natM_value
    SS_ctl[natM_type_line] <- paste(natM_type_value, collapse = "")

  # specify natM_type optional lines according to type
	natM_additionalInput <- grep ( "#_no additional input for selected M option", SS_ctl )
  if ( natM_value == 1 ) {
    natM_breakpoints = length ( natM_vector )
	 SS_ctl[natM_additionalInput] <- paste ( natM_breakpoints, 
	                                         "#_no additional input for selected M option; read 1P per morph",
											 sep = " " )
	SS_ctl <- append ( SS_ctl, 
	                   values = paste0(paste(natM_vector, collapse = " "), " # vector of age breakpoints for natM"), 
	                   after = natM_additionalInput )
	
	natM_p_FirstLine <- grep ( "# NatM_p_1_Fem_GP_1", SS_ctl )
	natM_p_value <- unlist(strsplit ( SS_ctl[natM_p_FirstLine], split = " " ) )
    # remove white space
    natM_p_value <- natM_p_value[which(nchar(natM_p_value)>0)]
      natM_p_value[3] <- natM_int[1]
	      #check to ensure that int is between the bounds
     	  if ( natM_p_value[3] > natM_p_value[2] ) natM_p_value[2] <- natM_int[1]*1.3
		  if ( natM_p_value[3] < natM_p_value[1] ) natM_p_value[1] <- natM_int[1]*.3
      SS_ctl[natM_p_FirstLine] <- paste ( natM_p_value, collapse = " " )
      for ( i in seq(natM_int) ) {
	      if ( i == 1 ) next
          natM_p_value[3] <- natM_int[i]
	        #check to ensure that int is between the bounds
		    if ( natM_p_value[3] > natM_p_value[2] ) natM_p_value[2] <- natM_int[i]*1.3
		    if ( natM_p_value[3] < natM_p_value[1] ) natM_p_value[1] <- natM_int[i]*.3
		  natM_p_value[grep ( "GP", natM_p_value )] <- paste0("NatM_p_1_Fem_GP_",i)
		  SS_ctl <- append ( SS_ctl, values = paste ( natM_p_value, collapse = " " ),
		           after = (natM_p_FirstLine+i-2) )
		}
  }
  if ( natM_value == 2 ) {
    SS_ctl[natM_additionalInput] <- paste ( natM_lorenzen, 
	                                        "#_no additional input for selected M option; read 1P per morph",
											sep = " " )
  }
  if ( natM_value == 3 | natM_value == 4 ) {
  	 SS_ctl[natM_additionalInput] <- paste ( natM_int[1], 
	                                         "#_read 1P per morph_1",
											 sep = " " )
	    for ( z in rev(seq(natM_int)) ) {
		     if ( z == 1 ) next
	         SS_ctl <- append ( SS_ctl, values = paste0(natM_int[z], "#_read 1P per morph_", z), 
	                  after = natM_additionalInput )
		}
  }

}

if ( length( which( natM_phase > 0 ) > 0 ) ) {
  natM_phase_line <- grep ( "# NatM_p_", SS_ctl )
  #_LO HI INIT PRIOR PR_type SD PHASE
  natM_phase_value <- lapply (SS_ctl[natM_phase_line], function(x) unlist(strsplit ( x, split = " " ) ) )
  # remove white space
  natM_phase_value <- sapply ( natM_phase_value, function (x) x[which(nchar(x)>0)], simplify = TRUE )  
  natM_phase_value[7,] <- natM_phase
  SS_ctl[natM_phase_line] <- apply ( natM_phase_value, 2, paste, collapse = " " )
}
if ( steep_phase > 0 ) {
  steep_line <- grep ( "_steep", SS_ctl )
  #_LO HI INIT PRIOR PR_type SD PHASE
  steep_value <- unlist(strsplit ( SS_ctl[steep_line], split = " " ) )
  # remove white space
  steep_value <- steep_value[which(nchar(steep_value)>0)]
  steep_value[7] <- steep_phase
  SS_ctl[steep_line] <- paste( steep_value, collapse = " " )
}
if ( !qSurvey_phase == "NULL" ) {
  qSurvey_line <- grep ( "LnQ_base_2_SURVEY", SS_ctl )
  #_LO HI INIT PRIOR PR_type SD PHASE
  qSurvey_value <- unlist(strsplit ( SS_ctl[qSurvey_line], split = " " ) )
  # remove white space
  qSurvey_value <- qSurvey_value[which(nchar(qSurvey_value)>0)]
  qSurvey_value[7] <- qSurvey_phase
  SS_ctl[qSurvey_line] <- paste( qSurvey_value, collapse = " " )
}
if ( !qCPUE_phase == "NULL" ) {
  qCPUE_line <- grep ( "LnQ_base_3_CPUE", SS_ctl )
  #_LO HI INIT PRIOR PR_type SD PHASE
  qCPUE_value <- unlist(strsplit ( SS_ctl[qCPUE_line], split = " " ) )
  # remove white space
  qCPUE_value <- qCPUE_value[which(nchar(qCPUE_value)>0)]
  qCPUE_value[7] <- qCPUE_phase
  SS_ctl[qCPUE_line] <- paste( qCPUE_value, collapse = " " )
}
if ( !CV_young_phase == "NULL" ) {
  CV_young_line <- grep ( "CV_young", SS_ctl )
  #_LO HI INIT PRIOR PR_type SD PHASE
  CV_young_value <- unlist(strsplit ( SS_ctl[CV_young_line], split = " " ) )
  # remove white space
  CV_young_value <- CV_young_value[which(nchar(CV_young_value)>0)]
  CV_young_value[7] <- CV_young_phase
  SS_ctl[CV_young_line] <- paste( CV_young_value, collapse = " " )
}
if ( !CV_old_phase == "NULL" ) {
  CV_old_line <- grep ( "CV_old", SS_ctl )
  #_LO HI INIT PRIOR PR_type SD PHASE
  CV_old_value <- unlist(strsplit ( SS_ctl[CV_old_line], split = " " ) )
  # remove white space
  CV_old_value <- CV_old_value[which(nchar(CV_old_value)>0)]
  CV_old_value[7] <- CV_old_phase
  SS_ctl[CV_old_line] <- paste( CV_old_value, collapse = " " )
}
  writeLines ( SS_ctl, ctl_file_out )
}
