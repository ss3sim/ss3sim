#' Function to modify selectivity parameters in \code{.ctl} files.
#'
#' @description Takes ss3 \code{.ctl} files and changes the selectivity parameters.
#' The function can be called by itself or within \code{\link{run_ss3sim}} to alter 
#' an operating and estimation model \code{.ctl} files. If used with 
#' \code{\link{run_ss3sim}} the case file should be named \code{P}. 
#' A suggested (default) case letter is \code{P} for selex Parameters 
#' (OK NOT GREAT).
#'
#' @param om_dir Directory to locate SS3 files
#' @param om_ctl_file_in  Input SS3 control file
#' @param om_ctl_file_out Output SS3 control file
#' @param par_file_in Input SS3 parameter file
#' @param par_file_out Output SS3 parameter file
#' @param em_dir Directory to locate SS3 files
#' @param em_ctl_file_in  Input SS3 control file
#' @param em_ctl_file_out Output SS3 control file
#' @param selex_om Type of selectivity. Can take the entry \code{age} or \code{length} 
#' @param selex_em Type of selectivity. Can take the entry \code{age} or \code{length} 
#' @param fleet Fleet for which the selectivity must be changed 
#' @param pars_om_selex Selectivity parameters for OM
#' @param pars_em_selex Selectivity parameters for EM
#' @param sex_flag sex_flag
#' @param om_sex_offset Offset selectivity parameters for males in OM
#' @param em_sex_offset Offset selectivity parameters for males in EM
#' @param plot_selex_om plot selectivity of OM
#' @param plot_selex_em plot selectivity of EM
#'
#' @details \code{change_p} modifies the selectivity parameters in the \code{.ctl} file.
#' Selectivity has to be the same type (i.e. either age or length) in both OM and EM. 
#' The function requires both an OM and EM folder but the same folder path and files 
#' names can be used if the function is used outside of \code{\link{run_ss3sim}} 
#' and is used to change the \code{.ctl} file of a single folder. If \code{pars_om_selex} 
#' is NULL, then thefunction will only change the EM selectivity parameters. If the 
#' \code{em_selex_shape} is dome shaped the selectivity parameters 5 and 6 will 
#' be fixed at the value given in \code{pars_em_selex}, the user has to make sure 
#' he inputs the right parameters that would represent an asymptotic 
#' selectivity curve.  
#' 
#' @note LOTS OF ROOM FOR IMPROVEMENT I THINK...
#' @template casefile-footnote
#' @family change functions
#' @return
#' Altered versions of SS3 \code{.dat}, \code{.ctl} and \code{.par} files are written
#' to the disk.
#'
#' @author Gwladys Lambert
#' @importFrom r4ss SS_parlines SSplotSelex
#' @importFrom utils tail
#' @export
#' 
#' @examples
#' \dontrun{
#' # Find the example data folders:
#' 
#' temp_path <- tempdir() 
#' dir.create(temp_path, showWarnings = FALSE)
#' wd        <- getwd()
#' setwd(temp_path)
#' 
#' library(ss3models)
#' d      <- system.file("models", package = "ss3models")
#' cod    <- paste0(d, "/cod")
#' file.copy(cod, ".", recursive = TRUE)
#' setwd("cod")
#' 
#' wd <- getwd()
#' 
#' case_folder <- system.file("extdata", "new-cases", package = "ss3sim")
#' 
#' # With a custom parameters case for selectivity, which we'll call
#' # the P case. Here, we'll need to define which file the case P should
#' # read from ("P*-cod.txt"):
#' 
#' # First scenario - 
#' arg_list <- get_caseargs(case_folder, scenario = "D0-F0-P3-cod",
#'                          case_files = list(P = "P", D = c("index", "lcomp", "agecomp"), F =
#'                                              "F"))
#' 
#' 
#' ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
#' ### THIS IS THE FUNCTION ALONE - CAN BE RUN FOR ANY SCENARIO       ###
#' ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
#' 
#' # Here we plot the selectivity curves for each case
#' 
#' library(ss3sim)
#' 
#' setwd(wd)
#' p_params <- arg_list$P
#' 
#' with(p_params,
#'      change_p(om_dir               = "om",
#'               om_ctl_file_in       = "ss3.ctl", ####
#'               om_ctl_file_out      = "ss3_temp.ctl",
#'               par_file_in          = "ss3.par",
#'               par_file_out         = "ss3_temp.par",
#'               em_dir               = "em",
#'               em_ctl_file_in       = "ss3.ctl",
#'               em_ctl_file_out      = "ss3_temp.ctl",
#'               selex_om               = selex_om,
#'               selex_em               = selex_em,
#'               sex_flag            = sex_flag,
#'               fleet               = fleet, 
#'               pars_om_selex       = pars_om_selex,
#'               pars_em_selex       = pars_em_selex,
#'               om_sex_offset      = om_sex_offset,
#'               em_sex_offset      = em_sex_offset,
#'               plot_selex_om       = NULL,
#'               plot_selex_em       = TRUE
#'      ))
#' 
#' ## NEED TO CLEAN UP AFTER ##
#' ## unlink()
#'}
#'
#' 


change_p <- function(om_dir="om",
                     om_ctl_file_in       = "ss3.ctl",
                     om_ctl_file_out      = "ss3.ctl",
                     par_file_in          = "ss3.par",
                     par_file_out         = "ss3.par",
                     em_dir               = "em",
                     em_ctl_file_in       = "ss3.ctl",
                     em_ctl_file_out      = "ss3.ctl",
                     fleet = c(1,2),
                     selex_om= c("age","length"),
                     selex_em = c("age","length"), #,"length"em_selex=, ###### NOW SELEX CAN BE DIFFERENT BETWEEN OM AND EM!!!!!!!!
                     pars_om_selex=list(c(51,-3,5.1,15,-999,-999),c(51,-3,5.1,15,-999,-999)),
                     pars_em_selex=list(c(45,-1,3, 11,-999,-999),c(45,-1,3, 11,-999,-999)),
                     sex_flag =c(1,1),
                     om_sex_offset=list(c(20,0,-0.1,-0.1),c(20,0,-0.1,-0.1)),
                     em_sex_offset=list(c(20,0,-0.1,-0.1),c(20,0,-0.1,-0.1)),
                     plot_selex_om = NULL,
                     plot_selex_em = NULL
){
  
  # COnditions in case the model is run outside of run_ss3sim
  # if (exists("i")) { iteration <- i } else { iteration <- NULL}
  if (exists("sc")) { sc <- sc } else { sc <- NULL; i = NULL}
  
  #require(Hmisc)
  
  # Read in all files needed
  
  # read in OM files needed
  wd_temp <- getwd()
  setwd(pastef(wd_temp,om_dir))
  # pars file
  ss.par    <- readLines(con = par_file_in)
  # ctl file
  om.ctl    <- readLines(con = om_ctl_file_in)
  #Everywhere there is a \t replace by space
  om.ctl <- gsub("\t", " ", om.ctl)
  
  omctltable <- SS_parlines(ctlfile = om_ctl_file_in)
  
  # read in EM files needed
  setwd(pastef(wd_temp,em_dir))
  em.ctl    <- readLines(con = em_ctl_file_in)
  #Everywhere there is a \t replace by space
  em.ctl <- gsub("\t", " ", em.ctl)
  
  emctltable <- SS_parlines(ctlfile = em_ctl_file_in)
  
  
  
  #######################################################################################
  ############### START BY EXTRACTING THE RELEVANT OLD OM CTL PARAMETERS ################
  #######################################################################################
  
  
  selex_type_om_orig <- NULL
  om_param_orig <- NULL
  om_param_orig_linenums <- NULL
  selex_type_om_orig_linenums <- NULL
  sex_fleet_om <- NULL
  mirror_fleet_om <- NULL
  
  if (!is.null(pars_om_selex)){
    
    
    ######## MAKE DATAFRAME OF ORIG OM SELEX TYPES
    newomctl         <- om.ctl
    start            <- which(newomctl %in% newomctl[grep("_Pattern ", newomctl)])+1
    nbflt_temp       <- newomctl[c(start[1]:start[2]-1)]
    total_nflt       <- length(which(suppressWarnings(sapply(sapply(strsplit(nbflt_temp, split = "[[:blank:]]+"), as.numeric),function(x) any(!is.na(x))))==T))
    end              <- which(newomctl %in% newomctl[grep("_Pattern ", newomctl)])+total_nflt
    linenums         <- c(start[1]:end[1],start[2]:end[2])
    subsetomctl      <- newomctl[linenums]
    splitline        <- unlist(strsplit(subsetomctl, "#")) #[seq(1,length(subsetomctl)*2,2)]
    vec_temp         <- unlist(strsplit(splitline, split = "[[:blank:]]+"))
    vec_temp[vec_temp==""] <- NA
    vec_temp         <- vec_temp[!is.na(vec_temp)]
    df               <- as.data.frame(matrix(vec_temp, ncol=6, byrow=T), stringsAsFactors = F)
    df               <- cbind.data.frame(sapply(df[,1:5], as.numeric),df[,6])
    df$Fleet         <- 1:c(nrow(df)/2)
    
    selex_type_om_orig <- df
    selex_type_om_orig_linenums <- linenums
    
    ######## ASSIGN THE NUMBER OF PARAMETERS EXPECTED
    selex_type_om_orig$nbpars <- NA
    for (idx in 1:nrow(selex_type_om_orig)) {
      if(selex_type_om_orig[idx,1] == 0) selex_type_om_orig$nbpars[idx] <- 0
      if(selex_type_om_orig[idx,1] == 11) selex_type_om_orig$nbpars[idx] <- 2
      if(selex_type_om_orig[idx,1] == 15) selex_type_om_orig$nbpars[idx] <- 0
      if(selex_type_om_orig[idx,1] == 20) selex_type_om_orig$nbpars[idx] <- 6
      if(selex_type_om_orig[idx,1] == 24) selex_type_om_orig$nbpars[idx] <- 6
      if(selex_type_om_orig[idx,3] != 0) selex_type_om_orig[idx,3] <- 3
    }
    
    ######## DEFINE WHICH FLEET HAVE GOT SEX-SPECIFIC SELECTIVITY AND ADD TO PARAMETERS ESTIMATES
    temp_flt    <- selex_type_om_orig[selex_type_om_orig[,3]!=0,][,5]
    if(length(temp_flt) !=0) sex_fleet_om   <- cbind.data.frame(Fleet = temp_flt, Sex = 3) #1
    to_add <-  ifelse(sex_flag[temp_flt] %in% c(1,2),4,5)
    if(length(temp_flt) !=0) selex_type_om_orig[selex_type_om_orig[,3]!=0,"nbpars"] <-  selex_type_om_orig[selex_type_om_orig[,3]!=0,"nbpars"] + to_add
    
    ######## DEFINE WHICH FLEET HAVE A MIRROR SELECTIVITY
    mirror_fleet_om <- NULL
    temp_flt    <- selex_type_om_orig[selex_type_om_orig[,4]!=0,][,5]
    if(length(temp_flt) !=0) temp_mirror <- selex_type_om_orig[selex_type_om_orig[,4]!=0,][,4]
    if(length(temp_flt) !=0) mirror_fleet_om <- cbind.data.frame(Fleet = temp_flt, Mirror = temp_mirror)
    
    ######## MAKE DATAFRAME OF ORIG OM PARAMETER VALUES
    
    # To do so we will first replace the .ctl parameters values by the .par values 
    # This is because the .par file gives the selex values for OM (not the .ctl file). 
    # We can then change values and options in OM .ctl then put back in .par
    newpars <- ss.par
    start <- grep("selparm[1]", newpars, fixed = T)
    newpars_part1 <- newpars[1:(start-1)]
    newpars_vals <- newpars[start:length(newpars)]
    newpars_vals <- as.character(newpars_vals[seq(2, length(newpars_vals),2)])
    # extract table of values in .ctl and replace with newpars_vals
    linenums <- omctltable$Linenum[c(c(tail(grep("LnQ", omctltable$Label, fixed = T),1)+1):dim(omctltable)[1])]
    #rm line if enviro is given
    is_env_given <- which(newomctl %in% newomctl[grep("env link ", newomctl)])
    #check taht first value is not a #
    if (length(is_env_given)!=0) {
      if (suppressWarnings(!is.na(as.numeric(unlist(strsplit(newomctl[is_env_given], " ")))[1]))) {
        linenums <- linenums[-length(linenums)]
      }    
    }
    # New om ctl will include eveything before and after those lines but those lines will be fully replaced
    save_omctl_part1 <- newomctl[c(1:c(linenums[1]-1))]
    save_omctl_part2 <- newomctl[c(c(tail(linenums,1)+1):length(newomctl))]
    om_params_orig <- newomctl[linenums[1]:tail(linenums,1)]
    # add a condition that if the line is too short, then it is a comment, take oout
    cond1        <- strsplit(om_params_orig, split = "[[:blank:]]+")
    cond2        <- sapply(cond1, function(x) length(x))
    om_params_orig        <- om_params_orig[which(cond2>=12)]
    splitline       <- unlist(strsplit(om_params_orig, "#")) #[seq(1,length(subsetomctl)*2,2)]
    #back to script
    vec_temp        <- unlist(strsplit(splitline, split = "[[:blank:]]+"))
    vec_temp[vec_temp==""] <- NA
    vec_temp[vec_temp==" "] <- NA
    vec_temp        <- vec_temp[!is.na(vec_temp)]
    df               <- as.data.frame(matrix(vec_temp, ncol=15, byrow=T), stringsAsFactors = F)
    df               <- cbind.data.frame(sapply(df[,1:14], as.numeric),df[,15])
    df$Fleet        <- rep(selex_type_om_orig$Fleet, times= selex_type_om_orig$nbpars)
    df[,3] <- newpars_vals # that is where we input the .par values
    df[,4] <- newpars_vals
    
    om_param_orig <- df
    om_param_orig_linenums <- linenums
    
  }
  
  
  #######################################################################################
  ############### START BY EXTRACTING THE RELEVANT OLD EM CTL PARAMETERS ################
  #######################################################################################
  
  selex_type_em_orig <- list()
  em_param_orig <- list()
  em_param_orig_linenums <- NULL
  selex_type_em_orig_linenums <- NULL
  
  sex_fleet_em <- NULL
  mirror_fleet_em <- NULL
  
  # if (run_change_p_full) {
  
  if (!is.null(pars_em_selex)){
    
    ######## MAKE DATAFRAME OF ORIG EM SELEX TYPES
    newemctl        <- em.ctl
    start           <- which(newemctl %in% newemctl[grep("_Pattern ", newemctl)])+1
    nbflt_temp       <- newemctl[c(start[1]:start[2]-1)]
    total_nflt       <- length(which(suppressWarnings(sapply(sapply(strsplit(nbflt_temp, split = "[[:blank:]]+"), as.numeric),function(x) any(!is.na(x))))==T))
    end             <- which(newemctl %in% newemctl[grep("_Pattern ", newemctl)])+total_nflt
    linenums        <- c(start[1]:end[1],start[2]:end[2])
    subsetemctl     <- newemctl[linenums]
    splitline       <- unlist(strsplit(subsetemctl, "#")) #[seq(1,length(subsetomctl)*2,2)]
    vec_temp        <- unlist(strsplit(splitline, split = "[[:blank:]]+"))
    vec_temp[vec_temp==""] <- NA
    vec_temp        <- vec_temp[!is.na(vec_temp)]
    df               <- as.data.frame(matrix(vec_temp, ncol=6, byrow=T), stringsAsFactors = F)
    df               <- cbind.data.frame(sapply(df[,1:5], as.numeric),df[,6])
    df$Fleet        <- 1:c(nrow(df)/2)
    
    selex_type_em_orig <- df
    selex_type_em_orig_linenums <- linenums
    
    
    ######## ASSIGN THE NUMBER OF PARAMETERS EXPECTED
    selex_type_em_orig$nbpars <- NA
    for (idx in 1:nrow(selex_type_em_orig)) {
      if(selex_type_em_orig[idx,1] == 0) selex_type_em_orig$nbpars[idx] <- 0
      if(selex_type_em_orig[idx,1] == 11) selex_type_em_orig$nbpars[idx] <- 2
      if(selex_type_em_orig[idx,1] == 15) selex_type_em_orig$nbpars[idx] <- 0
      if(selex_type_em_orig[idx,1] == 20) selex_type_em_orig$nbpars[idx] <- 6
      if(selex_type_em_orig[idx,1] == 24) selex_type_em_orig$nbpars[idx] <- 6
      if(selex_type_em_orig[idx,3] != 0) selex_type_em_orig[idx,3] <- 3
    }
    
    ######## DEFINE WHICH FLEET HAVE GOT SEX-SPECIFIC SELECTIVITY AND ADD TO PARAMETERS ESTIMATES
    temp_flt    <- selex_type_em_orig[selex_type_em_orig[,3]!=0,][,5]
    if(length(temp_flt) !=0) sex_fleet_em   <- cbind.data.frame(Fleet = temp_flt, Sex = 3) #1sex_flag[temp_flt]
    to_add <-  ifelse(sex_flag[temp_flt] %in% c(1,2),4,5)
    if(length(temp_flt) !=0)  selex_type_em_orig[selex_type_em_orig[,3]!=0,"nbpars"] <-  selex_type_em_orig[selex_type_em_orig[,3]!=0,"nbpars"] + to_add
    
    ######## DEFINE WHICH FLEET HAVE A MIRROR SELECTIVITY
    mirror_fleet_em <- NULL
    temp_flt    <- selex_type_em_orig[selex_type_em_orig[,4]!=0,][,5]
    temp_mirror <- selex_type_em_orig[selex_type_em_orig[,4]!=0,][,4]
    if(length(temp_flt) !=0) mirror_fleet_em <- cbind.data.frame(Fleet = temp_flt, Mirror = temp_mirror)
    
    ######## MAKE DATAFRAME OF ORIG EM PARAMETER VALUES
    linenums <- emctltable$Linenum[c(c(tail(grep("LnQ", emctltable$Label, fixed = T),1)+1):dim(emctltable)[1])]
    #rm line if enviro is given
    is_env_given <- which(newemctl %in% newemctl[grep("env link ", newemctl)])
    #check taht first value is not a #
    if (length(is_env_given)!=0) {
      if (suppressWarnings(!is.na(as.numeric(unlist(strsplit(newemctl[is_env_given], " ")))[1]))) {
        linenums <- linenums[-length(linenums)]
      }
    }
    # New om ctl will include eveything before and after those lines but those lines will be fully replaced
    save_emctl_part1 <- newemctl[c(1:c(linenums[1]-1))]
    save_emctl_part2 <- newemctl[c(c(tail(linenums,1)+1):length(newemctl))]
    em_params_orig   <- newemctl[linenums[1]:tail(linenums,1)]
    # add a condition that if the line is too short, then it is a comment, take oout
    em_params_orig <- gsub("#A", "# A", em_params_orig)
    em_params_orig <- gsub("#S", "# S", em_params_orig)
    cond1        <- strsplit(em_params_orig, split = "[[:blank:]]+")
    cond2        <- sapply(cond1, function(x) length(x))
    em_params_orig        <- em_params_orig[which(cond2>=12)]
    # also if it starts with a comment remove
    cond3 <- do.call(rbind, cond1)
    to_rm <- which(cond3[,1]=="#")
    if (length(to_rm)!=0)  em_params_orig <- em_params_orig[-which(cond3[,1]=="#")] 
    # back to script
    splitline        <- unlist(strsplit(em_params_orig, "#")) #[seq(1,length(subsetomctl)*2,2)]
    vec_temp         <- unlist(strsplit(splitline, split = "[[:blank:]]+"))
    vec_temp[vec_temp==""] <- NA
    vec_temp        <- vec_temp[!is.na(vec_temp)]
    df               <- as.data.frame(matrix(vec_temp, ncol=15, byrow=T), stringsAsFactors = F)
    df               <- cbind.data.frame(sapply(df[,1:14], as.numeric),df[,15])
    df$Fleet        <- rep(selex_type_em_orig$Fleet, times= selex_type_em_orig$nbpars)
    
    em_param_orig   <- df
    em_param_orig_linenums <- linenums
    
  }
  
  # Tidying up
  
  total_nb_fleet <- total_nflt
  selex_type_om_orig[,6] <- as.character(selex_type_om_orig[,6])
  selex_type_em_orig[,6]<- as.character(selex_type_em_orig[,6])
  
  fleet.names <- unique(selex_type_em_orig[,6])
  om_param_orig[,15] <- as.character(om_param_orig[,15])
  em_param_orig[,15]<- as.character(em_param_orig[,15])
  
  #sex_fleet <- unique(rbind(sex_fleet_om, sex_fleet_em))
  sex_fleet <- list(sex_fleet_om, sex_fleet_em)
  mirror_fleet <- unique(rbind(mirror_fleet_om, mirror_fleet_em))
  
  to_rm <- which(om_param_orig$Fleet %in% mirror_fleet$Fleet)
  if (length(to_rm)!=0)  om_param_orig <- om_param_orig[-to_rm,]
  
  to_rm <- which(em_param_orig$Fleet %in% mirror_fleet$Fleet)
  if (length(to_rm)!=0)  em_param_orig <- em_param_orig[-to_rm,]

  
  
  ###########################################################
  ############## MAKE THE TWO NEW DATAFRAMES ################
  ###########################################################
  
  
  ## List the old dataframes
  selex_type_orig_both <- list(selex_type_om_orig,selex_type_em_orig)
  param_orig_both  <- list(om_param_orig, em_param_orig)
  
  selex_type_new_both <- list()
  param_new_both <- list()
  
  sex_offset <- list(om_sex_offset,em_sex_offset)
  
  for ( i in 1:2) {
    
    selex_type_orig <- selex_type_orig_both[[i]]
    param_orig <- param_orig_both[[i]]
    
    if (i == 1 ) {
      pars_selex <- pars_om_selex; selex = selex_om }
    if (i == 2 ) {
      pars_selex <- pars_em_selex;  selex = selex_em }
    
    if (!is.null(pars_selex)){
      
      ######### FIRST SORT OUT SELECTIVITY TYPE
      
      selex_type_new <- selex_type_orig
      
      for (flx in 1:length(fleet)) {
        
        if(selex[flx]=="length") {
          
          # replace first half by 24, second half by 11
          selex_type_new[,1][selex_type_new$Fleet==fleet[flx]] <- c(24,11) #rep(c(24,11), each=total_nb_fleet)
          # ASSIGN THE NUMBER OF PARAMETERS EXPECTED
          selex_type_new$nbpars[selex_type_new$Fleet==fleet[flx]] <- c(6,2)
          
          # replace mirror fleets by 15
          if (!is.null(mirror_fleet)) {
            if (fleet[flx] %in% mirror_fleet$Fleet) {
              selex_type_new[,4][selex_type_new$Fleet==fleet[flx]]  <- c(1)
              selex_type_new[,1][selex_type_new$Fleet==fleet[flx]]  <- c(15)
              selex_type_new$nbpars[selex_type_new$Fleet==fleet[flx]]  <- 0
            }
          }
        }
        
        if(selex[flx]=="age") {
          # replace first half by 24, second half by 11
          #  selex_type_new[,1] <- rep(c(0,20), each=total_nb_fleet)
          selex_type_new[,1][selex_type_new$Fleet==fleet[flx]] <- c(0,20) #rep(c(24,11), each=total_nb_fleet)
          # ASSIGN THE NUMBER OF PARAMETERS EXPECTED
          selex_type_new$nbpars[selex_type_new$Fleet==fleet[flx]] <- c(0,6)
          # replace mirror fleets by 15
          if (!is.null(mirror_fleet)) {
            if (fleet[flx] %in% mirror_fleet$Fleet) {
              selex_type_new[,4][selex_type_new$Fleet==fleet[flx]]  <- c(1)
              selex_type_new[,1][selex_type_new$Fleet==fleet[flx]]  <- c(15)
              selex_type_new$nbpars[selex_type_new$Fleet==fleet[flx]]  <- 0
            }
          }
          selex_type_new[selex_type_new$Fleet==fleet[flx],c(1:4)][1,] <- 0 # all lengths based selectivity are 0
        }
      }
      
      ######### THEN SORT OUT SELECTIVITY PARAMETERS IN .CTL FILE
      
      param_new_temp_final <- NULL
      param_new <- param_orig

      for (flx in 1:length(fleet)) {
        
        if(selex[flx]=="length") {
    
          idx = fleet[flx]
          
          param_new[param_new$Fleet==idx,][1:6,][,c(3,4)] <- as.numeric(pars_selex[[flx]]) ########### CHECK NUMBER HERE ######################
          
          # Adapt bounds
          vec_lo <- sapply(param_new[param_new$Fleet==idx,][1:6,][,c(1,3)],as.numeric)
          vec_hi <- sapply(param_new[param_new$Fleet==idx,][1:6,][,c(2,3)],as.numeric)
          vec_lo[,1] <- ifelse(vec_lo[,1]>vec_lo[,2], 0.1*vec_lo[,2],vec_lo[,1])
          vec_lo[,1] <- ifelse(vec_lo[,1]>vec_lo[,2], 1.9*vec_lo[,2],vec_lo[,1]) # for negative values
          vec_hi[,1] <- ifelse(vec_hi[,1]<vec_hi[,2], 2*vec_hi[,2],vec_hi[,1])
          vec_hi[,1] <- ifelse(vec_hi[,1]<vec_hi[,2], 0.5*vec_hi[,2],vec_hi[,1]) # for negative values
          
          param_new[param_new$Fleet==idx,][1:6,][,c(1,2)] <- cbind(vec_lo[,1],vec_hi[,1])
          
          param_new[param_new$Fleet==idx,][1:6,][,15] <-
            paste(" SizeSel_",fleet[idx],"P_",1:6,"_",fleet.names[idx], sep="")
          
          if (idx %in% c(mirror_fleet$Mirror)) {
            mirror_flt <- mirror_fleet[mirror_fleet$Mirror==idx,]
            new_rows <- param_new[c(1,2),]
            new_rows[1,] <- c(0, 1, 0.1, 0.1, -1, 99.00, -3, 0, 0, 0, 0, 0.5, 0, 0, paste("AgeSel_", mirror_flt$Fleet, "P_1_", fleet.names[mirror_flt$Fleet], sep=""), mirror_flt$Fleet)
            new_rows[2,] <- c(0, 101, 100, 100, -1, 99.00, -3, 0, 0, 0, 0, 0.5, 0, 0, paste("AgeSel_", mirror_flt$Fleet, "P_2_", fleet.names[mirror_flt$Fleet], sep=""), mirror_flt$Fleet)
            param_new <- rbind(param_new, new_rows)
          }

          # If the fleet is in the sex data, add 4 parameters
          
          if (!is.null(sex_fleet[[i]]) & idx %in% sex_fleet[[i]]$Fleet) {
            
            selex_type_new$nbpars[selex_type_new$Fleet==idx & selex_type_new$nbpars!=0] <- 
              selex_type_new$nbpars[selex_type_new$Fleet==idx & selex_type_new$nbpars!=0] + 5
            
            selex_type_new[selex_type_new$Fleet ==idx & selex_type_new$nbpars!=0,][,3] <- 3 #sex_flag[idx]
            
            new_vals <- sex_offset[[i]][[flx]] ########### CHECK NUMBER HERE ######################
            
            param_new_temp <- rbind(param_new[param_new$Fleet==idx,][1:10,], param_new[param_new$Fleet==idx,][10,])
            param_new_temp[7:11,][,c(3)] <- new_vals
            param_new_temp[7:11,][,c(4)] <- new_vals
            
            # Adapt bounds
            vec_lo <- sapply(param_new_temp[7:11,][,c(1,3)], as.numeric)
            vec_hi <- sapply(param_new_temp[7:11,][,c(2,3)], as.numeric)
            vec_lo[,1] <- ifelse(vec_lo[,1]>vec_lo[,2], 0.1*vec_lo[,2],vec_lo[,1])
            vec_lo[,1] <- ifelse(vec_lo[,1]>vec_lo[,2], 1.9*vec_lo[,2],vec_lo[,1]) # for negative values
            vec_hi[,1] <- ifelse(vec_hi[,1]<vec_hi[,2], 2*vec_hi[,2],vec_hi[,1])
            vec_hi[,1] <- ifelse(vec_hi[,1]<vec_hi[,2], 0.5*vec_hi[,2],vec_hi[,1]) # for negative values
            param_new_temp[7:11,][,c(1,2)] <- cbind(vec_lo[,1],vec_hi[,1])
            
            param_new_temp[8,][,c(1:2)] <- c(-15,15)
            param_new_temp[9,][,c(1:2)] <- c(-15,15)
            param_new_temp[10,][,c(1:2)]<- c(-15,15)
            param_new_temp[11,][,c(1:2)]<- c(-15,15)
            
            param_new_temp[7,15] <- paste(" SizeSel_",fleet[idx],"Male_Peak_",fleet.names[idx], sep="")
            param_new_temp[8,15] <- paste(" SizeSel_",fleet[idx],"Male_Ascend_",fleet.names[idx], sep="")
            param_new_temp[9,15] <- paste(" SizeSel_",fleet[idx],"Male_Descend_",fleet.names[idx], sep="")
            param_new_temp[10,15] <- paste(" SizeSel_",fleet[idx],"Male_Final_",fleet.names[idx], sep="")
            param_new_temp[11,15] <- paste(" SizeSel_",fleet[idx],"Male_Scale_",fleet.names[idx], sep="")
            
            param_new_temp_final <- rbind(param_new_temp_final, param_new_temp)
            
            p1 <- c(0,1,0.1,0.1,-1,99,-3,0,0,0,0,0,0,0)
            cmt_p1 <- paste(" AgeSel_",fleet[idx],"P_1_",fleet.names[idx], sep="")
            p2 <- c(0,200,100,100,-1,99,-3,0,0,0,0,0,0,0)
            cmt_p2 <- paste(" AgeSel_",fleet[idx],"P_2_",fleet.names[idx], sep="")
            
            p1 <- c(p1,cmt_p1)
            p2 <- c(p2,cmt_p2)
            
            param_new_temp_final <- rbind(param_new_temp_final,p1)
            param_new_temp_final <- rbind(param_new_temp_final,p2)
            
          } else { param_new_temp_final <- param_new }
          
        }
        
        
        if(selex[flx]=="age") {
          
          idx = fleet[flx]
          
          if (length(grep("SizeSel",param_new[param_new$Fleet==idx,15])) != 0) 
            # param_new[param_new$Fleet==idx,] <- which(param_new$ [-grep("AgeSel",param_new[param_new$Fleet==idx,15]),] #[param_new$Fleet==idx,]
          { param_new <- param_new[-grep(paste("AgeSel_",idx, sep=""), param_new[,15]),] }        
          
          param_new[param_new$Fleet==idx,][1:6,][,c(3,4)] <- pars_selex[[flx]] 
          
          # Adapt bounds
          vec_lo <- sapply(param_new[param_new$Fleet==idx,][1:6,][,c(1,3)], as.numeric)
          vec_hi <- sapply(param_new[param_new$Fleet==idx,][1:6,][,c(2,3)], as.numeric)
          vec_lo[,1] <- ifelse(vec_lo[,1]>vec_lo[,2], 0.1*vec_lo[,2],vec_lo[,1])
          vec_lo[,1] <- ifelse(vec_lo[,1]>vec_lo[,2], 1.9*vec_lo[,2],vec_lo[,1]) # for negative values
          vec_hi[,1] <- ifelse(vec_hi[,1]<vec_hi[,2], 2*vec_hi[,2],vec_hi[,1])
          vec_hi[,1] <- ifelse(vec_hi[,1]<vec_hi[,2], 0.5*vec_hi[,2],vec_hi[,1]) # for negative values
          
          param_new[param_new$Fleet==idx,][1:6,][,c(1,2)] <- cbind(vec_lo[,1],vec_hi[,1])
          
          param_new[param_new$Fleet==idx,][1:6,][,15] <-
            paste(" AgeSel_",fleet[idx],"P_",1:6,"_",fleet.names[idx], sep="")
          
          # If the fleet is in the sex data, add 4 parameters
          
          if (!is.null(sex_fleet[[i]]) & idx %in% sex_fleet[[i]]$Fleet) {
            
            selex_type_new$nbpars[selex_type_new$Fleet==idx & selex_type_new$nbpars!=0] <- 
              selex_type_new$nbpars[selex_type_new$Fleet==idx & selex_type_new$nbpars!=0] + 5
            
            selex_type_new[selex_type_new$Fleet ==idx & selex_type_new$nbpars!=0,][,3] <- 3 #sex_flag[idx]
            
            new_vals <- sex_offset[[i]][[idx]] ########### CHECK NUMBER HERE ######################
            
            ### NEED TO MAKE THIS TO FOLLOW THE FORMAT ABOVE - NEED TO INSERT IT IN THE NEW DATASET WHERE IT BELONGS 
            ### (MIGHT HAVE TO BE DONE AFTER) ### ALTHO THESE DATA SHOULD ALREADY BE IN SO.... IT MIGHT BE EASIER THAN IT SEEMS
            
            param_new_temp <- rbind(param_new[param_new$Fleet==idx,][1:10,], param_new[param_new$Fleet==idx,][10,])
            param_new_temp[7:11,][,c(3)] <- new_vals
            param_new_temp[7:11,][,c(4)] <- new_vals
  
            # Adapt bounds
            vec_lo <- sapply(param_new_temp[7:11,][,c(1,3)], as.numeric)
            vec_hi <- sapply(param_new_temp[7:11,][,c(2,3)], as.numeric)
            vec_lo[,1] <- ifelse(vec_lo[,1]>vec_lo[,2], 0.1*vec_lo[,2],vec_lo[,1])
            vec_lo[,1] <- ifelse(vec_lo[,1]>vec_lo[,2], 1.9*vec_lo[,2],vec_lo[,1]) # for negative values
            vec_hi[,1] <- ifelse(vec_hi[,1]<vec_hi[,2], 2*vec_hi[,2],vec_hi[,1])
            vec_hi[,1] <- ifelse(vec_hi[,1]<vec_hi[,2], 0.5*vec_hi[,2],vec_hi[,1]) # for negative values
            param_new_temp[7:11,][,c(1,2)] <- cbind(vec_lo[,1],vec_hi[,1])
            
            if (param_new_temp[7,][,c(1)]  ==0 ) param_new_temp[7,][,c(1)]<- -1
            if (param_new_temp[7,][,c(2)]  ==0 ) param_new_temp[7,][,c(2)]<- 5
            
            param_new_temp[8,][,c(1:2)] <- c(-15,15)
            param_new_temp[9,][,c(1:2)] <- c(-15,15)
            param_new_temp[10,][,c(1:2)]<- c(-15,15)
            param_new_temp[11,][,c(1:2)]<- c(-15,15)
            
            param_new_temp[7,15] <- paste(" AgeSel_",fleet[idx],"Male_Peak_",fleet.names[idx], sep="")
            param_new_temp[8,15] <- paste(" AgeSel_",fleet[idx],"Male_Ascend_",fleet.names[idx], sep="")
            param_new_temp[9,15] <- paste(" AgeSel_",fleet[idx],"Male_Descend_",fleet.names[idx], sep="")
            param_new_temp[10,15] <- paste(" AgeSel_",fleet[idx],"Male_Final_",fleet.names[idx], sep="")
            param_new_temp[11,15] <- paste(" AgeSel_",fleet[idx],"Male_Scale_",fleet.names[idx], sep="")
            
            param_new_temp_final <- rbind(param_new_temp_final, param_new_temp)
            
          }else{
            param_new_temp_final <- param_new }
        }
        
      }
      
    } 
    param_new_temp_final <- na.omit(param_new_temp_final)
    selex_type_new_both[[i]] <- selex_type_new
    param_new_both[[i]] <- param_new_temp_final
  }    
  
  
  ##############################################################
  ############## REPLACE THE OLD CTL DATAFRAMES ################
  ##############################################################
  
  for (i in 1:2) {
    
    if (length(param_new_both[[i]]) > 1) {
      
      param_new_both[[i]]$to_order= 1
      param_new_both[[i]][grepl("AgeSel",param_new_both[[i]][,15]),"to_order"] <- 2
      
      # Change the selex params
      param_new_both_i <- param_new_both[[i]]#[,1:15]
      param_new_both_i <- param_new_both_i[order(param_new_both_i$Fleet),]
      param_new_both_i <- param_new_both_i[order(param_new_both_i$to_order),]
      subsetnewctl <- param_new_both_i[,1:14]
      
      if (i ==1) save_pars_for_parfile <- subsetnewctl[,3]
      
      subsetnewctl$comment <- sapply(split(param_new_both_i[,15], seq(nrow(param_new_both_i))), function(x) paste("#", paste(x, collapse=" "), sep=""))
      subsetnewctl <- as.vector((sapply(split(subsetnewctl, seq(nrow(subsetnewctl))), function(x) paste("", paste(x, collapse = " ")))))
      
      if (i ==1) {subsetnewctl_part1 <- save_omctl_part1; subsetnewctl_part2 <- save_omctl_part2  } #param_orig_linenums <- om_param_orig_linenums; tot_length <- length(newomctl); newctl = newomctl
      if (i ==2) {subsetnewctl_part1 <- save_emctl_part1; subsetnewctl_part2 <- save_emctl_part2  } #param_orig_linenums <- em_param_orig_linenums; tot_length <- length(newemctl); newctl = newemctl  
      
      newctl <- c(subsetnewctl_part1, subsetnewctl, subsetnewctl_part2)
      
      # Change the selex types
      subsetnewctl <- selex_type_new_both[[i]][,1:4]
      subsetnewctl$comment <- sapply(split(selex_type_new_both[[i]][,5:6], seq(nrow(selex_type_new_both[[i]]))), function(x) paste("#", paste(x, collapse=" "), sep=""))
      subsetnewctl <- as.vector((sapply(split(subsetnewctl, seq(nrow(subsetnewctl))), function(x) paste("", paste(x, collapse = " ")))))
      
      cmt <- "#_Pattern "
      subsetnewctl <- c(subsetnewctl[1:total_nb_fleet], cmt, subsetnewctl[(total_nb_fleet+1):(total_nb_fleet*2)])
      
      if (i ==1) { selex_type_orig_linenums <- selex_type_om_orig_linenums} #; tot_length <- length(newomctl) 
      if (i ==2) { selex_type_orig_linenums <- selex_type_em_orig_linenums} #; tot_length <- length(newemctl) 
      
      subsetnewctl_part1 <- newctl[1:(min(selex_type_orig_linenums)-1)]
      subsetnewctl_part2 <- newctl[(max(selex_type_orig_linenums)+1):length(newctl)]
      newctl <- c(subsetnewctl_part1, subsetnewctl, subsetnewctl_part2)
      
      if (i ==1) { 
        setwd(pastef(wd_temp,om_dir))
        writeLines(newctl, con= om_ctl_file_out)
      }
      
      if (i ==2) { 
        setwd(pastef(wd_temp,em_dir))
        writeLines(newctl, con= em_ctl_file_out)
      }
    }
  }
  
  ######################################################################################################
  ############################## CHANGE OM .PAR FILE ###################################################
  ######################################################################################################
  
  if (!is.null(pars_om_selex)){
    
    vec <- c(rbind(NA, save_pars_for_parfile))
    vec[seq(1,(length(vec)-1),2)] <- paste("# selparm[",1:length(save_pars_for_parfile),"]:", sep="")
    
    newpars <- c(newpars_part1, vec)
    
    setwd(pastef(wd_temp,om_dir))
    writeLines(newpars, con=par_file_out)
    
  }
  
  
  #############################################################################################
  ########## PLOTTING OM AND EM SELEX IN STAND ALONE FUNCTION #################################
  #############################################################################################
  
  
  if (!is.null(plot_selex_om)) {
    
    # Copy om in a different folder
    setwd(pastef(wd_temp,om_dir))
    from <- getwd()
    setwd('..')
    to <- paste(getwd(),"om_plot_selex", sep="/")
    create.sim.folder(from, to)
    
    # Run SS
    setwd(to)
    setwd('..');setwd('..');setwd('..')
    
    example = NULL
    #split to
    vec_dirs <- strsplit(to, "/")
    dir1 <- vec_dirs[[1]][c(length(vec_dirs[[1]])-2)]
    dir2 <- vec_dirs[[1]][c(length(vec_dirs[[1]])-1)]
    
    if (is.null(sc)) { sc1 = dir1; iteration1= dir2; example = TRUE }
    
    run_ss3model(scenarios = sc1, iterations = iteration1, type = "om_plot_selex",
                 hess = FALSE) #, ...
    
    # Plot Selex
    setwd(to)
    dat <- SS_output(dir = getwd(), "ss3", covar=F, ncols=300)
    SSplotSelex(dat)
  }
  
  if (!is.null(plot_selex_em)) {
    
    # Copy em in a different folder 
    ### TRICK HERE - COPY OM FOLDER - BUT USE OM FILES EXCEPT FOR CTL (EM DOES NOT HAVE A PAR OR DAT FILE TO START WITH)
    setwd(pastef(wd_temp,om_dir))
    from <- getwd()
    setwd('..')
    to <- paste(getwd(),"em_plot_selex", sep="/")
    create.sim.folder(from, to)
    
    # Replace .ctl file with EM
    setwd(pastef(wd_temp,em_dir))
    file_ctl <- paste(getwd(), "em.ctl", sep="/")
    file_old_ctl <- paste(to, "om.ctl", sep="/")
    if (!is.null(example)) {
      file_ctl <- paste(getwd(), "ss3.ctl", sep="/")
      file_old_ctl <- paste(to, "ss3.ctl", sep="/")
    }
    file.copy(file_ctl, file_old_ctl, overwrite =T)

    example = NULL

    vec_dirs <- strsplit(to, "/")
    dir1 <- vec_dirs[[1]][c(length(vec_dirs[[1]])-2)]
    dir2 <- vec_dirs[[1]][c(length(vec_dirs[[1]])-1)]
    
    if (is.null(sc)) { sc1 = dir1; iteration1=dir2; example = TRUE }
    
    # Run SS
    setwd(to)
    setwd('..');setwd('..');setwd('..')
    run_ss3model(scenarios = sc1, iterations = iteration1, type = "em_plot_selex",
                 hess = FALSE) #, ...
    
    # Plot Selex
    setwd(to)
    dat <- SS_output(dir = getwd(), "ss3", covar=F, ncols=300)
    SSplotSelex(dat)
  }
  
}
