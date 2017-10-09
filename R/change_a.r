#' Function to modify ageing error parameters in \code{.dat} files.
#'
#' @description Takes ss3 \code{.dat} files and changes the ageing error.
#' The function can be called by itself or within \code{\link{run_ss3sim}} to alter 
#' an operating and estimation model \code{.ctl} files. If used with \code{\link{run_ss3sim}} 
#' the case file should be named \code{A}. A suggested (default) case letter is \code{A} for 
#' Ageing error.
#'
#' @param dat_list Input SS3 data file
#' @param outfile Output SS3 data file
#' @param df Dataframe giving fleets, years, gender etc and starts with corresponding 
#' ageing error matrix index
#' @param agerr_mat new ageing error matrix to be added to existing ones and index 
#' number
#' @param mis_spec whether to mis-specify ageing error in the EM file 
#' (turns back to assuming no ageing error in the EM)
#' @template write_file
#'
#' @details \code{change_a} modifies the ageing error in the \code{.dat} file both
#' in the OM and in the EM unless specified otherwise. There is no option yet to
#' specify different ageing errors in OM and EM, but can be easily implemented.
#' Using this case file in \code{\link{ss3sim_base}} means that the expected 
#' data out of the OM will include ageing error before the sampling occurs.
#' 
#' @template casefile-footnote
#' @family change functions
#' @return
#' Altered versions of SS3 \code{.dat} files are written to the disk.
#'
#' @author Gwladys Lambert
#' @export
#' @examples
#' \dontrun{
#' # Find the SS3sim simple model in the package:
#' d         <- system.file("extdata", package = "ss3sim")
#' simple    <- paste0(d, "/Simple")
#' file.copy(simple, ".", recursive = TRUE)
#' setwd("simple")
#' 
#' wd <- getwd()
#' 
#' ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
#' ### READ EXISTING CASES OR CREATE CASES ###
#' ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
#' 
#' ## Read existing files
#' 
#' # Find the example data folders:
#' case_folder <- system.file("extdata", "new-cases", package = "ss3sim")
#' 
#' arg_list <- get_caseargs(case_folder, scenario = "D0-F0-A2-cod",
#'                          case_files = list(A = "A", 
#'                          D = c("index", "lcomp", "agecomp"), 
#'                          F ="F"))
#' 
#' ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
#' ### THIS IS THE FUNCTION ALONE - CAN BE RUN FOR ANY SCENARIO ABOVE ###
#' ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
#' 
#' # Here we plot the selectivity curves for each case
#' 
#' setwd(wd)
#' 
#' datfile.orig <- SS_readdat("simple.dat", verbose = FALSE)
#' 
#' a_params <- arg_list$A
#' temp <- with(a_params,
#'      change_a(dat_list             =  datfile.orig,
#'               outfile              = "agerror.dat", 
#'               df                   =  df,
#'               agerr_mat            = agerr_mat,
#'               write_file           = FALSE,
#'               # mis_spec           = mis_spec
#'      ))
#' 
#' }


change_a <- function(dat_list, outfile, df, agerr_mat, mis_spec=NULL, write_file=TRUE){
  
  # remember that the default will be ageing error matrix 1 - no ageing error
  
  agecomp <- dat_list$agecomp
  df <- as.data.frame(df)
  names(df)[5] <- tolower(names(df)[5])
 
  if (is.null(mis_spec)) {
    
  # replace the number of defined matrices and the matrices in the .dat file
  new_mat <- as.data.frame(agerr_mat)
  names(new_mat) <- c("which_matrix",paste("age",0:c(ncol(new_mat)-2), sep=""))
  if (ncol(dat_list$ageerror) > c(ncol(new_mat)-1)){
    dat_list$ageerror <- dat_list$ageerror[,c(1:c(dim(new_mat)[2]-1))]# ageing error matrices must have same dimensions
  } else {
    new_mat <- new_mat[,c(1:c(dim(dat_list$ageerror)[2]+1))]# ageing error matrices must have same dimensions
  }
  dat_list$ageerror <- rbind(dat_list$ageerror,new_mat[,-1])
  
  dat_list$N_ageerror_definitions <- dim(dat_list$ageerror)[1]/2
  
  df$Gender <- dat_list$agecomp$Gender[1]
  
  # Now replace Agerr in agecomp from df data
  
  save_names <- names(agecomp)
  names(agecomp)[6] <- tolower(names(agecomp)[6])

  for (yr in unique(abs(agecomp$Yr))){
    sub <- agecomp[abs(agecomp$Yr)==yr,]
    for(seas in unique(sub$Seas)){
      sub1 <- sub[sub$Seas==seas,]
      if (dim(sub1)[1]>0){
        for(flt in unique(abs(sub1$FltSvy))){
          sub2 <- sub1[abs(sub1$FltSvy)==flt,]
          if (dim(sub2)[1]>0){
            for(sex in unique(sub2$Gender)){
              agecomp[abs(agecomp$Yr)==yr & agecomp$Seas==seas & abs(agecomp$FltSvy)==flt & agecomp$Gender==sex,"ageerr"] <-
                df[df$Yr==yr & df$Seas==seas & df$FltSvy==flt & df$Gender==sex,"ageerr"]
            }
          }
        }
      }
    }
  }
  
  names(agecomp) <- save_names
  agecomp -> dat_list$agecomp
  
  # If there are mean length at age they need to be replaced too
  
  if (dat_list$N_MeanSize_at_Age_obs !=0) {
    
    mlacomp <- dat_list$MeanSize_at_Age_obs
    save_names <- names(mlacomp)
    names(mlacomp)[6] <- tolower(names(mlacomp)[6])
    
    df$Gender <- mlacomp$Gender[1]
    
    for (yr in unique(abs(mlacomp$Yr))){
      sub <- mlacomp[abs(mlacomp$Yr==yr),]
      for(seas in unique(sub$Seas)){
        sub1 <- sub[sub$Seas==seas,]
        if (dim(sub1)[1]>0){
          for(flt in unique(abs(sub1$FltSvy))){
            sub2 <- sub1[abs(sub1$FltSvy)==flt,]
            if (dim(sub2)[1]>0){
              for(sex in unique(sub2$Gender)){
                mlacomp[abs(mlacomp$Yr)==yr & mlacomp$Seas==seas & abs(mlacomp$FltSvy)==flt & mlacomp$Gender==sex,"ageerr"] <-
                  df[df$Yr==yr & df$Seas==seas & df$FltSvy==flt & df$Gender==sex,"ageerr"]
              }
            }
          }
        }
      }
    }
    
    names(mlacomp) <- save_names
    mlacomp -> dat_list$MeanSize_at_Age_obs
    
  }
  
  }
  
  
if (!is.null(mis_spec)){
  if (grepl("em",outfile) == T) {
    agecomp <- dat_list$agecomp
    agecomp$Ageerr <- 1
    agecomp -> dat_list$agecomp
  }
}  
    ## Write the modified file
    if(write_file) {
      SS_writedat(datlist = dat_list, outfile = outfile, overwrite = TRUE, verbose = FALSE)
    }    
  invisible(dat_list)
}
