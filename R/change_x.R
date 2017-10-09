#' Function to mis-specify dimorphism (i.e. remove it) in EM \code{.dat} and \code{.ctl} files.
#'
#' @description Takes ss3 \code{.dat} and \code{.ctl} files and removes the sex-specific
#' parameters to create single sex files. The function can be called by
#'   itself or within \code{\link{run_ss3sim}} to alter an estimation model
#'   \code{.dat} and \code{.ctl} file.
#'   If used with \code{\link{run_ss3sim}} the case file should be named
#'   \code{X}. A suggested (default) case letter is \code{X} for seX.
#'
#' @param dat_file_in Input SS3 data file
#' @param dat_file_out Output SS3 data file
#' @param ctl_file_in  Input SS3 control file
#' @param ctl_file_out Output SS3 control file
#' @param change_dat *If TRUE, the function will change the 2 sex model \code{.dat} file
#' and turn it into single sex
#' @param change_ctl *If TRUE, the function will change the 2 sex model \code{.ctl} file
#' and turn it into single sex
#' @template write_file
#'
#' @details \code{change_x} removes sex dimorphism in the \code{.dat} file
#' by summing up males and females lcomp and agecomp. It also calculates the average 
#' mlacomp if mlacomp is present in the \code{.dat} file. Growth and selectivity are changed
#' in the \code{.ctl} file simply by commenting out the male parameters. The initial values 
#' and bonds should be changed before running the simulation outside of ss3sim. An option 
#' will be added later on to do that here.
#' 
#' @note ROOM FOR IMPROVEMENT - MORE CHECKS TO DO... AND COULD DO WITH IMPROVING 
#' THE ARGUMENTS IN.
#' @template casefile-footnote
#' @family change functions
#' @return
#' Altered versions of SS3 \code{.dat} and \code{.ctl} files are written
#' to the disk and the altered \code{dat_file_in} is also returned invisibly.
#'
#' @author Gwladys Lambert
#' @importFrom r4ss SS_parlines
#' @export
#' @examples
#' \dontrun{
#' temp_path <- tempdir()
#' dir.create(temp_path, showWarnings = FALSE)
#' wd        <- getwd()
#' setwd(temp_path)
#' 
#' # Find the SS3sim simple model in the package:
#' d         <- system.file("extdata/new models", package = "ss3sim")
#' simple_sex    <- paste0(d, "/GOAflathead")
#' file.copy(simple_sex, ".", recursive = TRUE)
#' setwd("GOAflathead")
#' 
#' wd <- getwd()
#' 
#' 
#' ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
#' ### THIS IS THE FUNCTION ALONE - CAN BE RUN FOR ANY SCENARIO ABOVE ###
#' ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
#' 
#' setwd(wd)
#' 
#' out <- change_x(dat_file_in = "om/ss3.dat", dat_file_out = "om/ss3.dat", 
#'                 ctl_file_in = "om/ss3.ctl", ctl_file_out = "om/ss3.ctl", 
#'                 change_dat= TRUE, change_ctl = TRUE,
#'                 write_file=FALSE) # write file controls .dat, not .ctl (always overwrite)
#' 
#' }


change_x <- function(dat_file_in, dat_file_out, 
                     ctl_file_in, ctl_file_out, 
                     change_dat= TRUE, change_ctl = TRUE,
                     write_file=TRUE){
  
  ### CHANGE THE DATA FILE

  # Add a condition in case 0 age data are sampled ??? ##################################################################################################### !!!!!
  
  if(change_dat ==TRUE) {
    dat_list <- SS_readdat(dat_file_in, verbose=F)  
    agecomp <- dat_list$agecomp  
    fem <- agecomp[,c(10:c(10+c(dim(agecomp)[2]-10)/2))] 
    fem <- fem/apply(fem,1,sum)
    mal <- agecomp[,c(c(10+ncol(fem)):dim(agecomp)[2])]
    mal <- mal/apply(mal,1,sum)
    both <- (fem + mal)/2
    both <- both/apply(both,1,sum)
    agecomp <- cbind(agecomp[,c(1:9)], both)
    agecomp$Gender <- 0
    
    lencomp <- dat_list$lencomp  
    fem <- lencomp[,c(7:c(7+c(dim(lencomp)[2]-7)/2))] 
    fem <- fem/apply(fem,1,sum)
    mal <- lencomp[,c(c(7+ncol(fem)):dim(lencomp)[2])]
    mal <- mal/apply(mal,1,sum)
    both <- (fem + mal)/2
    lencomp <- cbind(lencomp[,c(1:6)], both)
    lencomp$Gender <- 0
      
    dat_list$Ngenders <- 1
    dat_list$agecomp <- agecomp
    dat_list$lencomp <- lencomp

    
    if (dat_list$N_MeanSize_at_Age_obs != 0) {
      MeanSize_at_Age_obs <- dat_list$MeanSize_at_Age_obs
      sizes <- MeanSize_at_Age_obs[,c(8:c(8+c(dim(MeanSize_at_Age_obs)[2]-8)/2))] 
      fem <- sizes[,c(1:c(dim(sizes)[2]/2))] 
      mal <- sizes[,c(c(1+ncol(fem)):dim(sizes)[2])]
      both_sizes <- (fem + mal)/2
      numbers <- MeanSize_at_Age_obs[,c(c(8+dim(sizes)[2]):dim(MeanSize_at_Age_obs)[2])] 
      fem <- numbers[,c(1:c(dim(sizes)[2]/2))] 
      mal <- numbers[,c(c(1+ncol(fem)):dim(numbers)[2])]
      both_numbers <- (fem + mal)/2
      MeanSize_at_Age_obs <- cbind(MeanSize_at_Age_obs[,c(1:7)], both_sizes, both_numbers)
      MeanSize_at_Age_obs$Gender <- 0
      dat_list$MeanSize_at_Age_obs <- MeanSize_at_Age_obs
    }
    
    
    ## Write the modified file
    if(write_file) {
      SS_writedat(datlist = dat_list, outfile = dat_file_out, overwrite = TRUE, verbose = FALSE)
    }
    
  }
  
  ### CHANGE THE CTL FILE
  
  # Remove extra lines in growth and selex
  
  if(change_ctl ==TRUE) {
    
    nflt <- dat_list$Nfleet + dat_list$Nsurveys 
    nfleets= nflt
    misspec= TRUE
    dir=getwd()
    ctlfile = ctl_file_in
    newctlfile = ctl_file_out
    sex_pars = c("_Mal","Male")
    verbose = FALSE
    newvals = NULL
    
    fullctlfile <- file.path(dir,ctlfile)
    ctl <- readLines(fullctlfile)
    ctl <- gsub("\t", " ", ctl)
    
    ctltable <- SS_parlines(ctlfile = fullctlfile)
    allnames <- ctltable$Label
    
    newctl <- ctl
    
    sex_pars_save <- NULL
    
    for (idx in sex_pars) {
      
      goodnames <- NULL
      
      goodnames <- c(goodnames, allnames[grep(idx, 
                                              allnames, fixed = TRUE)])
      goodnames <- unique(goodnames)
      if (verbose) {
        cat("parameter names in control file matching input vector 'strings' (n=", 
            length(goodnames), "):\n", sep = "")
        print(goodnames)
      }
      
      if (length(goodnames) != 0) {
      #   stop("No parameters names match input vector 'strings'")
      # }
       nvals <- length(goodnames)
      if (verbose) {
        cat("These are the ctl file lines as they currently exist:\n")
        print(ctltable[ctltable$Label %in% goodnames, ])
      }
      linenums <- NULL
      for (i in 1:nvals) {
        linenums[i] <- ctltable$Linenum[ctltable$Label == 
                                          goodnames[i]]
      }
      ctlsubset <- ctl[linenums]
      if (verbose) {
        cat("line numbers in control file (n=", length(linenums), 
            "):\n", sep = "")
        print(linenums)
      }
      newctlsubset <- NULL
      cmntvec <- NULL
      nvals <- length(linenums)
      
      for (i in 1:nvals) {
        splitline <- strsplit(ctlsubset[i], "#")[[1]]
        cmnt <- paste("#", paste(splitline[-1], collapse = "#"), 
                      sep = "")
        cmntvec <- c(cmntvec, cmnt)
        vecstrings <- strsplit(splitline[1], split = "[[:blank:]]+")[[1]]
        vec <- as.numeric(vecstrings[vecstrings != ""])
        
        if (!is.null(misspec)) newline <- paste("#", paste(vec, collapse = " "), cmnt) ########## ADDITION ######
        newctlsubset <- rbind(newctlsubset, newline)
      }
      
      newctl[linenums] <- newctlsubset

      sex_pars_save <- c(sex_pars_save, idx)
      }
      
      
      if (!is.null(sex_pars_save) & "Male" %in% sex_pars_save) {
        
        start <- which(ctl %in% ctl[grep("_Pattern ", ctl)])+1
        end <- which(ctl %in% ctl[grep("_Pattern ", ctl)])+nfleets
        linenums <- c(start[1]:end[1],start[2]:end[2])
        subsetctl <- ctl[linenums]

        newctlsubset <- NULL
        cmntvec <- NULL
        nvals <- length(subsetctl)
        
        for (i in 1:nvals) {
          splitline <- strsplit(subsetctl[i], "#")[[1]]
          cmnt <- paste("#", paste(splitline[-1], collapse = "#"), 
                        sep = "")
          cmntvec <- c(cmntvec, cmnt)
          vecstrings <- strsplit(splitline[1], split = "[[:blank:]]+")[[1]]
          vec <- as.numeric(vecstrings[vecstrings != ""])
          vec[3] <- 0
          
          if (!is.null(misspec)) newline <- paste("", paste(vec, collapse = " "), cmnt) ########## ADDITION ######
          newctlsubset <- rbind(newctlsubset, newline)
        }
        newctl[linenums] <- newctlsubset
      }
      writeLines(newctl, file.path(dir, newctlfile))
      
    }
  }
  invisible(return(dat_list))
}
