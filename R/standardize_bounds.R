#' Standardize the bounds of the estimation model control file.
#'
#' Function to standardize the bounds of the control file in the estimation
#' model. This function first checks to ensure the initial values in the
#' estimation model control file are set to the true values of the
#' \code{OM_ctl_file} and if not sets them for every parameter. Next, the
#' function adjusts the LO and HI values in the \code{EM_ctl_file} to
#' be a fixed percentage of the initial value for every parameter.
#'
#' @author Christine Stawitz
#'
#' @param percent_df A \code{data.frame} with nine rows and three columns.
#'   The first column is the parameter.
#'   The second column the % of the initial parameter value LO is set to.
#'   The third column the % of the initial parameter value HI is set to.
#' @param OM_ctl_file A string with the path and name of the operating model
#'   control file. If it is not given the part of the function which matches the
#'   OM and EM INIT values is ignored. Default is \code{""}.
#' @param EM_ctl_file A string with the path and name of the estimation model
#'   control file.
#' @importFrom r4ss SS_parlines SS_changepars
#' @export
#' @examples
#' \dontrun{
#' ## require(r4ss)
#' ## Set to the path and filename of the OM and EM control files
#' OM.ctl<-"control.ss_new"
#' EM.ctl<-"control.ss_new"
#'
#' ## Use SS_parlines to get the proper names for parameters for the data frame
#' om.pars<-SS_parlines(ctlfile=OM.ctl)
#' em.pars<-SS_parlines(ctlfile=EM.ctl)
#'
#' ## Set percentages to make lower and upper bounds
#' lo.percent<-rep(.5,11)
#' hi.percent<-c(500,1000,1000,rep(500,8))
#'
#' ##Populate data frame using EM parameter names and percentages
#' percent.df<-data.frame(label=as.character(em.pars[c(1:6,17,27:30),"Label"]),
#'   lo=lo.percent,hi=hi.percent)
#'
#' #Run function
#' standardize_bounds(percent_df,EM_ctl_file=EM.ctl,OM_ctl_file=OM.ctl)
#' }

standardize_bounds<-function(percent_df, EM_ctl_file, OM_ctl_file=""){

  #Read in EM values
  em_pars<-SS_parlines(ctlfile=EM_ctl_file)

  #1. ensure the INIT value in the EM ctl file is set to the OM true value
  #If an OM is passed
  if(nchar(OM_ctl_file)>0){

    #Read in OM true value
    om_pars<-SS_parlines(ctlfile=OM_ctl_file)

    #Restrict the parameters which have their initial values
    #set equal to only those which occur in both the EM and OM
    restr_percent_df <- percent_df[which(percent_df[,"Label"] %in%
      unique(c(om_pars[,"Label"], em_pars[, "Label"]))), ]

    if(!is.na(restr_percent_df)){

      #Get the indices of the user input parameters in the OM/EM
      om_indices<-which(om_pars[,"Label"] %in% restr_percent_df[,"Label"])
      em_indices<-which(em_pars[,"Label"] %in% restr_percent_df[,"Label"])


    #If they are not equal, set the EM initial value to the OM true value
      if(any(om_pars[om_indices,"INIT"]!= em_pars[em_indices,"INIT"])){
        inits_to_change <- em_pars[which(em_pars[em_indices,"INIT"] !=
                                         om_pars[om_indices,"INIT"]), "Label"]
        SS_changepars(dir=substr(EM_ctl_file, 1, nchar(EM_ctl_file)-9),
          ctlfile=substr(EM_ctl_file, nchar(EM_ctl_file)-8, nchar(EM_ctl_file)),
          newctlfile = substr(EM_ctl_file, nchar(EM_ctl_file)-8,
                              nchar(EM_ctl_file)),
          strings = inits_to_change,
          newvals = om_pars[which(om_pars[om_indices,"INIT"] !=
                                  em_pars[em_indices,"INIT"]),"INIT"])
      }
    }else{
      print("None of the entered parameter labels are found in both the EM and OM.")
    }
  }

  #Second, use the input data frame to set the LO and HI values of the EM control file
  #To a fixed % of the init value as provided in the user input

  #Read in parameters from EM ctl file
  em_pars<-SS_parlines(ctlfile=EM_ctl_file)

  #Check input parameter names are valid
  #Do these match the data frame first column?
  if(any(!percent_df[,"Label"] %in% em_pars[,"Label"])){
    print(paste("Element",which(!percent_df[,"Label"] %in% em_pars[,"Label"]),
      "does not have a valid parameter label."))
  }else{
    #Get indices of parameters to standardize; first column is in the data frame
    # and second is in the EM read values
    indices_to_standardize<-matrix(ncol=2,nrow=nrow(percent_df))
    indices_to_standardize[,1]<-which(percent_df[,1] %in% em_pars[,"Label"])
    indices_to_standardize[,2]<-which(em_pars[,"Label"] %in% percent_df[,1])

    #Change lo and hi's
    newlos <- percent_df[indices_to_standardize[, 1], "lo"] *
              em_pars[indices_to_standardize[, 2], "INIT"]
    newhis <- percent_df[indices_to_standardize[, 1], "hi"] *
              em_pars[indices_to_standardize[, 2], "INIT"]

    #If the parameter label contains "LnQ", use the value given in the
    #table rather than a percentage times the initial value.
    newlos[grep("LnQ", percent.df$Label, ignore.case = TRUE)] <-
      percent.df[grep("LnQ", percent.df$Label, ignore.case = TRUE), 2]
    newhis[grep("LnQ", percent.df$Label, ignore.case = TRUE)] <-
    percent.df[grep("LnQ", percent.df$Label, ignore.case = TRUE), 3]

    change_lo_hi(ctlfile=EM_ctl_file,newctlfile=EM_ctl_file,
                 strings=as.character(percent_df[indices_to_standardize[,1],1]),
      newlos=newlos,newhis=newhis)
  }
}

#' Changes the LO and HI bounds of the estimation model control file
#'
#' This is a modified version of \code{\link[r4SS]{SS_changepars}} which
#' modifies the LO and HI bounds in the \code{ctlfile} based on the INIT value.
#' \code{newhis} and \code{newlos} must be equivalent lengths
#' and both refer to the parameters in strings in the same order.
#'
#' @author Ian Taylor, modified by Christine Stawitz
#'
#' @param ctlfile Control file name with directory.
#' @param newctlfile Name of new control file to be written.
#' @param linenums Line numbers of control file to be modified. Either this or
#'   the Strings input are needed. Default is \code{NULL}.
#' @param strings Strings (with optional partial matching) indicating which
#'   parameters to be modified. This is an alternative to \code{linenums}.
#'   Strings correspond to commented parameter names included in a
#'   \code{control.ss_new}, or whatever is written as comment at the end of
#'   the 14 number parameter lines.
#' @param newlos Vector of new lo bounds. Default is \code{NULL}.
#' @param newhis Vector of new hi bounds. Must be the same length as
#'   \code{newhis}. Default is \code{NULL}.
#' @param estimate Vector of \code{TRUE}/\code{FALSE} for which changed
#'   parameters are to be estimated. Default is \code{FALSE}.
#' @param verbose More detailed output to command line. Default is \code{TRUE}.
#' @seealso \code{\link{SS_changepars}}
#' @importFrom r4ss SS_parlines
#' @export
change_lo_hi <- function (ctlfile = "control.ss_new",
          newctlfile = "control_modified.ss", linenums = NULL, strings = NULL,
          newlos = NULL, newhis = NULL, estimate = FALSE, verbose = TRUE)
{
  ctl = readLines(ctlfile)
  if (is.null(linenums) & !is.null(strings) & class(strings) ==
        "character") {
    ctltable <- SS_parlines(ctlfile = ctlfile)
    allnames <- ctltable$Label
    goodnames <- NULL

    if (!is.null(strings)) {
      for (i in 1:length(strings)) goodnames <- c(goodnames,
        allnames[grep(strings[i], allnames,fixed=TRUE)])
      goodnames <- unique(goodnames)
      cat("parameter names in control file matching input vector 'strings' (n=",
          length(goodnames), "):\n", sep = "")
      print(goodnames)
      if (length(goodnames) == 0) {
        stop("No parameters names match input vector 'strings'")
      }
    }
    nvals <- length(goodnames)
    cat("These are the ctl file lines as they currently exist:\n")
    print(ctltable[ctltable$Label %in% goodnames, ])
    for (i in 1:nvals) linenums[i] <- ctltable$Linenum[ctltable$Label ==
                                                         goodnames[i]]
  } else {
    if (is.null(linenums))
      stop("valid input needed for either 'linenums' or 'strings'")
  }
  ctlsubset <- ctl[linenums]
  cat("line numbers in control file (n=", length(linenums),
      "):\n", sep = "")
  print(linenums)
  newctlsubset <- NULL
  cmntvec <- NULL
  nvals <- length(linenums)
  oldlos <- oldhis <- oldphase <- newphase <- rep(NA, nvals)
  if (!is.null(newlos) & length(newlos) != nvals) {
    stop(paste("'newlos' and either 'linenums' or 'strings' should have the",
               "same number of elements"))
  }
  if (!is.null(newhis) & length(newhis) != nvals) {
      stop(paste("'newhis' and either 'linenums' or 'strings' should have",
                 "the same number of elements."))
  }
  if (!(length(estimate) %in% c(1, nvals)))
    stop("'estimate' should have 1 element or same number as 'newvals'")
  if (length(estimate) == 1)
    estimate <- rep(estimate, nvals)
  if (is.data.frame(newlos))
    newlos <- as.numeric(newlos)
  if (is.data.frame(newhis))
    newhis <- as.numeric(newhis)
  if (is.null(newlos))
    stop("Nothing input for 'newlos'")
  if (is.null(newhis))
    stop("Nothing input for 'newhis'")
  for (i in 1:nvals) {
    splitline <- strsplit(ctlsubset[i], "#")[[1]]
    cmnt <- paste("#", paste(splitline[-1], collapse = "#"),
                  sep = "")
    cmntvec <- c(cmntvec, cmnt)
    vecstrings <- strsplit(splitline[1], split = "[[:blank:]]+")[[1]]
    vec <- as.numeric(vecstrings[vecstrings != ""])
    if (max(is.na(vec)) == 1)
      stop(paste("There's a problem with a non-numeric value in line",
                 linenums[i]))
    oldlos[i] <- vec[1]
    oldhis[i] <- vec[2]
    if ((!is.null(oldlos))&(!is.null(oldhis)))
      vec[1] <- newlos[i]
      vec[2] <- newhis[i]
    oldphase[i] <- as.numeric(vec[7])
    if (estimate[i]) {
      vec[7] <- abs(oldphase[i])
    } else {
      vec[7] <- -abs(oldphase[i])
    }
    if (vec[1] > vec[3])
      cat("!warning: new lower bound ", vec[1], "is above initial value ",
          vec[3], "for", cmnt, "\n")
    if (vec[1] > vec[2])
      cat("!warning: new lower bound ", vec[1], "is above upper bound ",
          vec[2], "for", cmnt, "\n")
    newphase[i] <- vec[7]
    newline <- paste("", paste(vec, collapse = " "), cmnt)
    newctlsubset <- rbind(newctlsubset, newline)
  }
  newctl <- ctl
  newctl[linenums] <- newctlsubset
  writeLines(newctl, newctlfile)
  if (verbose)
    cat("\nwrote new file to", newctlfile, "with the following changes:\n")
  results <- data.frame(oldlos, newlos, oldhis, newhis, oldphase, newphase,
                        comment = cmntvec)
  if (is.null(newlos))
    newlos <- NA
  if (is.null(newhis))
    newhis <-NA
  if (verbose)
    print(results)
  return(invisible(results))
}
