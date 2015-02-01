#' Standardize the bounds of the estimation model control file.
#'
#' Function to standardize the bounds of the control file in the estimation
#' model. This function first checks to ensure the initial values in the
#' estimation model control file are set to the true values of the
#' \code{om_ctl_file} and if not sets them for every parameter. Next, the
#' function adjusts the LO and HI values in the \code{em_ctl_file} to
#' be a fixed percentage of the initial value for every parameter.
#'
#' @author Christine Stawitz
#'
#' @param percent_df A \code{data.frame} with nine rows and three columns.
#'   The first column is the parameter.
#'   The second column is the % of the initial parameter value LO is set to.
#'   The third column is the % of the initial parameter value HI is set to.
#' @param om_ctl_file A string with the path and name of the operating model
#'   control file. If it is not given the part of the function which matches the
#'   OM and EM INIT values is ignored. Default is \code{""}.
#' @param em_ctl_file A string with the path and name of the estimation model
#'   control file.
#' @param verbose Detailed output to command line. Default is \code{FALSE}.
#' @param ... Any other arguments to pass to \code{\link{change_lo_hi}}.
#' @importFrom r4ss SS_parlines SS_changepars
#' @export
#' @examples
#' \dontrun{
#' temp_path <- file.path(tempdir(), "standardize-bounds-example")
#' dir.create(temp_path, showWarnings = FALSE)
#' wd <- getwd()
#' setwd(temp_path)
#'
#' ## Set to the path and filename of the OM and EM control files
#' OM.ctl <- system.file("extdata", "models", "cod-om", "codOM.ctl",
#'   package = "ss3sim")
#' EM.ctl <- system.file("extdata", "models", "cod-em", "codEM.ctl",
#'   package = "ss3sim")
#' file.copy(OM.ctl, "om.ctl")
#' file.copy(EM.ctl, "em.ctl")
#'
#' ## Use SS_parlines to get the proper names for parameters for the data frame
#' om.pars <- r4ss::SS_parlines(ctlfile="om.ctl")
#' em.pars <- r4ss::SS_parlines(ctlfile="em.ctl")
#'
#' ## Set percentages to make lower and upper bounds
#' lo.percent<-rep(.5,11)
#' hi.percent<-c(500,1000,1000,rep(500,8))
#'
#' ##Populate data frame using EM parameter names and percentages
#' percent_df<-data.frame(Label=as.character(em.pars[c(1:6,17,27:30),"Label"]),
#'   lo=lo.percent,hi=hi.percent)
#'
#' ##Run function
#' standardize_bounds(percent_df = percent_df, em_ctl_file = "em.ctl",
#'                    om_ctl_file = "om.ctl")
#'
#' setwd(wd)
#' }

standardize_bounds <- function(percent_df, em_ctl_file, om_ctl_file = "",
                               verbose = FALSE, ...) {

  #Read in EM values
  em_pars<-SS_parlines(ctlfile=em_ctl_file)

  #1. ensure the INIT value in the EM ctl file is set to the OM true value
  #If an OM is passed
  if(nchar(om_ctl_file)>0){

    #Read in OM true value
    om_pars<-SS_parlines(ctlfile=om_ctl_file)

    #Restrict the parameters which have their initial values
    #set equal to only those which occur in both the EM and OM
    # If par is not found change from "integer(0)" to NA
    indices <- sapply(percent_df$Label, function(x) {
      rmpuncx <- gsub("[[:punct:]]", "", x)
      rmpuncom <- gsub("[[:punct:]]", "", om_pars$Label)
      rmpuncem <- gsub("[[:punct:]]", "", em_pars$Label)
      findinom <- grep(rmpuncx, rmpuncom, ignore.case = TRUE)
      findinem <- grep(rmpuncx, rmpuncem, ignore.case = TRUE)
      c(ifelse(is.null(findinom), NA, findinom),
        ifelse(is.null(findinem), NA, findinem))
    })
    tochange <- !is.na(indices[1, ]) | !is.na(indices[2, ])
    restr_percent_df <- percent_df[tochange, ]
    if (NROW(restr_percent_df) == 0) {
      stop(paste("None of the entered parameter labels (,",
                 paste(percent_df[, 1], collapse = ", "),
                 ") are found in both the EM and OM.", sep = ""))
    }

    changeem <- cbind(em_pars$Label[indices[2, ]],
                      om_pars$INIT[indices[1, ]], em_pars$INIT[indices[2, ]])
    changeem <- changeem[tochange, ]
    changeinits <- changeem[which(changeem[, 2] != changeem[, 3]), ,
                            drop = FALSE]
    if (NROW(changeinits) > 0) {
      # TODO: eventually remove capture.output when r4ss uses verbose to capture
      # the output from SS_changepars
      print.verbose <- capture.output(
        SS_changepars(dir = ".", ctlfile = em_ctl_file,
          newctlfile = em_ctl_file, strings = changeinits[, 1],
          newvals = changeinits[, 2], verbose = FALSE, repeat.vals = FALSE))
      if (verbose) message(paste(print.verbose, collapse = "\n"))
    }
  }

  #2: Use input data frame to set the LO and HI values of the EM ctl file
  #To a fixed % of the init value as provided in the user input

  #Read in parameters from EM ctl file
  em_pars<-SS_parlines(ctlfile=em_ctl_file)
  #Check input parameter names are valid
  #Do these match the data frame first column?
  indexem <- sapply(percent_df$Label, function(x) {
      rmpuncx <- gsub("[[:punct:]]", "", x)
      rmpuncem <- gsub("[[:punct:]]", "", em_pars$Label)
      findinem <- grep(rmpuncx, rmpuncem, ignore.case = TRUE)
      ifelse(is.null(findinem), NA, findinem)
    })
  if (any(is.na(indexem))) {
    stop(paste("Element(s):",
               paste(percent_df$Label[which(is.na(indexem))], collapse = ", "),
               "do not have valid parameter labels."))
  }
    #Get indices of pars to standardize; first column is in the data frame
    # and second is in the EM read values
    indices_to_standardize<-matrix(ncol=2,nrow=nrow(percent_df))
    indices_to_standardize[, 1] <- 1:NROW(percent_df)
    indices_to_standardize[, 2] <- indexem

    #Change lo and hi's
    newlos <- percent_df[indices_to_standardize[, 1], "lo"] *
              em_pars[indices_to_standardize[, 2], "INIT"]
    newhis <- percent_df[indices_to_standardize[, 1], "hi"] *
              em_pars[indices_to_standardize[, 2], "INIT"]

    #If the parameter label contains "LnQ", use the value given in the
    #table rather than a percentage times the initial value.
    newlos[grep("LnQ", percent_df$Label, ignore.case = TRUE)] <-
      percent_df[grep("LnQ", percent_df$Label, ignore.case = TRUE), 2]
    newhis[grep("LnQ", percent_df$Label, ignore.case = TRUE)] <-
    percent_df[grep("LnQ", percent_df$Label, ignore.case = TRUE), 3]

    change_lo_hi(ctlfile=em_ctl_file,newctlfile=em_ctl_file,
                 strings=as.character(percent_df[indices_to_standardize[,1],1]),
      newlos=newlos,newhis=newhis, verbose = verbose, ...)
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
#'   parameters are to be estimated. If \code{NULL}, the phase for each
#'   parameter will remain unchanged. If a single \code{TRUE} or \code{FALSE}
#'   is provided the argument will be repeated for each parameter.
#'   Default is \code{NULL}.
#' @param verbose More detailed output to command line. Default is \code{TRUE}.
#' @param write_file A logical argument, specifying if the \code{newctlfile}
#'   should be written to the disk or not.
#' @seealso \code{\link{SS_changepars}}
#' @importFrom r4ss SS_parlines
#' @export
change_lo_hi <- function (ctlfile = "control.ss_new",
          newctlfile = "control_modified.ss", linenums = NULL, strings = NULL,
          newlos = NULL, newhis = NULL, estimate = NULL, verbose = TRUE,
          write_file = TRUE) {
  ctl = readLines(ctlfile)
  if (is.null(linenums) & !is.null(strings) & class(strings) ==
        "character") {
    ctltable <- SS_parlines(ctlfile = ctlfile)
    allnames <- ctltable$Label
    allnamesfx <- sapply(allnames, function(x) gsub("[[:punct:]]", "", x))
    goodnames <- NULL

    if (!is.null(strings)) {
      for (i in 1:length(strings)) {
        rmpunc <- gsub("[[:punct:]]", "", strings[i])
        goodnames <- c(goodnames,
          allnames[grep(rmpunc, allnamesfx, ignore.case = TRUE)])
      }
      goodnames <- unique(goodnames)
      if (length(goodnames) == 0) {
        stop("No parameters names match input vector 'strings'")
      }
    }
    nvals <- length(goodnames)
    for (i in 1:nvals) {
      linenums[i] <- ctltable$Linenum[ctltable$Label == goodnames[i]]
    }
  } else {
    if (is.null(linenums))
      stop("valid input needed for either 'linenums' or 'strings'")
  }
  ctlsubset <- ctl[linenums]
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
  if (!any(is.null(estimate)) & !(length(estimate) %in% c(1, nvals)))
    stop(paste("If 'estimate' is not NULL, it should have 1 element",
               "or be the same length as 'newvals'"))
  if (!any(is.null(estimate)) & length(estimate) == 1)
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
                 linenums[i], "pertaining to", cmnt))
    oldlos[i] <- vec[1]
    oldhis[i] <- vec[2]
    if ((!is.null(oldlos))&(!is.null(oldhis)))
      vec[1] <- newlos[i]
      vec[2] <- newhis[i]
    oldphase[i] <- as.numeric(vec[7])
    if (any(is.null(estimate))) {
      vec[7] <- oldphase[i]
    } else {
      vec[7] <- ifelse(estimate[i], abs(oldphase[i]), -abs(oldphase[i]))
    }
    if (vec[1] > vec[3])
      warning(paste("new lower bound", vec[1], "is above initial value",
                    vec[3], "for", cmnt))
    if (vec[1] > vec[2])
      warning(paste("new lower bound", vec[1], "is above upper bound",
                    vec[2], "for", cmnt))
    newphase[i] <- vec[7]
    newline <- paste("", paste(vec, collapse = " "), cmnt)
    newctlsubset <- rbind(newctlsubset, newline)
  }
  newctl <- ctl
  newctl[linenums] <- newctlsubset
  writeLines(newctl, newctlfile)
  results <- data.frame(oldlos, newlos, oldhis, newhis, oldphase, newphase,
                        comment = cmntvec)
  if (is.null(newlos))
    newlos <- NA
  if (is.null(newhis))
    newhis <-NA
  if (verbose & write_file) {
    message(paste("wrote new file to", newctlfile))
  }
  return(invisible(results))
}
