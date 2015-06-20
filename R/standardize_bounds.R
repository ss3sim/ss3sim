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
#'   The second column is the percent of the initial parameter value LO is set to.
#'   The third column is the percent of the initial parameter value HI is set to.
#' @param dir A path to the directory containing the model files.
#' @param om_ctl_file A string with the name of the operating model
#'   control file. If it is not given the part of the function which matches the
#'   OM and EM INIT values is ignored. Default is \code{""}.
#'   \code{om_ctl_file} must be located in \code{dir}.
#' @param em_ctl_file A string with the name of the estimation model
#'   control file. \code{em_ctl_file} must be located in \code{dir}.
#' @param verbose Detailed output to command line. Default is \code{FALSE}.
#' @param estimate A logical for which changed parameters are to be estimated.
#'   Used by \code{\link[r4ss]{SS_changepars}}, where in \pkg{r4ss} the default
#'   is \code{FALSE}, which turns all parameter estimation off. Here the default
#'   is \code{NULL}, which will leave parameter phases unchanged.
#' @param ... Any other arguments to pass to \code{\link[r4ss]{SS_changepars}}.
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
#' standardize_bounds(percent_df = percent_df, dir = temp_path, em_ctl_file = "em.ctl",
#'                    om_ctl_file = "om.ctl")
#' unlink(temp_path, recursive = TRUE)
#'
#' setwd(wd)
#' }

standardize_bounds <- function(percent_df, dir, em_ctl_file, om_ctl_file = "",
                               verbose = FALSE, estimate = NULL, ...) {
  # Check that EM file exists
  if (!file.exists(file.path(dir, em_ctl_file))) {
    stop(paste("The em_ctl_file,", em_ctl_file, "does not exist",
               "in the directory", dir))
  }
  if (!"Label" %in% colnames(percent_df)) {
    stop(paste("In percent_df, the first column is currently named",
      colnames(percent_df)[1], "rename as 'Label'"))
  }
  #Read in EM values
  em_pars <- SS_parlines(ctlfile = file.path(dir, em_ctl_file),
                         verbose = verbose)
 #If an OM is passed
  if(nchar(om_ctl_file)>0){

    #Read in OM true value
    om_pars <- SS_parlines(ctlfile = file.path(dir, om_ctl_file),
                           verbose = verbose)

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
      print.verbose <- SS_changepars(dir = dir, ctlfile = em_ctl_file,
          newctlfile = em_ctl_file, strings = changeinits[, 1],
          newvals = changeinits[, 2], verbose = verbose, repeat.vals = FALSE)
      if (verbose) message(paste(print.verbose, collapse = "\n"))

    om_pars<-SS_parlines(ctlfile = file.path(dir,om_ctl_file), verbose = verbose)

   #Restrict the parameters which have their initial values
    #set equal to only those which occur in both the EM and OM
    parsinboth <- which(percent_df$Label %in% om_pars$Label &
                        percent_df$Label %in% em_pars$Label)
    restr_percent_df <- percent_df[parsinboth, ]

    if(NROW(restr_percent_df) != 0){

      #Get the indices of the user input parameters in the OM/EM
      om_indices<-which(om_pars[,"Label"] %in% restr_percent_df[,"Label"])
      em_indices<-which(em_pars[,"Label"] %in% restr_percent_df[,"Label"])

    #If they are not equal, set the EM initial value to the OM true value
    whichunequal <- om_pars[om_indices,"INIT"]!= em_pars[em_indices,"INIT"]
      if(any(whichunequal)){
        inits_to_change <- em_pars[which(whichunequal), "Label"]

        SS_changepars(dir=dir, ctlfile=em_ctl_file,newctlfile = em_ctl_file,
                    strings = inits_to_change,
          newvals = om_pars[which(whichunequal),"INIT"],
                    verbose = verbose)
      }
    }else{
      message("None of the entered parameter labels are found in both the EM and OM.")
    }
  }
  }

  #2: Use input data frame to set the LO and HI values of the EM ctl file
  #To a fixed % of the init value as provided in the user input

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
  }else{
    #Get indices of parameters to standardize; first column is in the data frame
  # and second is in the EM read values
    indices_to_standardize<-matrix(ncol=2,nrow=nrow(percent_df))
    indices_to_standardize[, 1] <- 1:NROW(percent_df)
    indices_to_standardize[, 2] <- indexem

    #Change lo and hi's
    newlos <- percent_df[indices_to_standardize[, 1], "lo"] *
              em_pars[indices_to_standardize[, 2], "INIT"]
    newhis <- percent_df[indices_to_standardize[, 1], "hi"] *
              em_pars[indices_to_standardize[, 2], "INIT"]

    #If the parameter label contains "Ln", use the value given in the
    #table rather than a percentage times the initial value.
    newlos[grep("Ln", percent_df$Label, ignore.case = TRUE)] <-
      percent_df[grep("Ln", percent_df$Label, ignore.case = TRUE), 2]
    newhis[grep("Ln", percent_df$Label, ignore.case = TRUE)] <-
    percent_df[grep("Ln", percent_df$Label, ignore.case = TRUE), 3]

    #Same for CV
    newlos[grep("CV", percent_df$Label, ignore.case = TRUE)] <-
      percent_df[grep("CV", percent_df$Label, ignore.case = TRUE), 2]
    newhis[grep("CV", percent_df$Label, ignore.case = TRUE)] <-
      percent_df[grep("CV", percent_df$Label, ignore.case = TRUE), 3]

    SS_changepars(dir=dir,ctlfile=em_ctl_file,newctlfile=em_ctl_file,
      linenums = em_pars[indexem, "Linenum"],
      newlos=newlos,newhis=newhis, verbose = verbose, estimate = estimate, ...)

    }

}
