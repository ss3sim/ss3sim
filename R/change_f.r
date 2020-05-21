#' Specify fishing mortality (\emph{F}) using the Stock Synthesis control file
#'
#' Replace or input fishing mortality (\emph{F}) for a Stock Synthesis (SS)
#' simulation via changes to the control file. \emph{F} for multiple fleets
#' and or seasons can be specified using list structures. If fishing only
#' occurrs within a single fleet, then vector inputs are acceptable.
#'
#' @details
#' The argument \code{years} is the only argument that must be a vector
#' or a list of vectors. Other arguments can be specified using a single
#' scalar value that will be repeated for all fisheries in all years.
#' If the input argument needs to be different for any year or fishery, the
#' argument must be a list with vectors for each fishery, where each vector
#' is the same length as the vectors within the \code{years} argument. Although,
#' both the \code{years} and other input arguments can be specified using a single
#' vector if the length of {\code{fleets}} is just one or a vector of values
#' is specified for fleets because all of these vectors will just be combined
#' into a single data frame. Where it gets complicated is when there are
#' mutliple fleet and year combinations, then it is best to just use the list
#' structure common to other functions within \pkg{ss3sim}.
#'
#' \code{change_f} overrides any \emph{F} values that are in the supplied
#' control file with the newly specified values, i.e., \code{fvals}.
#' Users do not need to specify values for years in which there
#' will be zero fishing because SS will automatically set them to zero when
#' running the operating model.
#' Using the control file rather than the par file to manipulate the
#' operating model requires a few other files within the operating
#' model folder to be set up in a particular manner. That is,
#' (1) the starter file must be set up to read parameters from the control file
#' rather than the par file and
#' (2) the data file must have a dummy catch entry for every year, fishery
#' combination that will be specified in the control file. If a year, fishery
#' combination is specified in the control file and not present in the data file,
#' then the entry in the control file will be ignored. ss3sim automatically corrects
#' for this using \code{\link{change_data}} within \code{\link{ss3sim_base}} by
#' specifying a row for every year and fleet combination possible.
#'
#' If used with \code{\link{run_ss3sim}}, the case file should be named
#' \code{F}. A suggested (default) case letter is \code{F}.
#'
#' @author Kelli Faye Johnson
#'
#' @template lcomp-agecomp-index
#' @param fisheries A deprecated argument that was replaced by \code{fleets} to
#' match the style of other ss3sim functions. Currently, it can still be used
#' to allow for back compatability with the specification of which fleet the
#' fishing mortality pertains to. A vector the same lengths as \code{years}
#' or a single integer value is acceptable.
#' @param fvals *A list of the same length as \code{fleets} with one
#' entry per fishing mortality level (\emph{F}) entry in \code{years}.
#' A single value will be repeated for every value in \code{years}. If more than
#' one fleet is present, then the single value will be used for all fleets, i.e.,
#' there is no way to map a single value to each year specific to the given fleet.
#' Instead you would need to provide a list of vectors of repeated values.
#' @param seasons A list of seasons to be entered into the
#' SS control file for each fleet.
#' The structure is the same as \code{fvals}, i.e., a list or a scalar.
#' The default is 1, which will be applied to all fleets in all years.
#' @param ses A list of fishing level standard errors (ses) to be entered into the
#' SS control file for each fleet.
#' The structure is the same as \code{fvals}, i.e., a list or a scalar.
#' The default is 0.005, which will be applied to all fleets in all years.
#' @template ctl_file_in
#' @template ctl_file_out
#' @return Modified SS control file.
#' @family change functions
#' @template casefile-footnote
#' @export
#' @examples
#' d <- system.file(file.path("extdata", "models"), package = "ss3sim")
#' # Using original vector-style inputs
#' change_f(years = 1:50, fleets = 1, fvals = 0.2,
#'   ctl_file_in = file.path(d, "cod-om", "codOM.ctl"),
#'   ctl_file_out = file.path(tempdir(), "control_fishing.ss"))
#' # Using list-style inputs for when there are multiple fisheries
#' change_f(years = list(1:5, 1:10), fleets = 3:4,
#'   fvals = list(rep(0.1, 5), rep(0.2, 10)),
#'   ctl_file_in = file.path(d, "cod-om", "codOM.ctl"),
#'   ctl_file_out = file.path(tempdir(), "control_fishing.ss"))
#' rm(d)
#'
change_f <- function(years, fleets, fisheries, fvals, seasons = 1, ses = 0.005,
  ctl_file_in, ctl_file_out = "control_fishing.ss") {

  if (!rlang::is_missing(fisheries)) {
    warning("fisheries in change_f is deprecated, use fleets argument instead")
    fleets <- fisheries
  }
  if (is.list(years)) {
    times <- sapply(years, length)
  } else {
    times <- length(years)
    years <- list(years)
  }
  stopifnot(length(years) == length(fleets))
  fvals <- scalar2list(fvals, length = times)
  ses <- scalar2list(ses, length = times)
  seasons <- scalar2list(seasons, length = times)
  newdata <- lists2df(
    "Yr" = years,
    "Seas" =  seasons,
    "F_value" = fvals,
    "se" = ses,
    "phase" = scalar2list(1, length = times))
  newdata[, "index"] <- fleets[newdata$index]
  names(newdata) <- gsub("index", "Fleet", names(newdata))
  ctl <- readLines(ctl_file_in)
  locations <- grep("F_Method", ctl, ignore.case = TRUE)
  if (length(locations) < 2) {
    # todo: use r4ss::SS_readctl to pass a control list rather than readLines
    stop("Phrase 'F_Method' should be found at least 2 times in the control ",
         "file, but was found ", length(locations), " times. Please make sure ",
         "a control file with standard SS comments is being used.")
  }
  # Check that F method = 2 b/c will not work with ctl files with Fmethod = 1 or 3.
  F_method <- as.numeric(trimws(strsplit(ctl[locations[1]], "#", fixed = TRUE)[[1]][1]))
  if(F_method != 2) {
    stop("change_F only works with F_method = 2, not 1 or 3. The F_method ",
         "found is ", F_method)
  }
  locations <- locations[c(1, length(locations))]
  location_terminal <- grep("Q_setup", ctl, ignore.case = FALSE)
  if (length(location_terminal) == 0) {
    stop("Q_setup was not found in the ctl_file_in")
  }
  ctl[locations[1]] <- gsub("^[1-4]\\s*", "2 ", trimws(ctl[locations[1]]))
  ctl <- c(ctl[1:locations[1]],
    paste(ifelse(max(newdata$F_value) < 4, 4, max(newdata$F_value) * 2),
      " # max F or harvest rate, depends on F_Method"),
    paste(0, 1, NROW(newdata),
      "# overall start F value; overall phase; N detailed inputs to read"),
    apply(newdata, 1, paste, collapse = " "),
    ctl[location_terminal:length(ctl)])

  # Write new control file
  if (!is.null(ctl_file_out)) {
    writeLines(ctl, con = ctl_file_out)
    close(file(ctl_file_out))
  }
  invisible(ctl)
}
