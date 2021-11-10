#' Specify fishing mortality, \eqn{F}, using the Stock Synthesis control file
#'
#' Replace or input a time series of fishing mortality, \eqn{F}, values into
#' a Stock Synthesis control file.
#' In Stock Synthesis, inserting \eqn{F} values in this manner,
#' relies on the assumption that \eqn{F} operates continuously throughout the year and
#' and the process operates jointly with natural mortality
#' (Baranov 1918; [Branch 2009](https://cdnsciencepub.com/doi/10.1139/F08-196)).
#' The documentation for Stock Synthesis also describes this process as
#' \eqn{F} method == 2, where \eqn{F} is continuous and modeled using
#' full parameters.
#'
#' @details
#' The argument `years` is the only argument that must be a vector
#' or a list of vectors. Other arguments can be specified using a single
#' scalar value that will be repeated for all fisheries in all years.
#' If the input argument needs to be different for any year or fishery, the
#' argument must be a list with vectors for each fishery, where each vector
#' is the same length as the vectors within the `years` argument. Although,
#' both the `years` and other input arguments can be specified using a single
#' vector if the length of `fleets` is just one or a vector of values
#' is specified for fleets because all of these vectors will just be combined
#' into a single data frame. Where it gets complicated is when there are
#' multiple fleet and year combinations, then it is best to just use the list
#' structure common to other functions within \pkg{ss3sim}.
#'
#' [change_f()] overrides any \eqn{F} values that are in the supplied
#' control file with the newly specified values, i.e., `fvals`.
#' Users do not need to specify values for years in which there
#' will be zero fishing because Stock Synthesis will automatically set them to zero when
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
#' for this using [ss3sim_base()] by
#' specifying a row for every year and fleet using [change_catch()].
#'
#' @author Kelli Faye Johnson
#'
#' @template lcomp-agecomp-index
#' @param fvals A list of the same length as `fleets` with one
#' entry per fishing mortality level, \eqn{F}, entry in `years`.
#' A single value will be repeated for every value in `years`. If more than
#' one fleet is present, then the single value will be used for all fleets, i.e.,
#' there is no way to map a single value to each year specific to the given fleet.
#' Instead you would need to provide a list of vectors of repeated values.
#' @param seasons A list of seasons to be entered into the
#' Stock Synthesis control file for each fleet.
#' The structure is the same as `fvals`, i.e., a list or a scalar.
#' The default is 1, which will be applied to all fleets in all years.
#' @param ses A list of fishing level standard errors (ses) to be entered into the
#' Stock Synthesis control file for each fleet.
#' The structure is the same as `fvals`, i.e., a list or a scalar.
#' The default is 0.005, which will be applied to all fleets in all years.
#' @template ctl_list
#' @return Modified Stock Synthesis control file list.
#' @seealso See [r4ss::SS_readctl()] and [r4ss::SS_writectl()]
#' for how to supply `ctl_list` and how to write the file back to the disk
#' once you are done manipulating the list object.
#' @family change functions
#' @export
#' @examples
#' dat <- r4ss::SS_readdat(
#'   system.file("extdata","models", "cod-om", "codOM.dat", package = "ss3sim"),
#'   verbose = FALSE)
#' ctl <- r4ss::SS_readctl(
#'   system.file("extdata","models", "cod-om", "codOM.ctl", package = "ss3sim"),
#'   verbose = FALSE, use_datlist = TRUE, datlist = dat)
#' # Using original vector-style inputs
#' newctl <- change_f(years = 1:50, fleets = 1, fvals = 0.2, ctl_list = ctl)
#' # Using list-style inputs for when there are multiple fisheries
#' newctl <- change_f(years = list(1:5, 1:10), fleets = 3:4,
#'   fvals = list(rep(0.1, 5), rep(0.2, 10)), ctl_list = ctl)
#' rm(dat, ctl, newctl)
#'
change_f <- function(
  years,
  fleets,
  fvals,
  seasons = 1,
  ses = 0.005,
  ctl_list
) {

  #### Input checks
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
  stopifnot(ctl_list[["F_Method"]] == 2)

  #### Create data frame for Stock Synthesis
  # Fleet Yr Seas F_value se phase
  newdata <- lists2df(
    "Yr" = years,
    "Seas" =  seasons,
    "F_value" = fvals,
    "se" = ses,
    "phase" = scalar2list(1, length = times))
  newdata[, "index"] <- fleets[newdata$index]
  names(newdata) <- gsub("index", "Fleet", names(newdata))

  #### Change control list
  nonequilibrium <- newdata[newdata[["Yr"]] != -999, ]
  ctl_list[["maxF"]] <- ifelse(
    test = max(newdata[["F_value"]]) < 4,
    yes = 4,
    no = max(utils::type.convert(newdata[["F_value"]], as.is = TRUE)) * 2
  )
  ctl_list[["F_setup"]][1] <- 0 # Initial F value
  ctl_list[["F_setup"]][2] <- 1 # F phase
  ctl_list[["F_setup"]][3] <- NROW(nonequilibrium) # N F values to read
  ctl_list[["F_setup2"]] <- nonequilibrium
  if (any(newdata[["Yr"]] == -999)) {
    equilibrium <- newdata[newdata[["Yr"]] == -999 & newdata[["F_value"]] > 0, ]
    equilibrium <- equilibrium[order(equilibrium[["Seas"]], equilibrium[["Fleet"]]), ]
    #_ LO HI INIT PRIOR PR_SD  PR_type PHASE
    ctl_list[["init_F"]] <- data.frame(
      LO = 0,
      HI = ctl_list[["maxF"]],
      INIT = equilibrium[["F_value"]],
      PRIOR = 0, PR_SD = 99, PR_TYPE = 0, PHASE = 1
    )
    rownames(ctl_list[["init_F"]]) <- paste0(
      "InitF_seas_",
      equilibrium[["Seas"]],
      "_flt_",
      equilibrium[["Fleet"]]
      )
  }

  return(invisible(ctl_list))
}
