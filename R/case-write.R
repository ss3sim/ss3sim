#' Write a case file for length- or age-composition data
#'
#' @param fleets Vector of fleet numbers, where the order of
#'   \code{fleets} will dictate the order of all remaining arguments.
#' @param Nsamp A list containing vectors of sample sizes for each
#'   year for each fleet.
#' @param years A list containing vectors of years for each year for each fleet.
#' @param cpar A vector of cpar for each fleet.
#' @param type A character value of \code{"agecomp"} or \code{"lcomp"},
#'   to write age- or length-composition specifications, respectively.
#' @param case The case you want to write to. E.g. \code{"M"}.
#' @param spp A vector of character values argument specifying the species'.
#' @export
#' @examples
#' case_comp(fleets = 1:2, case = 30, spp = "cod",
#'   Nsamp = list(rep(10, 40), rep(10, 25)),
#'   years = list(61:100, 76:100), cpar = 2:1, type = "agecomp")
case_comp <- function(fleets = 1, Nsamp = NULL, years = NULL, cpar = 2,
  type, case, spp) {

  old <- options()$"deparse.cutoff"
  options(deparse.cutoff = 500L)
  on.exit(options(deparse.cutoff = old))

  for (ind in seq_along(spp)){
    filename <- paste0(type, case, "-", spp[ind], ".txt")
    write(c("fleets;", case_deparse(fleets)), ncolumns = 2, filename)
    write(c("Nsamp;", case_deparse(Nsamp)), ncolumns = 2, filename, append = TRUE)
    write(c("years;", case_deparse(years)), ncolumns = 2, filename, append = TRUE)
    write(c("cpar;", case_deparse(cpar)), ncolumns = 2, filename, append = TRUE)
  }
}

#' Write time varying casefiles to the disk
#'
#' @param species A vector of species, for which a unique case file will be
#'   generated.
#' @param parameter A character value specifying the parameter to add deviates
#'   to. The argument must match the parameter name exactly.
#' @param perc_change A vector of percents, which will be used to add deviates
#'   to the parameter specified in \code{parameter}. A percentage must be supplied
#'   for every year in the model.
#' @param outfile A character value specifying the case letter and number used
#'   to save the file.
#' @param dir_out A character value specifying the directory to save the
#'   \code{outfile} to.
#' @param dir_models The path where the models are stored, such that
#'   \code{file.path(dir_models, species, "om", "ss3.ctl")} leads to valid
#'   \code{ss3.ctl} operating model files.
#' @param nyears The length time-series included in the model. The length of
#'   \code{perc_change} must equal \code{nyears}.
#' @param verbose Useful for debugging to print output to screen. Default is
#'   \code{FALSE}.
#'
#' @importFrom r4ss SS_parlines
#' @author Peter Kuriyama
#' @export
#'
#' @examples
#' case_tv(species = c("cod", "yellow"), parameter = "NatM_p_1_Fem_GP_1",
#'   perc_change = rep(0.5, 100), outfile = "G1", nyears = 100,
#'   dir_out = getwd(), verbose = TRUE)
case_tv <- function(species, parameter, perc_change, outfile,
  dir_out = "cases", dir_models = system.file("models", package = "ss3models"),
  nyears = 100, verbose = FALSE) {

  if (! file.exists(dir_out)) {
    stop(paste("The directory", dir_out, "does not exist."))
  }
  if (!any(species %in% dir(system.file("models", package = "ss3models")))) {
    stop(paste("One of the species does not exist as specified in the folder",
      system.file("models", package = "ss3models")))
  }
  if (!grepl("[0-9]$", outfile)) {
    stop(paste("The outfile must end in a numeric character."))
  }

  #Modify by percentage
  ctl <- file.path(dir_models, species, "om", "ss3.ctl")
  pars <- lapply(ctl, SS_parlines)
  val <- lapply(pars, function(x) x[grep(parameter, x$Label), "INIT"])

  if (length(perc_change) != nyears) {
    stop(paste("perc_change must have length of", nyears))
  }

  if (verbose) {
  message(paste("OM parameter value(s) are:",
    paste(paste(species, unlist(val)), collapse = ", ")))
  }

  #Create vector of deviates
  dev <- lapply(val, function(x) x * perc_change)

  #Prep output for file
  dev <- lapply(dev, paste0, collapse = ", ")
  header <- c("dev; c(")
  for (ind in seq_along(species)) {
    out <- paste0(header, dev[[ind]], ")", collapse = "")
    towrite <- c("function_type; change_tv", paste0("param; ", parameter), out)
  writeLines(towrite,
    con = file.path(dir_out, paste0(outfile, "-", species[ind], ".txt")))
  }
}

#' Turn an argument describing an object into a character.
#' @details Includes checks to make sure multiple lines will not be created.
#' @param x The argument you would like to \code{deparse}.
#'   \code{"M1-F1-D1-R1"}
#' @return A single character value.
case_deparse <- function(x) {
  temp <- deparse(x)
  if (length(temp) > 1) {
    temp <- paste(temp, collapse = "")
    temp <- gsub(" ", "", temp)
    temp <- gsub("\"", "", deparse(temp))
    if (length(temp) > 1) {
      stop(paste("Number of characters of", x, "was greater than",
        "500, which is not allowed in the case writing functions",
        "for ss3sim."))
    } else return(temp)
  }
  return(temp)
}

