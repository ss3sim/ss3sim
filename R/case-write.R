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
#'   Argument can be a vector (e.g., \code{c("agecomp", "lcomp")}) if you want
#'   the case files to be the same for length and age compositions.
#' @param case The case you want to write to. E.g. \code{"M"}.
#' @param spp A vector of character values argument specifying the species.
#' @export
#' @examples
#' case_comp(fleets = 1:2, case = 30, spp = "cod",
#'   Nsamp = list(rep(10, 40), rep(10, 25)),
#'   years = list(61:100, 76:100), cpar = 2:1, type = "agecomp")
#' done <- file.remove("agecomp30-cod.txt")
case_comp <- function(fleets = 1, Nsamp = NULL, years = NULL, cpar = 2,
  type, case, spp) {

  old <- options()$"deparse.cutoff"
  options(deparse.cutoff = 500L)
  on.exit(options(deparse.cutoff = old))

  for (ind in seq_along(spp)){
    filename <- paste0(type, case, "-", spp[ind], ".txt")
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2,
      x = c("fleets;", case_deparse(fleets))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
      x = c("Nsamp;", case_deparse(Nsamp))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
      x = c("years;", case_deparse(years))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
      x = c("cpar;", case_deparse(cpar))))
  }
}

#' Write a case file for index data to the disk.
#'
#' @param fleets Vector of fleet numbers, where the order of
#'   \code{fleets} will dictate the order of all remaining arguments.
#' @param years A list containing vectors of years for each year for each fleet.
#' @param sd A vector of standard deviations for each fleet.
#' @param case The case you want to write to. E.g. \code{'I'}.
#' @param spp A vector of character values argument specifying the species.
#' @export
#' @examples
#' case_index(fleets = 2, case = 1, spp = "cod", years = list(7:10), sd = 0.1)
#' done <- file.remove("index1-cod.txt")

case_index <- function(fleets = 1, years = NULL, sd = 2, case, spp) {

  old <- options()$"deparse.cutoff"
  options(deparse.cutoff = 500L)
  on.exit(options(deparse.cutoff = old))

  for (ind in seq_along(spp)){
    filename <- paste0("index", case, "-", spp[ind], ".txt")
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2,
      x = c("fleets;", case_deparse(fleets))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
      x = c("years;", case_deparse(years))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
      x = c("sds_obs;", case_deparse(sd))))
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
#' @examples
#' temp_path <- file.path(tempdir(), "cod")
#' dir.create(temp_path, showWarnings = FALSE)
#'
#' d <- system.file("extdata", package = "ss3sim")
#'
#' om <- file.path(d, "models", "cod-om")
#' ig <- file.copy(om, temp_path, recursive = TRUE)
#' ig <- file.rename(file.path(temp_path, "cod-om"), file.path(temp_path, "om"))
#' verify_input(file.path(temp_path, "om"), type = "om")
#' ig <- file.rename(file.path(temp_path, "om", "om.ctl"),
#'   file.path(temp_path, "om", "ss3.ctl"))
#' case_tv(species = "cod", parameter = "NatM_p_1_Fem_GP_1",
#' perc_change = rep(0.5, 100), outfile = "G1",
#' dir_out = temp_path, dir_models = gsub("/cod", "", temp_path),
#' nyears = 100, verbose = TRUE)
#' unlink(temp_path, recursive = TRUE)
case_tv <- function(species, parameter, perc_change, outfile,
  dir_out = "cases", dir_models = system.file("models", package = "ss3models"),
  nyears = 100, verbose = FALSE) {

  if (! file.exists(dir_out)) {
    stop(paste("The directory", dir_out, "does not exist."))
  }
  if (!any(species %in% dir(dir_models))) {
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
  temp <- deparse(x, control = c("keepInteger", "keepNA"))
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

