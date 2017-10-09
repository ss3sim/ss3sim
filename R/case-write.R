#' Write a case file for length- or age-composition data
#'
#' Use \R code to write arguments to the disk, which
#' will later be used in a \pkg{ss3sim} simulation.
#'
#' @param fleets Vector of fleet numbers, where the order of
#'   \code{fleets} will dictate the order of all remaining arguments.
#' @param Nsamp A list of length \code{length(fleets)},
#'   where each element of the list contains a vector of
#'   sample sizes for each year for that given fleet.
#' @param years A list of length \code{length(fleets)},
#'   where each element of the list contains a vector of
#'   years for the given fleet.
#' @param cpar A vector of cpar for each fleet.
#' @param type A character value of \code{"agecomp"} or \code{"lcomp"},
#'   to write age- or length-composition specifications, respectively.
#'   Argument can be a vector (e.g., \code{c("agecomp", "lcomp")}) if you want
#'   the case files to be the same for length and age compositions.
#' @param case The casenumber you want to write to.
#'   If \code{case = 1} and \code{type = "agecomp"},
#'   then the result will be \code{'agecomp1'}.
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

#' Write a case file for length- data with my new arguments
#'
#' Use \R code to write arguments to the disk, which
#' will later be used in a \pkg{ss3sim} simulation.
#'
#' @param fleets Vector of fleet numbers, where the order of
#'   \code{fleets} will dictate the order of all remaining arguments.
#' @param Nsamp A list of length \code{length(fleets)},
#'   where each element of the list contains a vector of
#'   sample sizes for each year for that given fleet.
#' @param years A list of length \code{length(fleets)},
#'   where each element of the list contains a vector of
#'   years for the given fleet.
#' @param cpar A vector of cpar for each fleet.
#' @param Nhauls TO fill in
#' @param cl_factor TO fill in
#' @param vals_at_cutoff TO fill in
#' @param empirical_aggs To fill in
#' @param plot.schools TO fill in
#' @param parallel_lgths TO fill in
#' @param ESS TO fill in
#' @param Nsamp_max if randomly generated numbers
#' @param random_gen if randomly generated numbers
#' @param change_ess_now now or later (at the end of sampling in ss3sim_base)
#' @param type A character value of \code{"agecomp"} or \code{"lcomp"},
#'   to write age- or length-composition specifications, respectively.
#'   Argument can be a vector (e.g., \code{c("agecomp", "lcomp")}) if you want
#'   the case files to be the same for length and age compositions.
#' @param case The casenumber you want to write to.
#'   If \code{case = 1} and \code{type = "agecomp"},
#'   then the result will be \code{'agecomp1'}.
#' @param spp A vector of character values argument specifying the species.
#' @export
#' @examples
#' case_comp_length(fleets = 1:2, case = 30, spp = "cod",
#'   Nsamp = list(rep(10, 40), rep(10, 25)),
#'   years = list(61:100, 76:100), cpar = 2:1, type = "agecomp")
#' done <- file.remove("agecomp30-cod.txt")
case_comp_length <- function(fleets = 1, Nsamp = NULL, Nsamp_max=NULL, years = NULL, random_gen=NULL, cpar = NA,
                             Nhauls = NULL, cl_factor = NULL, vals_at_cutoff = NULL, empirical_aggs = NULL,
                             ESS = NULL, change_ess_now=NULL, plot.schools = NULL, parallel_lgths=NULL, 
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
                                                   x = c("Nsamp_max;", case_deparse(Nsamp_max))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
                                                   x = c("years;", case_deparse(years))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
                                                   x = c("random_gen;", case_deparse(random_gen))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
                                                   x = c("cpar;", case_deparse(cpar))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
                                                   x = c("Nhauls;", case_deparse(Nhauls))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
                                                   x = c("cl_factor;", case_deparse(cl_factor))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
                                                   x = c("vals_at_cutoff;", case_deparse(vals_at_cutoff))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
                                                   x = c("empirical_aggs;", case_deparse(empirical_aggs))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
                                                   x = c("ESS;", case_deparse(ESS))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
                                                   x = c("change_ess_now;", case_deparse(change_ess_now))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
                                                   x = c("plot.schools;", case_deparse(plot.schools))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
                                                   x = c("parallel_lgths;", case_deparse(parallel_lgths))))
  }
}

#' Write a case file for conditional length-at-age composition data
#'
#' Use \R code to write arguments to the disk, which
#' will later be used in a \pkg{ss3sim} simulation.
#'
#' @param fleets Vector of fleet numbers, where the order of
#'   \code{fleets} will dictate the order of all remaining arguments.
#' @param Nsamp A list of length \code{length(fleets)},
#'   where each element of the list contains a vector of
#'   sample sizes for each year for that given fleet.
#' @param years A list of length \code{length(fleets)},
#'   where each element of the list contains a vector of
#'   years for the given fleet.
#' @param fixed.number If the sampling should be length-stratified with
#' a fixed number of ages at length
#' @param method If it should be simple random sampling or length-stratified
#' @param Nhauls Number of hauls to sample age data from
#' @param ESS Change effective samplign size to number of hauls
#' @param percent_read collected vs read
#' @param max_ss max number read (use either this or percent_read)
#' @param random_gen if randomly generated numbers
#' @param change_ess_now now or later (at the end of sampling in ss3sim_base)
#' @param fit.on.agecomp Fit on marginal age composition instead of default
#' conditional age at length
#' @param rw Run iterative re-weighing (McAllister and Ianelli)
#' @param type A character value of \code{"agecomp"} or \code{"lcomp"},
#'   to write age- or length-composition specifications, respectively.
#'   Argument can be a vector (e.g., \code{c("agecomp", "lcomp")}) if you want
#'   the case files to be the same for length and age compositions.
#' @param case The casenumber you want to write to.
#'   If \code{case = 1} and \code{type = "agecomp"},
#'   then the result will be \code{'agecomp1'}.
#' @param spp A vector of character values argument specifying the species.
#' @export

case_comp_caal <- function(fleets = 1, Nsamp = NULL, years = NULL, random_gen=NULL, fixed.number = NULL,
                           method = NULL, Nhauls = NULL, ESS = NULL, change_ess_now=NULL,
                           fit.on.agecomp = NULL, rw=NULL, percent_read=100, max_ss=NULL,#add.MLA = NULL, 
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
                                                   x = c("random_gen;", case_deparse(random_gen))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
                                                   x = c("Nhauls;", case_deparse(Nhauls))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
                                                   x = c("method;", case_deparse(method))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
                                                   x = c("percent_read;", case_deparse(percent_read))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
                                                   x = c("max_ss;", case_deparse(max_ss))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
                                                   x = c("fit.on.agecomp;", case_deparse(fit.on.agecomp))))
  #  mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
  #                                                 x = c("add.MLA", case_deparse(add.MLA))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
                                                   x = c("ESS;", case_deparse(ESS))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
                                                   x = c("change_ess_now;", case_deparse(change_ess_now))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
                                                   x = c("fixed.number;", case_deparse(fixed.number))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
                                                   x = c("rw;", case_deparse(rw))))
  }
}

#' Write a case file for weight at age data to the disk.
#'
#' Use \R code to write arguments to the disk, which
#' will later be used in a \pkg{ss3sim} simulation.
#'
#' @param fleets Vector of fleet numbers, where the order of
#'   \code{fleets} will dictate the order of all remaining arguments.
#' @param years A list of length \code{length(fleets)},
#'   where each element of the list contains a vector of
#'   years for the given fleet.
#' @param cv_wtatage A coefficient of variation
#' @param case The case number you want to write to.
#'   If \code{case = 1}, then the result will be \code{'index1'}.
#' @param random_gen if numbers were randomly generated in case file
#' @param spp A vector of character values argument specifying the species.
#' @export

case_wtatage <- function(fleets = 1, years = NULL, cv_wtatage = 0.05, random_gen = NULL, case, spp) {
  
  old <- options()$"deparse.cutoff"
  options(deparse.cutoff = 500L)
  on.exit(options(deparse.cutoff = old))
  
  for (ind in seq_along(spp)){
    filename <- paste0("wtatage", case, "-", spp[ind], ".txt")
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2,
                                                   x = c("fleets;", case_deparse(fleets))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
                                                   x = c("years;", case_deparse(years))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
                                                   x = c("cv_wtatage;", case_deparse(cv_wtatage))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
                                                   x = c("random_gen;", case_deparse(random_gen))))
  }
}
#' Write a case file for mean length at age data to the disk.
#'
#' Use \R code to write arguments to the disk, which
#' will later be used in a \pkg{ss3sim} simulation.
#'
#' @param fleets Vector of fleet numbers, where the order of
#'   \code{fleets} will dictate the order of all remaining arguments.
#' @param years A list of length \code{length(fleets)},
#'   where each element of the list contains a vector of
#'   years for the given fleet.
#' @param Nsamp The number of fish to measure for length and age
#' @param ageplus The age-plus group, if different from the population max age, in case \code{sample_wtatage}
#' is used after
#' @param case The case number you want to write to.
#'   If \code{case = 1}, then the result will be \code{'index1'}.
#' @param spp A vector of character values argument specifying the species.
#' @export

case_mla <- function(fleets = 1, years = NULL, Nsamp = NULL, ageplus=NULL, case, spp) {

  old <- options()$"deparse.cutoff"
  options(deparse.cutoff = 500L)
  on.exit(options(deparse.cutoff = old))

  for (ind in seq_along(spp)){
    filename <- paste0("mlacomp", case, "-", spp[ind], ".txt")
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2,
                                                   x = c("fleets;", case_deparse(fleets))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
                                                   x = c("years;", case_deparse(years))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
                                                   x = c("Nsamp;", case_deparse(Nsamp))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
                                                   x = c("ageplus;", case_deparse(ageplus))))
  }
}

#' Write a case file for index data to the disk.
#'
#' Use \R code to write arguments to the disk, which
#' will later be used in a \pkg{ss3sim} simulation.
#'
#' @param fleets Vector of fleet numbers, where the order of
#'   \code{fleets} will dictate the order of all remaining arguments.
#' @param years A list of length \code{length(fleets)},
#'   where each element of the list contains a vector of
#'   years for the given fleet.
#' @param sd A list of standard deviations for each fleet.
#' @param case The case number you want to write to.
#'   If \code{case = 1}, then the result will be \code{'index1'}.
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



#' Write a case file for data to the disk.
#'
#' Use \R code to write arguments to the disk, which
#' will later be used in a \pkg{ss3sim} simulation.
#'
#' @param age_bins A numeric vector of age bins to use. If left as NULL 
#' then the age bin structure will be taken from the OM
#' @param len_bins A numeric vector of length bins to use. If left as NULL 
#' then the length bin structure will be taken from the OM
#' @param pop_binwidth Population length bin width. Note that this value must 
#' be smaller than the bin width specified in length composition data len_bins or 
#' SS3 will fail (see notes in the SS3 manual).
#' @param pop_minimum_size Population minimum length bin value.
#' @param pop_maximum_size Population maximum length bin value.
#' @param lcomp_constant A new robustification constant for length composition data 
#' to be used. Must be a numeric value, as a proportion. For example 0.1 means 10 percent. 
#' See the SS3 manual for further information. A NULL value indicates no action resulting 
#' in using the current value, and a value of 0 will throw an error since that leads to an 
#' error when zeroes exist in the data. Instead use a very small value like 1e-07.
#' @param tail_compression A new tail compression value to be used in SS3. Must be a numeric 
#' value, as a proportion. For example 0.1 means 10 percent. See the SS3 manual for further 
#' information. A NULL value indicates no action, a negative value indicates to SS3 to 
#' ignore it (not use that feature).
#' @param case The case number you want to write to.
#'   If \code{case = 1}, then the result will be \code{'index1'}.
#' @param spp A vector of character values argument specifying the species.
#' @export

case_data<- function(age_bins = NULL, len_bins = NULL, pop_binwidth =NULL,
                     pop_minimum_size = NULL, pop_maximum_size= NULL, 
                     lcomp_constant = NULL, tail_compression = NULL,
                     case, spp) {
  
  old <- options()$"deparse.cutoff"
  options(deparse.cutoff = 500L)
  on.exit(options(deparse.cutoff = old))
  
  for (ind in seq_along(spp)){
    filename <- paste0("data", case, "-", spp[ind], ".txt")
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, #append = TRUE,
                                                   x = c("age_bins;", case_deparse(age_bins))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
                                                   x = c("len_bins;", case_deparse(len_bins))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
                                                   x = c("pop_binwidth;", case_deparse(pop_binwidth))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
                                                   x = c("pop_minimum_size;", case_deparse(pop_minimum_size))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
                                                   x = c("pop_maximum_size;", case_deparse(pop_maximum_size))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
                                                   x = c("lcomp_constant;", case_deparse(lcomp_constant))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
                                                   x = c("tail_compression;", case_deparse(tail_compression))))
  }
}

#' Write a case file for binning to the disk.
#'
#' Use \R code to write arguments to the disk, which
#' will later be used in a \pkg{ss3sim} simulation.
#'
#' @param lbin_method xxxxxxx
#' @param pop_binwidth Population length bin width. Note that this value must 
#' be smaller than the bin width specified in length composition data len_bins or 
#' SS3 will fail (see notes in the SS3 manual).
#' @param pop_minimum_size Population minimum length bin value.
#' @param pop_maximum_size Population maximum length bin value.
#' @param bin_vector xxxx
#' @param case The case number you want to write to.
#'   If \code{case = 1}, then the result will be \code{'index1'}.
#' @param spp A vector of character values argument specifying the species.
#' @export

case_binning<- function(lbin_method = NULL, pop_binwidth =NULL,
                     pop_minimum_size = NULL, pop_maximum_size= NULL, 
                     bin_vector = NULL,
                     case, spp) {
  
  old <- options()$"deparse.cutoff"
  options(deparse.cutoff = 500L)
  on.exit(options(deparse.cutoff = old))
  
  for (ind in seq_along(spp)){
    filename <- paste0("em_binning", case, "-", spp[ind], ".txt")
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, #append = TRUE,
                                                   x = c("lbin_method;", case_deparse(lbin_method))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
                                                   x = c("pop_binwidth;", case_deparse(pop_binwidth))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
                                                   x = c("pop_minimum_size;", case_deparse(pop_minimum_size))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
                                                   x = c("pop_maximum_size;", case_deparse(pop_maximum_size))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
                                                   x = c("bin_vector;", case_deparse(bin_vector))))

  }
}



#' Write time varying casefiles to the disk
#'
#' Use \R code to write arguments to the disk, which
#' will later be used in a \pkg{ss3sim} simulation.
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

#' Write a case file for fishing data to the disk.
#'
#' Use \R code to write arguments to the disk, which
#' will later be used in a \pkg{ss3sim} simulation.
#'
#' @param years Vector of years for which \emph{F} values are specified,
#' if there is more than one fleet or season the catches must be ordered by
#' season:year:fishey (e.g., season1year1fishery1, season2year1fishery1,
#' season1year2fishery1). The actual vector does not have to correspond to
#' true years but must be the correct length (e.g., instead of
#' \code{2000:2004} you can use \code{1:5}). Use this argument to create
#' an index to old values. \code{years_alter} will use values in this vector.
#' For example, with two seasons and one fishery that operates for 4 years
#' you could use the following: \code{1:8}.
#' @param fleets vector of fleets for which to change F (only one in master ss3sim)
#' @param years_alter Vector of years for the which \emph{F} values will be altered.
#' If there is more than one fishery or season, use the mapping system
#' created in \code{years} because actual year values cannot be recycled.
#' For example, to change the second season of the second year in the
#' example above, use: \code{4}.
#' @param fvals Vector of \emph{F} values to be entered into \code{ss3.par} file,
#' where \code{length(fvals) == length(years_alter)} must be true.
#' @param case The case number you want to write to.
#'   If \code{case = 1}, then the result will be \code{'F1'}.
#' @param spp A vector of character values argument specifying the species.
#' @export
#' @examples
#' case_fishing(years=1:100,years_alter= 1:100,fvals= seq(0, 0.4, length.out = 100), fleets=1, 
#' case=2, spp="cod")
#' done <- file.remove("F2-cod.txt")
case_fishing <- function(years = 1, years_alter = NULL, fvals = 2, fleets = 1, case, spp) {

  old <- options()$"deparse.cutoff"
  options(deparse.cutoff = 500L)
  on.exit(options(deparse.cutoff = old))

  for (ind in seq_along(spp)){
    filename <- paste0("F", case, "-", spp[ind], ".txt")
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2,
      x = c("years;", case_deparse(years))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
      x = c("years_alter;", case_deparse(years_alter))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
      x = c("fvals;", case_deparse(fvals))))
    mapply(write, file = filename, MoreArgs = list(ncolumns = 2, append = TRUE,
                                                   x = c("fleets;", case_deparse(fleets))))
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
