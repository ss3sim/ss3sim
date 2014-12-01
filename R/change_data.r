#' Change the data that is available as output from an SS operating model.
#'
#' \code{change_data} alters the bin structure for the age or length composition
#' data in an SS operating model. Original data is removed and dummy data is
#' added at the appropriate bin sizes to the SS \code{.dat} file. This causes SS
#' to record age or length composition data in the appropriate bins when the
#' operating model is run. Additionally, \code{change_data} will introduce dummy
#' conditional length-at-age or size- (weight or length) at-age data to the
#' \code{.dat} file. For each data type altered, \code{change_data} will add
#' data in a full factorial manner, working with existing fleets for all years;
#' potentially adding many rows of data. Currently, \code{.dat} files with
#' multiple genders cannot be manipulated with \code{change_data}. Use
#' \code{\link{sample_lcomp}}, \code{\link{sample_agecomp}}, and
#' \code{\link{sample_calcomp}} to reduce the data. It is not intended for an
#' \pkg{ss3sim} user to use \code{change_data} directly. Instead
#' \code{change_data} is called internally by \code{\link{ss3sim_base}} based on
#' arguments to the sampling functions.
#'
#' @param file_in A character value giving the location of an SS \code{.dat}
#'   file to input.
#' @param file_out A character value giving the location of an SS \code{.dat}
#'   file to output.
#' @param fleets A numeric vector of fleets
#' @param years A numeric vector of years
# TODO: as coded, types only does anything if len or age:
#' @param types A vector that can take the one or all of the following entries:
#'   \code{"len"}, \code{"age"}, \code{"cal"}, \code{"mla"}.
#'   \code{types} controls what data structures the function acts on, with
#'   \code{"len"} augmenting the length composition data, \code{"age"}
#'   augmenting the age composition, \code{"cal"} augmenting the conditional age
#'   at length, and \code{"mla"} augmenting the mean length at age data.
#' @param age_bins A numeric vector of age bins to use. If left as
#'   \code{NULL} then the age bin structure will be taken from the OM.
#' @param len_bins A numeric vector of length bins to use. If left as
#'   \code{NULL} then the length bin structure will be taken from the OM.
#' @param write_file Should the \code{.dat} file be written? The new \code{.dat}
#'   file will always be returned invisibly by the function. Setting
#'   \code{write_file = FALSE} can be useful for testing. Note that you must
#'   supply a value to the argument \code{file_out}, but this argument can be
#'   set to any arbitrary value (such as \code{NULL}) if \code{write_file =
#'   FALSE}.
#'
# # TODO: do we need any of this? - SA
# @details Within the \code{.dat} file, the conditional age-at-length data is
#   stored in the same matrix as the age composition data. Thus, it is
#   necessary that the conditional age-at-length data use the same binning
#   structure as the age composition data. If \code{types = "caa"} and not
#   \code{types = c("age", "caa")} the function will add conditional
#   age-at-length using the binning structure of the current \code{.dat} file.
#   Also note that if \code{types = c("mla", "mwa")} no entries are currently
#   necessary in the \code{len_bins}.
#'
#' @importFrom r4ss SS_readdat SS_writedat
#' @export
#' @seealso \code{\link{sample_lcomp}}, \code{\link{sample_agecomp}}
#' @author Cole Monnahan, Ian Taylor, Sean Anderson, Kelli Johnson
#' @examples
#' ## These examples are in development still and untested
#' d <- system.file("extdata", package = "ss3sim")
#' fleets <- 1:2
#' years <- c(5,10,15)
#' types <- c("len", "age")
#' file_in <- paste0(d, "/models/cod-om/codOM.dat")
#' ## Basic test with just length data, default bins
#' out <- change_data(file_in,"test2.dat", types="len", years=years,
#'                    fleets=fleets, write_file=FALSE)
#' print(out$lbin_vector)
#' print(tail(out$lencomp[, 1:8]))
#' ## Change the length bins
#' out <- change_data(file_in,"test.dat", types="len", years=years,
#'                    fleets=fleets, len_bins=3:6, write_file=FALSE)
#' print(out$lbin_vector)
#' print(tail(out$lencomp[, 1:8]))
#' ## Change data types
#' out <- change_data(file_in,"test.dat", types=c("len", "age"), years=years,
#'                    fleets=fleets, len_bins=3:6, age_bins=5:7, write_file=FALSE)
#' print(out$agebin_vector)
#' print(out$lbin_vector)
#' print(names(out$agecomp))
#' print(names(out$lencomp))
change_data <- function(file_in, file_out, fleets = NULL, years = NULL,
                        types = NULL, age_bins = NULL, len_bins = NULL,
                        write_file = TRUE) {

  types <- match.arg(types, choices = c("len", "age", "cal", "mla", "mwa"),
    several.ok = TRUE)

  ## Input checks:

  ## Check .dat file and read it in
  if (!file.exists(file_in)) {
    stop(paste(file_in, "was not found while running change_data"))
  }
  datfile <- SS_readdat(file = file_in, verbose = FALSE)

  ## Test for compatibility with ss3sim
  if (datfile$Ngenders > 1) {
    stop(paste("_Ngenders is greater than 1 in the operating model.",
      "change_data only works with single-gender models."))
  }

  ## Change the data vectors if specified
  if(is.null(len_bins)) len_bins <- datfile$lbin_vector
  if(is.null(age_bins)) age_bins <- datfile$agebin_vector

  ## Now modify each data type in turn
  if ("len" %in% types) {
      datfile$lencomp <-
           make_dummy_dat_lencomp(fleets=fleets, years=years,
                                  len_bins=len_bins)
      datfile$lbin_vector <- len_bins
      datfile$N_lencomp <- nrow(datfile$lencomp)
  }
  if ("age" %in% types) {
      datfile$agecomp <-
           make_dummy_dat_agecomp(fleets=fleets, years=years,
                                  age_bins=age_bins)
      datfile$agebin_vector <- age_bins
      datfile$N_agecomp <- nrow(datfile$agecomp)
  }
  if ("mla" %in% types) {
      datfile$MeanSize_at_Age_obs <- make_dummy_dat_agecomp(fleets = fleets,
        years = years, age_bins = age_bins)
      datfile$MeanSize_at_Age_obs$AgeErr <- -1
      datfile$N_MeanSize_at_Age_obs <- nrow(datfile$MeanSize_at_Age_obs)
  }

  if (write_file) {
    SS_writedat(datlist = datfile, outfile = file_out, overwrite = TRUE,
      verbose = FALSE)
  }
  invisible(datfile)
}

make_dummy_dat_lencomp <- function(fleets, years, len_bins) {
    ## expand dummy data across all types:
    dummy_dat_list <- lapply(fleets, function(fleet) {
        data.frame("Yr"   = years, "Seas" = 1, "Flt"  = fleets[fleet],
                   "Gender" = 0, "Part"   = 0, "Nsamp" = 0,
                   stringsAsFactors = FALSE)})
    dummy_dat <- as.data.frame(do.call('rbind', dummy_dat_list))
    ## Add the dummy data for each data cell
    dummy_df <- data.frame(matrix(1, nrow=nrow(dummy_dat), ncol=length(len_bins)))
    names(dummy_df) <- paste0("l", len_bins)
    cbind(dummy_dat, dummy_df)
}

make_dummy_dat_agecomp<- function(fleets, years, age_bins) {
    ## expand dummy data across all types:
    dummy_dat_list <- lapply(fleets, function(fleet)
        data.frame("Yr"   = years, "Seas" = 1, "Flt"  = fleets[fleet], "Gender" = 0,
                   "Part"   = 0, "AgeErr"=0, "Lbin_lo"=-1, "Lbin_hi"=-1,
                   "Nsamp" = 0, stringsAsFactors = FALSE))
    dummy_dat <- as.data.frame(do.call('rbind', dummy_dat_list))
    ## Add the dummy data for each data cell
    dummy_df <- data.frame(matrix(1, nrow=nrow(dummy_dat), ncol=length(age_bins)))
    names(dummy_df) <- paste0("a", age_bins)
    cbind(dummy_dat, dummy_df)
}


## Given sampling arguments, calculate super set of fleets, years, and data
## types. No documentation for now since not sure it'll be used.
## ### Examples
## ## Should throw error since nothing passed
## calculate_data_units()
## ## Only one fleet
## calculate_data_units(lcomp_params=list(fleets=1, years=c(3,4,6)))
## ## Add new fleet
## calculate_data_units(lcomp_params=list(fleets=1, years=c(3,4,6)),
##                      agecomp_params=list(fleets=2, years=5))
## ## If CAL data called, need other types even if not specified
## calculate_data_units(calcomp_params=list(fleets=1, years=c(3,4,6)))
calculate_data_units <- function(lcomp_params=NULL, agecomp_params=NULL,
                                calcomp_params=NULL, mlacomp_params=NULL){
    sample_args <- list("len"=lcomp_params, "age"=agecomp_params, "cal"=calcomp_params,
                        "mla"=mlacomp_params)
    sample_args_null <- vapply(sample_args, is.null, logical(1L))
    ## Exit if nothing specified to prevent error.
    if(!any(!sample_args_null)) stop("No data passed: all arguments NULL")
    ## Get the superset of fleets
    fleets <-
        as.vector(unlist(lapply(sample_args, function(x) x$fleets)))
    ## Get the superset of years
    years <-
        as.vector(unlist(lapply(sample_args, function(x) x$years)))
    ## Sort the unique values
    fleets <- sort(unique(fleets))
    years <- sort(unique(years))
    ## Now figure out which data types need to be in the OM for sampling (but
    ## not necessarily the EM). For now these are special cases but could be
    ## different based on different algorithms.
    types <- names(sample_args)[!sample_args_null]
    if("cal" %in% types) types <- c(types, "len", "age")
    if("mla" %in% types) types <- c(types, "age")
    return(list(fleets=fleets, years=years, types=types))
}

change_pop_bin <- function(){
    stop("This function is not yet developed")
  ## # TODO: look at making pop_bin of length two with the first argument
  ## # pertaining to the number of length population bins and the second argument
  ## # pertaining to the age population bins. The ageing error matrices must also
  ## # be changed because they have one column per population length bin
  ## if (length(pop_bin) != 1 & !is.null(pop_bin)) {
  ##   stop("pop bin should be a real number")
  ## }
  ## if (!is.null(pop_bin)) datfile$binwidth <- pop_bin
}
