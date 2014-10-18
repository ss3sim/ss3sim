#' Change the data that is available as output from an SS operating model.
#'
#' \code{change_bin} alters the bin structure for the age or length composition
#' data in an SS operating model.
#' Original data is removed and dummy data is added at the appropriate bin
#' sizes to the SS \code{.dat} file, causing SS to record age or length composition
#' data in the appropriate bins when the operating model is run.
#' Additionally the code will introduce dummy conditional length-at-age
#' or size- (weight or length) at-age data to the \code{.dat} file.
#' For each data type altered, \code{change_bin} will add data in a
#' full factorial manner, working with existing fleets, seasons,
#' population ages, and ageing error matrices;
#' potentially adding many rows of data.
#' Currently, \code{.dat} files with multiple genders cannot be manipulated
#' with \code{change_bin}.
#' Use \code{\link{sample_lcomp}}, \code{\link{sample_agecomp}},
#' and \code{\link{sample_ccomp}} to reduce the data.
#'
#' @param file_in A character value giving the location of an SS \code{.dat}
#'   file to input.
#' @param file_out A character value giving the location of an SS \code{.dat}
#'   file to output.
#' @param bin_vector A named list of vectors. Named elements provide
#'   the desired bin structure for each data \code{type},
#'   where the \code{type} is an additional argument.
#'   List should be named to map the appropriate vector to each \code{type}.
#'   If names are forgotten, list elements will be named according to the
#'   order of entries in \code{type}.
#'   Each vector of bins is substituted into the \code{.dat} file.
#' @param type A vector that can take the one or all of the following entries:
#'   \code{"len"}, \code{"age"}, \code{"cal"}, \code{"mla"}, \code{"mwa"}.
#'   \code{type} controls what data structures the function acts on,
#'   with \code{"len"} augmenting the length composition data,
#'   \code{"age"} augmenting the age composition, \code{"cal"} augmenting
#'   the conditional age at length, \code{"mla"} augmenting the mean length
#'   at age data, and \code{"mwa"} augmenting the mean weight at age data.
#'   Default is \code{"len"}.
#' @param pop_bin Choose a real number to choose the population bin width. This option
#'   only works if \code{"lbin_method"} is set to \code{"2"}. Default is
#'   \code{"NULL"} which leaves the original value.
#' @param write_file Should the \code{.dat} file be written? The new \code{.dat}
#'   file will always be returned invisibly by the function. Setting
#'   \code{write_file = FALSE} can be useful for testing. Note that you must
#'   supply a value to the argument \code{file_out}, but this argument can be
#'   set to any arbitrary value (such as \code{NULL}) if \code{write_file =
#'   FALSE}.
#'
#' @details Within the \code{.dat} file, the conditional age-at-length data
#'   is stored in the same matrix as the age composition data. Thus, it is
#'   necessary that the conditional age-at-length data use the same binning
#'   structure as the age composition data. If \code{type = "caa"} and not
#'   \code{type = c("age", "caa")} one will only add conditional age-at-length
#'   using the binning structure of the current \code{.dat} file.
#'   \code{change_bin} will only add conditional age-at-length with a binning
#'   width equal to 1 age bin (i.e. Lbin_lo == Lbin_hi).
#'   Also note that if \code{type = c("mla", "mwa")} no entries are currently
#'   necessary in the \code{bin_vector}.
#'
#' @importFrom r4ss SS_readdat SS_writedat
#' @export
#' @seealso \code{\link{sample_lcomp}}, \code{\link{sample_agecomp}}
#' @author Ian Taylor, Sean Anderson
<<<<<<< HEAD
# @examples
# d <- system.file("extdata", package = "ss3sim")
# f_in <- paste0(d, "/example-om/data.ss_new")
# l <- change_bin(f_in, file_out = NULL, type = "len",
#   bin_vector = list("len" = seq(2, 8, 2)), write_file = FALSE)
# print(l$lbin_vector)
# print(head(l$lencomp))
#
# a <- change_bin(f_in, file_out = NULL, type = "age",
#   bin_vector = list("age" = seq(2, 8, 2)), write_file = FALSE)
# print(a$agebin_vector)
# print(head(a$agecomp))
#
# e <- change_bin(f_in, file_out = NULL,
#   type = c("len", "age", "cal", "mla", "mwa"),
#   bin_vector = list("len" = seq(2, 8, 2), "age" = seq(2, 8, 2),
#     "cal" = 10:20), write_file = FALSE)
# print(a$agebin_vector)
# print(head(a$agecomp))
change_bin <- function(file_in, file_out, bin_vector,
  type = "len", pop_bin = NULL,
  write_file = TRUE) {

  type <- match.arg(type, choices = c("len", "age", "cal", "mla", "mwa"),
                    several.ok = TRUE)

    #add backward compatibility for when bin_vector is a vector and not list
  if(is.vector(bin_vector)) {
    bin_vector <- list(bin_vector)
  }
    if(is.null(names(bin_vector))) {
      names(bin_vector) <- type[seq(length(bin_vector))]
    }

  if(!any(sapply(bin_vector, is.numeric))) {
    stop("bin_vector must be numeric")
  }
  if(any(sapply(bin_vector, length) == 1)) {
    bad <- which(sapply(bin_vector, length) == 1)
    stop(paste("length(bin_vector[[", bad, "]]) == 1; are you sure you",
       "input a full numeric vector of bins and not a bin size?"))
  }
  #TODO: look at making pop_bin of length two with the first argument pertaining
  #to the number of length population bins and the second argument pertaining to
  #the age population bins.
  #The ageing error matrices must also be changed
  #because they have one column per population length bin
  if(length(pop_bin)!=1 & !is.null(pop_bin)) {
    stop("pop bin should be a real number")
  }

  datfile <- SS_readdat(file = file_in, verbose = FALSE)
  if(datfile$Ngenders > 1) {
    stop(paste("_Ngenders is greater than 1 in the operating model.",
        "change_bin only works with single-gender models."))
  }

  if(datfile$lbin_method != 2 & !is.null(pop_bin)) {
    stop(paste("the current model doesn't support a change in 'pop_bin' with a",
    "lbin_method different than option 2"))
  }

  if(!is.null(pop_bin)) datfile$binwidth <- pop_bin

  #TODO: the "len" and "age" types do not create factorial data
  #they only change the bins of the current data structure
  #benefits of this is you would not have to use sample functions
  #cons is if all the data is not in the OM you have no way to add
  #factorial length data
  if("len" %in% type) {
    datfile$lbin_vector <- bin_vector$len
    datfile$N_lbins <- length(datfile$lbin_vector)
    newdummy <- data.frame(matrix(1, nrow = nrow(datfile$lencomp),
      ncol = length(datfile$lbin_vector)))
    # Find ID columns and data columns to replace:
    names(newdummy) <- paste0("l", datfile$lbin_vector)
    old_len_columns <- grep("^l[0-9.]+$", names(datfile$lencomp))
    id_columns <- seq_along(names(datfile$lencomp))[-old_len_columns]
    # Substitute new bins:
    datfile$lencomp <- data.frame(datfile$lencomp[, id_columns], newdummy)
    # change population length bin width
    # (original file could have smaller value)
    # datfile$binwidth <- 1 #min(abs(diff(bin_vector)))
  }

  if("age" %in% type) {
    datfile$agebin_vector <- bin_vector$age
    datfile$N_agebins <- length(datfile$agebin_vector)
    newdummy <- data.frame(matrix(1, nrow = nrow(datfile$agecomp),
      ncol = length(datfile$agebin_vector)))
    # Find ID columns and data columns to replace:
    names(newdummy) <- paste0("a", datfile$agebin_vector)
    old_age_columns <- grep("^a[0-9.]+$", names(datfile$agecomp))
    id_columns <- seq_along(names(datfile$agecomp))[-old_age_columns]
    # Substitute new bins:
    datfile$agecomp <- data.frame(datfile$agecomp[, id_columns], newdummy)
  }

  if("cal" %in% type) {
    # Add conditional-age-at-length data to existing data file
    # get dimensions of years and fleets from marginal age data
    my.vector <- bin_vector$cal
    #generate the years to add data for
    #original code from IT used ageyrs <- sort(unique(datfile$agecomp$Yr))
    #new code implements data for every year of the model in case OM
    #does not currently have any age data
    ageyrs <- seq(datfile$styr, datfile$endyr, 1)
    seas <- seq_along(datfile$nseas)
    fleets <- seq(datfile$Nfleet + datfile$Nsurveys)
    ageerr <- seq_along(datfile$N_ageerror_definitions)
    CAALdata <- expand.grid(Yr = ageyrs, Seas = seas, FltSvy = fleets,
                            Gender = 0, Part = 0, Ageerr = ageerr,
                            Lbin_lo = my.vector)
    # copy Lbin_lo to Lbin_hi
    CAALdata$Lbin_hi <- CAALdata$Lbin_lo
    # add sample size, this value does not matter b/c we are taking the expected values
    # and resampling from them. The previous sentence was verified by KFJ.
    CAALdata$Nsamp <- 100
    # create dummy values for ages
    dummy <- data.frame(matrix(1, nrow = nrow(CAALdata),
                               ncol = length(datfile$agebin_vector)))
    names(dummy) <- paste0("a", datfile$agebin_vector)
    # add dummy values to the right of the first set of columns
    CAALdata <- cbind(CAALdata, dummy)
    # add new data frame below existing age data
    datfile$agecomp <- rbind(datfile$agecomp, CAALdata)
    datfile$N_agecomp <- nrow(datfile$agecomp)
  }

  if(any(c("mla", "mwa") %in% type)) {
    yrs <- seq(datfile$styr, datfile$endyr, 1)
    seas <- seq_along(datfile$nseas)
    fleets <- seq(datfile$Nfleet + datfile$Nsurveys)
    #if “AgeErr” is positive, then the observation is mean length-at-age.
    #if  “AgeErr”  is  negative,  then  the observation is mean bodywt-at-age
    #and the abs(AgeErr) is used as AgeErr.
    if("mla" %in% type){
      ageerr <- seq_along(datfile$N_ageerror_definitions)
    } else{
      ageerr <- seq_along(datfile$N_ageerror_definitions) * -1
    }
    dummy <- expand.grid("#_Yr" = yrs,
                         "Seas" = seas,
                         "Fleet" = fleets,
                         "Gender" = 0,
                         "Partition" = 0,
                         "AgeErr" = ageerr,
                         "Ignore" = 1000)
    orig.col <- dim(dummy)[2]
    dummy <- cbind(dummy,
                   data.frame(matrix(1, nrow = nrow(dummy),
                    ncol = length(datfile$agebin_vector) * 2)))
    colnames(dummy)[(orig.col + 1) : dim(dummy)[2]] <-
      c(paste0("f", datfile$agebin_vector),
        paste0("N_f", datfile$agebin_vector))
    if(all(c("mla", "mwa") %in% type)) {
      dummy.mwa <- dummy
      dummy.mwa$AgeErr <- dummy.mwa$AgeErr * -1
      dummy <- rbind(dummy, dummy.mwa)
    }
    datfile$MeanSize_at_Age_obs <- dummy
    datfile$N_MeanSize_at_Age_obs <- nrow(datfile$MeanSize_at_Age_obs)
  }

  if(write_file) {
    SS_writedat(datlist = datfile, outfile = file_out, overwrite=TRUE)
  }

  invisible(datfile)
}
