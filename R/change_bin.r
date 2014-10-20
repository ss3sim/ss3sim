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
#' full factorial manner, working with existing fleets for all years;
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
#'   the desired bin structure for each data \code{type}.
#'   If \code{type = "len"} or \code{type = "age"} \code{bin_vector} should
#'   be a vector of new bins.
#'   For other types in \code{bin_vector} the bins are determined from
#'   the current or specified length and age bins.
#'   If names are forgotten, list elements will be named according to the
#'   order of entries in \code{type}.
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
#'   \code{type = c("age", "caa")} the function will add conditional age-at-length
#'   using the binning structure of the current \code{.dat} file.
#'   Also note that if \code{type = c("mla", "mwa")} no entries are currently
#'   necessary in the \code{bin_vector}.
#'
#' @importFrom r4ss SS_readdat SS_writedat
#' @export
#' @seealso \code{\link{sample_lcomp}}, \code{\link{sample_agecomp}}
#' @author Ian Taylor, Sean Anderson, Kelli Johnson
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
# print(e$agebin_vector)
# print(tail(e$agecomp))
# print(head(e$MeanSize_at_Age_obs))

change_bin <- function(file_in, file_out, bin_vector = NULL,
  type = "len", pop_bin = NULL,
  write_file = TRUE) {
  type <- match.arg(type, choices = c("len", "age", "cal", "mla", "mwa"),
                    several.ok = TRUE)
  datfile <- SS_readdat(file = file_in, verbose = FALSE)

  ##Checks and balances
  #check .dat file
  if(!file.exists(file_in)) {
    stop(paste(file_in, "was not found while running change_e"))
  }
  if(datfile$Ngenders > 1) {
    stop(paste("_Ngenders is greater than 1 in the operating model.",
        "change_bin only works with single-gender models."))
  }
  if(datfile$lbin_method != 2 & !is.null(pop_bin)) {
    stop(paste("the current model doesn't support a change in 'pop_bin' with a",
    "lbin_method different than option 2"))
  }
  #Check for a vector for every type
  if(any(c("mla", "mwa") %in% type)) {
    for(i in grep("m", type)){
      if("age" %in% type) {
        bin_vector[[type[i]]] <- bin_vector[[which(type == "age")]]
      } else {bin_vector[[type[i]]] <- datfile$agebin_vector}
    }
  }
  if("cal" %in% type) {
    if("len" %in% type) {
      bin_vector[["cal"]] <- bin_vector[[which(type == "len")]]
    } else {bin_vector[["cal"]] <- datfile$lbin_vector}
  }
  #bin_vector names in same order as type, if its not named.
  if(is.null(names(bin_vector))) {
      names(bin_vector) <- type[seq(length(bin_vector))]
  }
  if(any(names(bin_vector) == "")){
    need <- which(names(bin_vector) == "")
    names(bin_vector)[need] <- type[need]
  }
  if("age" %in% type){
    if(rev(bin_vector$age)[1] > datfile$Nages){
    stop("bin_vector for ages in change_bin extends beyond population ages")
    }
  }
  if(!any(sapply(bin_vector, is.numeric))) stop("bin_vector must be numeric")

  ##Generate dummy data for all routines
  years <- seq(datfile$styr, datfile$endyr, 1)
  fleets <- seq(datfile$Nfleet + datfile$Nsurveys)
  dummy <- expand.grid("Year" = years, "Season" = 1,"Fleet" = fleets,
                       "Gender" = 0, "Part" = 0, "AgeErr" = 1)
  dummy.data <- lapply(bin_vector, function(x) {
    goodnames <- data.frame("types" = c("len", "age", "cal", "mla", "mwa"),
                            "create" = c("l", "a", "a", "f", "f"),
                            stringsAsFactors = FALSE)
    partname <- eval.parent(quote(names(X)))[substitute(x)[[3]]]
    newname <- goodnames[match(partname, goodnames$types), "create"]
    newdata <- data.frame(matrix(1, nrow = nrow(dummy), ncol = length(x)))
    names(newdata) <- paste0(newname, x)

    if(grepl("m", partname)) {
      newdata <- cbind(newdata,
                       setNames(newdata, gsub("f", "N_f", names(newdata))))
    }
    return(newdata)
  })

  #TODO: look at making pop_bin of length two with the first argument pertaining
  #to the number of length population bins and the second argument pertaining to
  #the age population bins.
  #The ageing error matrices must also be changed
  #because they have one column per population length bin
  if(length(pop_bin)!=1 & !is.null(pop_bin)) {
    stop("pop bin should be a real number")
  }
  if(!is.null(pop_bin)) datfile$binwidth <- pop_bin

  if("len" %in% type) {
    datfile$lbin_vector <- bin_vector$len
    datfile$N_lbins <- length(datfile$lbin_vector)
    datfile$lencomp <- cbind(dummy, dummy.data$len)
    datfile$N_lencomp <- nrow(datfile$lencomp)
  }

  if("age" %in% type) {
    datfile$agebin_vector <- bin_vector$age
    datfile$N_agebins <- length(datfile$agebin_vector)
    datfile$agecomp <- data.frame(dummy, "Lbin_lo" = -1, "Lbin_hi" = -1,
                                  "Nsamp" = 1, dummy.data$age)
    datfile$N_agecomp <- nrow(datfile$agecomp)
  }

  if("cal" %in% type) {
    binvals <- data.frame("Lbin_lo" = rep(seq_along(datfile$lbin_vector), each = nrow(dummy)),
                          "Lbin_hi" = rep(seq_along(datfile$lbin_vector), each = nrow(dummy)))
    caldummy <- do.call("rbind",
      replicate(length(datfile$lbin_vector), dummy, simplify = FALSE))
    dummy.data$cal <- data.frame(matrix(1, ncol = datfile$N_agebins,
                                        nrow = nrow(caldummy)))
    # add new data frame below existing age data
    datfile$agecomp <- rbind(datfile$agecomp,
      setNames(cbind(data.frame(caldummy, "Lbin_lo" = binvals$Lbin_lo,
        "Lbin_hi" = binvals$Lbin_hi, "Nsamp" = 1), dummy.data$cal),
        names(datfile$agecomp)))
    datfile$N_agecomp <- nrow(datfile$agecomp)
  }
  if(any(c("mla", "mwa") %in% type)) {
    mdummy <- data.frame(dummy, "Ignore" = 1000)
    mdummy$AgeErr <- ifelse("mla" %in% type, 1, -1)
    if("mla" %in% type){
      mdummy <- cbind(mdummy, dummy.data$mla)
    } else{mdummy <- cbind(mdummy, dummy.data$mwa)}
    if(all(c("mla", "mwa") %in% type)) {
      mdummy <- do.call("rbind", replicate(2, mdummy, simplify = FALSE))
      mdummy$AgeErr[(nrow(dummy) + 1):nrow(mdummy)] <- -1
    }
    datfile$MeanSize_at_Age_obs <- mdummy
    datfile$N_MeanSize_at_Age_obs <- nrow(datfile$MeanSize_at_Age_obs)
  }
  if(write_file) {
    SS_writedat(datlist = datfile, outfile = file_out, overwrite=TRUE,
                verbose = FALSE)
  }

  invisible(datfile)
}
