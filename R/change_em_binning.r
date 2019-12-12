#' Change population and observed length composition bins in an SS estimation
#' model
#'
#' \code{change_em_binning} alters the bin structure for the population and
#' length composition data in an SS estimation model. It is done by taking the
#' original length composition info from the EM \code{ss3.dat} then changing
#' according to the user's specification. If the data file also contains
#' conditional age-at-length data then these data will be re-binned as well.
#'
#' @template dat_list
#' @template outfile
#' @param bin_vector A numeric vector of new length bins to substitute into the
#'   \code{ss3.dat} file.
#' @param lbin_method A numeric value of either \code{NULL, 1, 2, 3} to change
#'   the lbin_method for the population bin. Only supports either \code{NULL, 1,
#'   2} at the moment. \code{NULL} means to keep it unchanged.
#' @param pop_binwidth *Population length bin width. Only necessary for
#' \code{lbin_method=2}. Note that this value must be smaller than the bin
#' width specified in length composition data \code{len_bins} or SS3 will
#' fail (see notes in the SS3 manual).
#' @param pop_minimum_size *Population minimum length bin value. 'Only
#' necessary for \code{lbin_method=2}
#' @param pop_maximum_size *Population maximum length bin value. Only
#' necessary for \code{lbin_method=2}
#' @importFrom r4ss SS_writedat
#' @export
#' @family sample functions
#' @family change functions
#' @author Kotaro Ono (length-composition rebinning), Sean Anderson
#'   (conditional age-at-length rebinning)
#' @examples
#' # Note that typically this function is used with estimation models in ss3sim,
#' # but it is used with an operating model data file in the following examples.
#' f <- system.file("extdata", "models", "cod-om", "codOM.dat", package = "ss3sim")
#' d <- r4ss::SS_readdat(f, version = NULL, verbose = FALSE)
#'
#' # An example with lbin_method = 1
#' l1 <- change_em_binning(d, outfile = NULL, lbin_method = 1,
#'   bin_vector = seq(20, 152, by = 4))
#' l1$lbin_vector
#' head(l1$lencomp)
#'
#' #An example with lbin_method = 2
#' new_bin_vec <- seq(min(d$lbin_vector), max(d$lbin_vector), by = 4)
#' # add the max value if necessary.
#' if(new_bin_vec[length(new_bin_vec)] != d$lbin_vector[length(d$lbin_vector)]){
#'   new_bin_vec <- c(new_bin_vec,
#'                    d$lbin_vector[length(d$lbin_vector)])
#' }
#' pop_bin_input <- 5
#' pop_min_size_input <- min(d$lbin_vector_pop) - 1
#' pop_max_size_input <- max(d$lbin_vector_pop) + 5
#' lbin_vec_pop <-seq(pop_min_size_input,
#'                    pop_max_size_input,
#'                    length.out = (pop_max_size_input - pop_min_size_input)/
#'                      pop_bin_input + 1
#' )
#' l2 <- change_em_binning(dat_list = d,
#'                             bin_vector = new_bin_vec,
#'                             lbin_method = 2,
#'                             #Note: need more inputs with lbin_method = 2
#'                             pop_binwidth = pop_bin_input,
#'                             pop_minimum_size = pop_min_size_input,
#'                             pop_maximum_size = pop_max_size_input)
#' l2$lbin_method
#' # note bin width is now the same as the input
#' pop_bin_input
#' l2$binwidth
#' # note the minimum size has changed based on the input:
#' pop_min_size_input
#' l2$minimum_size
#' # so has max
#' l2$maximum_size
#' l2$lbin_vector
#' #other modified components:
#' l2$lbin_vector_pop
#' head(l2$lencomp)
change_em_binning <- function(dat_list, outfile = NULL, bin_vector, lbin_method = NULL,
                              pop_binwidth=NULL, pop_minimum_size=NULL,
                              pop_maximum_size=NULL) {
  ## If lbin_method is NULL then don't do anything
  if (is.null(lbin_method)){
    return(NULL)
  }
  # error checking
  if (!is.numeric(bin_vector)) {
    stop("bin_vector must be numeric")
  }

  if (length(bin_vector) > length(dat_list$lbin_vector)) {
    stop("The specified bin_vector is longer than the original ",
         "lbin_vector in the SS3 data file and therefore can't be re-binned.")
  }

  if (length(bin_vector) == 1) {
    warning("length(bin_vector) == 1; are you sure you input a full numeric ",
            "vector of bins and not a bin size?")
  }
  ## verify correct pop bin specification
  if (!is.null(lbin_method)) {
      ## If you implement method 3 need to alter code below
      if(lbin_method==1 & !all(is.null(pop_binwidth) & is.null(pop_minimum_size) & is.null(pop_maximum_size)))
          warning("lbin_method=1 so pop bin parameters ignored and data bins used")
      if(lbin_method==2 & any(is.null(pop_binwidth), is.null(pop_minimum_size), is.null(pop_maximum_size)))
          stop("you must specify all pop bin parameters if using lbin_method=2")
      if (lbin_method > 2)
          stop("lbin_method method should be either NULL, 1 or 2; 3 is not currently implemented")
  }
  if (is.null(dat_list$lencomp)) {
    stop("no lcomp data. Verify your case argument files")
  }
  if (dat_list$Ngenders > 1) {
    stop("_Ngenders is greater than 1 in the model.change_em_binning only ",
         "works with single-sex models.")
  }
  if (!identical(as.integer(max(bin_vector)), as.integer(max(dat_list$lbin_vector)))) {
    stop("The maximum value in the bin_vector is not equal to the original ",
         "maximum length bin value.")
  }
  if(!identical(as.integer(min(bin_vector)), as.integer(min(dat_list$lbin_vector)))) {
    stop("The minimum value in the bin_vector is not equal to the original ",
         "minimum length bin value.")
  }
  if (any(!is_divisible(bin_vector, by_ = dat_list$binwidth)) ) {
    stop("One or more of the values in bin_vector are not divisible by the ",
      "population binwidth specified in the SS3 data file.")
  }

  # Find ID columns and data columns to replace:
  old_len_columns <- grep("^l[0-9.]+$", names(dat_list$lencomp))
  old_lcomp_total <- sum(dat_list$lencomp[, old_len_columns]) # check later
  id_columns <- seq_along(names(dat_list$lencomp))[-old_len_columns]
  newdummy <- dat_list$lencomp[, old_len_columns]
  old_binvector <- dat_list$lbin_vector

  # change population length bin width
  lcomp_new <- as.data.frame(matrix(0, nrow = nrow(newdummy),
    ncol = length(bin_vector)))
  names(lcomp_new) <- paste0("l", bin_vector)

  # Re-bin length comps:
  for (i in seq_along(bin_vector)) {

    if (i == 1) {
      select_col <- which(dat_list$lbin_vector < bin_vector[i + 1])
      if (length(select_col) > 1) {
        lcomp_new[, i] <- apply(newdummy[, select_col], 1, sum, na.rm = TRUE)
      }
      if (length(select_col) == 1)
        lcomp_new[, i] <- newdummy[, select_col]
    }

    if (i > 1 & i < length(bin_vector)) {
      select_col <- which(dat_list$lbin_vector >= bin_vector[i] &
          dat_list$lbin_vector < bin_vector[i + 1])
      if (length(select_col) > 1) {
        lcomp_new[, i] <- apply(newdummy[, select_col], 1, sum, na.rm = TRUE)
      }
      if (length(select_col) == 1)
        lcomp_new[, i] <- newdummy[, select_col]
    }

    if (i == length(bin_vector)) {
      select_col <- which(dat_list$lbin_vector >= bin_vector[i])
      if (length(select_col) > 1) {
        lcomp_new[, i] <- apply(newdummy[, select_col], 1, sum, na.rm = TRUE)
      }
      if (length(select_col) == 1) {
        lcomp_new[, i] <- newdummy[, select_col]
      }
    }
  }

  new_lcomp_total <- sum(lcomp_new)
  if (!identical(old_lcomp_total, new_lcomp_total)) {
    #TODO: is this a necessary check? remove if not add test if so.
    stop("Number of samples in the new lcomp data matrix does not match the ",
         "number of samples in the original dataset.")
  }

  # Substitute new bins:
  dat_list$lencomp <- data.frame(dat_list$lencomp[, id_columns], lcomp_new)
  dat_list$lbin_vector <- bin_vector
  dat_list$N_lbins <- length(dat_list$lbin_vector)

  # change the lbin_method, if it's NULL leave it as is
  if (!is.null(lbin_method)) {
    ## otherwise if 1 it will use the data bins and requires no more
    ## input
    if (lbin_method == 1) {
      dat_list$lbin_method <- lbin_method
      dat_list$binwidth <- NULL
      dat_list$minimum_size <- NULL
      dat_list$maximum_size <- NULL
      dat_list$lbin_vector_pop <- dat_list$lbin_vector
    } else {
      ## it is 2 so we  need to specify width, min and max
      dat_list <- change_pop_bin(dat_list, binwidth = pop_binwidth,
                                 minimum_size = pop_minimum_size,
                                 maximum_size = pop_maximum_size)
    }
  }

  # Re-bin conditional age-at-length comps (not implemented)
  # if all Lbin_lo == -1 then there aren't any CAL data:
  if (length(unique(dat_list$agecomp$Lbin_lo)) > 1) {
    if (!identical(dat_list$Lbin_method, 3)) {
      stop("Lbin_method was not set to 3 in the SS3 data file. ",
           "change_em_binning() requires the data file to specify conditional ",
           "age-at-length data with Lbin_method == 3. See the SS3 manual. Note ",
           "the capital L in Lbin_method.")
      }

    if (dat_list$lbin_method == 2) {
      population_bins <- seq(dat_list$minimum_size, dat_list$maximum_size,
        by = dat_list$binwidth)
      if (!all(bin_vector %in% population_bins)) {
        stop("One or more of bin_vector is not contained in the ",
             "population bins (and lbin_method = 2). This is required in ",
             "SS for conditional age-at-length composition data.")
      }
    }

    # to check later:
    a_ids <- grep("^a[0-9.]+$", names(dat_list$agecomp))
    old_age_dat <- dat_list$agecomp[, a_ids]
    old_agecomp_total <- sum(old_age_dat)

    # grab the data we'll work with:
    old_age <- dat_list$agecomp[dat_list$agecomp$Lbin_lo == -1, ]
    old_cal_all <- dat_list$agecomp[dat_list$agecomp$Lbin_lo != -1, ]

    # remove columns we'll merge back in after:
    a_ids_character <- names(old_cal_all)[a_ids]
    old_cal <- old_cal_all[ , c("Yr", "Lbin_lo", a_ids_character)]

    # make a lookup table of old and new bins:
    lookup <- data.frame(
      Lbin_lo = old_binvector,
      lbin_new_low = bin_vector[findInterval(old_binvector, bin_vector)],
      lbin_new_high =
        c(bin_vector, -1)[findInterval(old_binvector,
          bin_vector)+1])

    # the re-binning happens here:
    new_cal <- merge(old_cal, lookup, by = "Lbin_lo", all = FALSE, sort = FALSE)
    dat_cols <- names(new_cal)[grep("^a[0-9.]+$", names(new_cal))]
    new_cal <- stats::aggregate(new_cal[,dat_cols],
      by = list(
        "Yr" = new_cal$Yr,
        "Lbin_lo" = new_cal$lbin_new_low,
        "Lbin_hi" = new_cal$lbin_new_high),
      sum)

    new_cal$Nsamp <- rowSums(new_cal[, grepl("^a[0-9.]+$", names(new_cal))])

    new_cal_meta_dat <- old_cal_all[seq_len(nrow(new_cal)),
      -which(names(old_cal_all) %in%
        c("Yr", "Lbin_lo", a_ids_character, "Lbin_hi", "Nsamp"))]
    new_cal <- cbind(new_cal_meta_dat, new_cal)

    # re-order the columns:
    new_cal <- new_cal[, match(names(old_cal_all), names(new_cal))]

    # and slot the new data into the .dat file:
    dat_list$agecomp   <- rbind(old_age, new_cal)
    dat_list$N_agecomp <- nrow(dat_list$agecomp)

    new_age_dat <- dat_list$agecomp[, grepl("^a[0-9.]+$", names(dat_list$agecomp))]
    new_agecomp_total <- sum(new_age_dat)
    if (!identical(old_agecomp_total, new_agecomp_total)) {
      stop("Number of samples in the new agecomp data matrix does not match",
           "the number of samples in the original dataset.")
    }
  }

  if (!is.null(outfile)) {
    SS_version <- get_ss_ver_dl(dat_list)
    SS_writedat(datlist = dat_list, outfile = outfile, overwrite = TRUE,
      version = SS_version, verbose = FALSE)
  }
  invisible(dat_list)
}

is_divisible <- function(x, by_ = 2L) {
  x %% by_ == 0
}
