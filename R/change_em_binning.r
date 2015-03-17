#' Change population and observed length composition bins in an SS estimation
#' model
#'
#' \code{change_em_binning} alters the bin structure for the population and
#' length composition data in an SS estimation model. It is done by taking the
#' original length composition info from the EM \code{ss3.dat} then changing
#' according to the user's specification. If the data file also contails
#' conditional age-at-length data then these data will be re-binned as well.
#'
#' @param datfile An SS3 data list object as read in by
#'   \code{\link[r4ss]{SS_readdat}}.
#' @param file_out A character value giving the location of an SS \code{ss3.dat}
#'   file to output.
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
#' @param write_file Should the \code{.dat} file be written? The new \code{.dat}
#'   file will always be returned invisibly by the function. Setting
#'   \code{write_file = FALSE} can be useful for testing. Note that you must
#'   supply a value to the argument \code{file_out}, but this argument can be
#'   set to any arbitrary value (such as \code{NULL}) if \code{write_file =
#'   FALSE}.
#' @importFrom r4ss SS_writedat
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by_ summarise_each_ rename_ mutate_ inner_join funs_
#' @export
#' @family sample functions
#' @family change functions
#' @author Kotaro Ono (length-composition rebinning), Sean Anderson
#'   (conditional age-at-length rebinning)
#' @examples
#' d <- system.file("extdata", package = "ss3sim")
#' f_in <- paste0(d, "/example-om/data.ss_new")
#' datfile <- r4ss::SS_readdat(file = f_in, verbose = FALSE)
#' l <- change_em_binning(datfile, file_out = NULL, lbin_method = 1,
#'   bin_vector = seq(8, 30, by = 1), write_file = FALSE)
#' print(l$lbin_vector)
#' print(head(l$lencomp))
#'
#' # An small example with conditional age-at-length re-binning:
#' f <- system.file("extdata", "models", "cod-om", "codOM.dat", package = "ss3sim")
#' d <- r4ss::SS_readdat(f, verbose = FALSE)
#'
#' # Add catch at length data (and simplify the bin structure for this example)
#' olddat <- change_data(d, outfile = NULL, write_file = FALSE,
#'   types = c("len", "age", "cal"), fleets = 1, years = seq(2000, 2002),
#'   age_bins = 1:3, len_bins = 4:8)
#' olddat$agecomp
#' newdat <- change_em_binning(olddat, file_out = NULL, bin_vector = c(4, 6, 8),
#'   lbin_method = 1, write_file = FALSE)
#' newdat$agecomp
#'
#' # A larger conditional age-at-length re-rebinning example:
#' olddat <- change_data(d, outfile = NULL, write_file = FALSE,
#'  types = c("len", "age", "cal"), fleets = 1, years = seq(2000, 2005),
#'  age_bins = seq(1, 5), len_bins = round(seq(20, 30, length.out = 13), 1))
#'
#' olddat$lbin_vector
#' head(olddat$lencomp)
#' head(olddat$agecomp)
#' newdat <- change_em_binning(olddat, file_out = NULL, bin_vector = seq(20, 30, 2),
#'  lbin_method = 1, write_file = FALSE)
#' newdat$lbin_vector
#' head(newdat$lencomp)
#' newdat$agecomp

change_em_binning <- function(datfile, file_out, bin_vector, lbin_method = NULL,
                              pop_binwidth=NULL, pop_minimum_size=NULL,
                              pop_maximum_size=NULL, write_file = TRUE) {

  ## If lbin_method is NULL then don't do anything
  if (is.null(lbin_method)) return(NULL)

  # error checking
  if (!is.numeric(bin_vector)) {
    stop("bin_vector must be numeric")
  }
  if (length(bin_vector) > length(datfile$lbin_vector)) {
    stop(paste("The specified bin_vector is longer than the original",
      "lbin_vector in the SS3 data file and therefore can't be re-binned."))
  }
  if (length(bin_vector) == 1) {
    warning(paste("length(bin_vector) == 1; are you sure you",
      "input a full numeric vector of bins and not a bin size?"))
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
  if (is.null(datfile$lencomp)) {
    stop("no lcomp data. Verify your case argument files")
  }
  if (datfile$Ngenders > 1) {
    stop(paste("_Ngenders is greater than 1 in the model.",
      "change_em_binning only works with single-gender models."))
  }
  if (!identical(as.integer(max(bin_vector)), as.integer(max(datfile$lbin_vector)))) {
    stop(paste("The maximum value in the bin_vector is not equal to the",
      "original maximum length bin value."))
  }
  if(!identical(as.integer(min(bin_vector)), as.integer(min(datfile$lbin_vector)))) {
    stop(paste("The minimum value in the bin_vector is not equal to the",
      "original maximum length bin value."))
  }
  if (any(!is_divisible(bin_vector, by_ = datfile$binwidth)) ) {
    stop(paste("One or more of the values in bin_vector are not divisible by",
      "the population binwidth specified in the SS3 data file."))
  }

  # Find ID columns and data columns to replace:
  old_len_columns <- grep("^l[0-9.]+$", names(datfile$lencomp))
  old_lcomp_total <- sum(datfile$lencomp[, old_len_columns]) # check later
  id_columns <- seq_along(names(datfile$lencomp))[-old_len_columns]
  newdummy <- datfile$lencomp[, old_len_columns]
  old_binvector <- datfile$lbin_vector

  # change population length bin width
  lcomp_new <- as.data.frame(matrix(0, nrow = nrow(newdummy),
    ncol = length(bin_vector)))
  names(lcomp_new) <- paste0("l", bin_vector)

  # Re-bin length comps:
  for (i in 1:length(bin_vector)) {

    if (i == 1) {
      select_col <- which(datfile$lbin_vector < bin_vector[i + 1])
      if (length(select_col) > 1) {
        lcomp_new[, i] <- apply(newdummy[, select_col], 1, sum, na.rm = TRUE)
      }
      if (length(select_col) == 1)
        lcomp_new[, i] = newdummy[, select_col]
    }

    if (i > 1 & i < length(bin_vector)) {
      select_col <- which(datfile$lbin_vector >= bin_vector[i] &
          datfile$lbin_vector < bin_vector[i + 1])
      if (length(select_col) > 1) {
        lcomp_new[, i] <- apply(newdummy[, select_col], 1, sum, na.rm = TRUE)
      }
      if (length(select_col) == 1)
        lcomp_new[, i] = newdummy[, select_col]
    }

    if (i == length(bin_vector)) {
      select_col <- which(datfile$lbin_vector >= bin_vector[i])
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
    stop(paste("Number of samples in the new lcomp data matrix does not match",
      "the number of samples in the original dataset."))
  }

  # Substitute new bins:
  datfile$lencomp <- data.frame(datfile$lencomp[, id_columns], lcomp_new)
  datfile$lbin_vector <- bin_vector
  datfile$N_lbins <- length(datfile$lbin_vector)

  # change the lbin_method, if it's NULL leave it as is
  if (!is.null(lbin_method)) {
    ## otherwise if 1 it will use the data bins and requires no more
    ## input
    if (lbin_method == 1) {
      datfile$lbin_method <- lbin_method
      datfile$binwidth <- NULL
      datfile$minimum_size <- NULL
      datfile$maximum_size <- NULL
    } else {
      ## it is 2 so we  need to specify width, min and max
      datfile <- change_pop_bin(datfile, binwidth = pop_binwidth,
        minimum_size = pop_minimum_size, maximum_size = pop_maximum_size)
    }
  }

  # Re-bin conditional age-at-length comps:
  # if all Lbin_lo == -1 then there aren't any CAL data:
  if (length(unique(datfile$agecomp$Lbin_lo)) > 1) {
    if (!identical(datfile$Lbin_method, 3)) {
      stop(paste("Lbin_method was not set to 3 in the SS3 data file.",
        "change_em_binning() requires the data file to specify conditional",
        "age-at-length data with Lbin_method == 3. See the SS3 manual. Note",
        "the capital L in Lbin_method."))
    }

    if (datfile$lbin_method == 2) {
      population_bins <- seq(datfile$minimum_size, datfile$maximum_size,
        by = datfile$binwidth)
      if (!all(bin_vector %in% population_bins)) {
        stop(paste("One or more of bin_vector is not contained in the",
                   "population bins (and lbin_method = 2). This is required in",
                   "SS for conditional age-at-length composition data."))
      }
    }

    # to check later:
    a_ids <- grep("^a[0-9.]+$", names(datfile$agecomp))
    old_age_dat <- datfile$agecomp[, a_ids]
    old_agecomp_total <- sum(old_age_dat)

    # grab the data we'll work with:
    old_age <- datfile$agecomp[datfile$agecomp$Lbin_lo == -1, ]
    old_cal_all <- datfile$agecomp[datfile$agecomp$Lbin_lo != -1, ]

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
    # this uses dplyr and pipes but avoids non-standard evaluation
    # http://cran.r-project.org/web/packages/dplyr/vignettes/nse.html
    new_cal <- inner_join(old_cal, lookup, by = "Lbin_lo") %>%
      group_by_(~Yr, ~lbin_new_low, ~lbin_new_high) %>%
      summarise_each_(funs_(~sum), ~matches("^a[0-9.]+$")) %>%
      rename_(Lbin_lo = ~lbin_new_low, Lbin_hi = ~lbin_new_high) %>%
      as.data.frame

    new_cal$Nsamp <- rowSums(new_cal[, grepl("^a[0-9.]+$", names(new_cal))])

    new_cal_meta_dat <- old_cal_all[1:nrow(new_cal),
      -which(names(old_cal_all) %in%
        c("Yr", "Lbin_lo", a_ids_character, "Lbin_hi", "Nsamp"))]
    new_cal <- cbind(new_cal_meta_dat, new_cal)

    # re-order the columns:
    new_cal <- new_cal[, match(names(old_cal_all), names(new_cal))]

    # and slot the new data into the .dat file:
    datfile$agecomp <- rbind(old_age, new_cal)
    datfile$N_agecomp <- nrow(datfile$agecomp)

    new_age_dat <- datfile$agecomp[, grepl("^a[0-9.]+$", names(datfile$agecomp))]
    new_agecomp_total <- sum(new_age_dat)
    if (!identical(old_agecomp_total, new_agecomp_total)) {
      stop(paste("Number of samples in the new agecomp data matrix does not match",
        "the number of samples in the original dataset."))
    }
  }

  if (write_file) {
    SS_writedat(datlist = datfile, outfile = file_out, overwrite = TRUE,
      verbose = FALSE)
  }
  invisible(datfile)
}

is_divisible <- function(x, by_ = 2L) {
  x %% by_ == 0
}
