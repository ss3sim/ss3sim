#' Change population and observed length composition bins in an SS estimation
#' model
#'
#' \code{change_em_binning} alters the bin structure for the population and
#' length composition data in an SS estimation model. It is done by taking the
#' original length composition info from the EM \code{ss3.dat} then changing
#' according to the user's specification.
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
#' @param rebin_cal Logical: should conditional age-at-length data also be
#'   re-binned? The SS3 data file must contain conditional age-at-length data if
#'   this is set to \code{TRUE}.
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
#' l <- change_em_binning(datfile, file_out = NULL, lbin_method = 1,
#'   bin_vector = seq(10, 27, by = 2), write_file = FALSE)
#' print(l$lbin_vector)
#' print(head(l$lencomp))
#'
#' # An small example with conditional age-at-length re-binning:
#' f <- system.file("extdata", "models", "cod-om", "codOM.dat", package = "ss3sim")
#' d <- r4ss::SS_readdat(f)
#' # Add catch at length data (and simplify the bin structure for this example)
#' olddat <- change_data(d, outfile = NULL, write_file = FALSE,
#'   types = c("len", "age", "cal"), fleets = 1, years = seq(2000, 2002),
#'   age_bins = 1:3, len_bins = 4:8)
#' olddat$agecomp
#' newdat <- change_em_binning(olddat, file_out = NULL, bin_vector = c(4, 6, 8),
#'   lbin_method = 1, rebin_cal = TRUE, write_file = FALSE)
#' newdat$agecomp
#'
#' # A larger conditional age-at-length re-rebinning example:
#' olddat <- change_data(d, outfile = NULL, write_file = FALSE,
#'  types = c("len", "age", "cal"), fleets = 1, years = seq(2000, 2005),
#'  age_bins = seq(1, 5), len_bins = round(seq(20, 30, length.out = 13), 1))
#' olddat$lbin_vector
#' head(olddat$lencomp)
#' head(olddat$agecomp)
#' newdat <- change_em_binning(olddat, file_out = NULL, bin_vector = seq(20, 30, 2),
#'  lbin_method = 1, rebin_cal = TRUE, write_file = FALSE)
#' newdat$lbin_vector
#' head(newdat$lencomp)
#' newdat$agecomp

change_em_binning <- function(datfile, file_out, bin_vector, lbin_method = NULL,
  rebin_cal = FALSE, write_file = TRUE) {

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
  if (!is.null(lbin_method)) {
    if (lbin_method > 2)
      stop("lbin_method method should be either NULL, 1 or 2")
  }
  if (is.null(datfile$lencomp)) {
    stop("no lcomp data. Verify your case argument files")
  }
  if (datfile$Ngenders > 1) {
    stop(paste("_Ngenders is greater than 1 in the model.",
      "change_em_binning only works with single-gender models."))
  }
  if (max(bin_vector) > max(datfile$lbin_vector)) {
    stop(paste("the maximum value in the bin_vector is above the original one",
      "this column would be filled with zero observation so it is meaningless"))
  }
  if (min(bin_vector) < min(datfile$lbin_vector)) {
    stop(paste("the minimum value in the bin_vector is below the original one",
      "this column would be filled with zero observation so it is meaningless"))
  }
  if (any(!is_divisible(bin_vector, by_ = datfile$binwidth)) ) {
    stop(paste("One or more of the values in bin_vector are not divisible by",
      "the population binwidth specified in the SS3 data file."))
  }

  # Find ID columns and data columns to replace:
  old_len_columns <- grep("^l[0-9.]+$", names(datfile$lencomp))
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

  # Substitute new bins:
  datfile$lencomp <- data.frame(datfile$lencomp[, id_columns], lcomp_new)
  datfile$lbin_vector <- bin_vector
  datfile$N_lbins <- length(datfile$lbin_vector)

  # change the lbin_method
  if (!is.null(lbin_method)) {
    if (lbin_method == 1) {
      datfile$lbin_method <- lbin_method
      datfile$binwidth <- NULL
      datfile$minimum_size <- NULL
      datfile$maximum_size <- NULL
    }
  }

  # Re-bin conditional age-at-length comps:
  if (rebin_cal) {
    if (is.null(datfile$agecomp))
      stop(paste("No age composition data were found in the data file within",
        "change_em_binning(). These are needed to modify the conditional",
        "age-at-length data binning structure."))
    # if all Lbin_lo == -1 then there aren't any CAL data:
    if (length(unique(datfile$agecomp$Lbin_lo)) == 1)
      stop(paste("It looks like there aren't any conditional age-at-length",
        "composition data in the age composition data matrix in your",
        "data file."))
    if (length(unique(datfile$agecomp$AgeErr)) != 1)
      stop(paste("change_em_binning only works for conditional age-at-length",
        "composition data with a single value for all AgeErr columns."))

    # grab the data we'll work with:
    old_age <- datfile$agecomp[datfile$agecomp$Lbin_lo == -1, ]
    old_cal <- datfile$agecomp[datfile$agecomp$Lbin_lo != -1, ]

    # make a lookup table of old and new bins:
    lookup <- data.frame(
      Lbin_hi = seq_along(old_binvector),
      lbin_orig = old_binvector,
      lbin_new = old_binvector[findInterval(old_binvector, bin_vector)])

    # the magic re-binning happens here:
    # this uses dplyr and pipes but avoids non-standard evaluation
    # http://cran.r-project.org/web/packages/dplyr/vignettes/nse.html
    new_cal <- inner_join(old_cal, lookup, by = "Lbin_hi") %>%
      group_by_(~Yr, ~Seas, ~Flt, ~Gender, ~Part, ~AgeErr, ~Nsamp, ~lbin_new) %>%
      summarise_each_(funs_(~sum), ~matches("^a[0-9.]+$")) %>%
      rename_(Lbin_lo = ~lbin_new) %>%
      mutate_(Lbin_hi = ~Lbin_lo) %>%
      as.data.frame

    # re-order the columns:
    new_cal <- new_cal[, match(names(old_cal), names(new_cal))]

    # add back the 'id' numbering of CAL data:
    new_cal$Lbin_lo <- as.numeric(as.factor(new_cal$Lbin_lo))
    new_cal$Lbin_hi <- as.numeric(as.factor(new_cal$Lbin_hi))

    # and slot the new data into the .dat file:
    datfile$agecomp <- rbind(old_age, new_cal)
    datfile$N_agecomp <- nrow(datfile$agecomp)
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
