#' Change age or length bins in an SS operating model
#'
#' \code{change_bins} alters the bin structure for the age or length composition
#' data in an SS operating model. By adding dummy data at the appropriate bin
#' sizes to the SS \code{.dat} file, SS will record age or length composition
#' data in the appropriate bins when the operating model is run.
#'
#' @param file_in A character value giving the location of an SS \code{.dat}
#'   file to input.
#' @param file_out A character value giving the location of an SS \code{.dat}
#'   file to output.
#' @param bin_vector A numeric vector of bins to substitute into the \code{.dat}
#'   file.
#' @param type One of \code{"length"} or \code{"age"}. This controls whether the
#'   function acts on the age composition or length composition data. Default is
#'   \code{"length"}.
#' @param write_file Should the \code{.dat} file be written? The new \code{.dat}
#'   file will always be returned invisibly by the function. Setting
#'   \code{write_file = FALSE} can be useful for testing. Note that you must
#'   supply a value to the argument \code{file_out}, but this argument can be
#'   set to any arbitrary value (such as \code{NULL}) if \code{write_file =
#'   FALSE}.
#' @importFrom r4ss SS_readdat SS_writedat
#' @export
#' @seealso \code{\link{sample_lcomp}}, \code{\link{sample_agecomp}}
#' @author Ian Taylor, Sean Anderson
#' @examples
#' d <- system.file("extdata", package = "ss3sim")
#' f_in <- paste0(d, "/example-om/data.ss_new")
#' l <- change_bins(f_in, file_out = NULL, type = "length",
#'   bin_vector = seq(2, 8, 2), write_file = FALSE)
#' print(l$lbin_vector)
#' print(head(l$lencomp))
#'
#' a <- change_bins(f_in, file_out = NULL, type = "age",
#'   bin_vector = seq(2, 8, 2), write_file = FALSE)
#' print(a$agebin_vector)
#' print(head(a$agecomp))

change_bins <- function(file_in, file_out, bin_vector, type = c("length", "age"),
  write_file = TRUE) {

  if(!type[1] %in% c("length", "age")) {
    stop("type must be one of length or age")
  }
  if(!is.numeric(bin_vector)) {
    stop("bin_vector must be numeric")
  }
  if(length(bin_vector) < 3) {
    warning(paste("The length of bin_vector looks small; are you sure you",
       "input a full vector of bins and not a bin size?"))
  }

  datfile <- SS_readdat(file = file_in, verbose = FALSE)

  if(type[1] == "length") {

    if(length(unique(datfile$lencomp$Gender)) > 1) {
      stop(paste("There are 2 genders defined in the length composition",
          "data; change_bin only works with single-gender models."))
    }

    datfile$lbin_vector <- bin_vector
    newdummy <- data.frame(matrix(1, nrow = nrow(datfile$lencomp),
      ncol = length(datfile$lbin_vector)))
    # Find ID columns and data columns to replace:
    names(newdummy) <- paste0("l", datfile$lbin_vector)
    old_len_columns <- grep("^l[0-9.]+$", names(datfile$lencomp))
    # Substitute new bins:
    id_columns <- seq_along(names(datfile$lencomp))[-old_len_columns]
    datfile$lencomp <- data.frame(datfile$lencomp[, id_columns], newdummy)
  }

  if(type[1] == "age") {

    if(length(unique(datfile$agecomp$Gender)) > 1) {
      stop(paste("There are 2 genders defined in the age composition",
          "data; change_bin only works with single-gender models."))
    }

    datfile$agebin_vector <- bin_vector
    newdummy <- data.frame(matrix(1, nrow = nrow(datfile$agecomp),
      ncol = length(datfile$agebin_vector)))
    # Find ID columns and data columns to replace:
    names(newdummy) <- paste0("a", datfile$agebin_vector)
    old_age_columns <- grep("^a[0-9.]+$", names(datfile$agecomp))
    # Substitute new bins:
    id_columns <- seq_along(names(datfile$agecomp))[-old_age_columns]
    datfile$agecomp <- data.frame(datfile$agecomp[, id_columns], newdummy)
  }

  if(write_file) {
    SS_writedat(datlist = datfile, outfile = file_out)
  }

  invisible(datfile)
}
