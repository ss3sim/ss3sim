#' Change age or length bins in an SS operating model
#'
#' \code{change_bin} alters the bin structure for the age or length composition
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
#' @param type One of \code{"len"} or \code{"age"}. This controls whether the
#'   function acts on the age composition or length composition data. Default is
#'   \code{"len"}.
#' @param pop_bin Choose a real number to choose the population bin width. This option
#' only works if \code{"lbin_method"} is set to \code{"2"}. Default is
#'   \code{"NULL"} which leaves the original value.
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
#' l <- change_bin(f_in, file_out = NULL, type = "len",
#'   bin_vector = seq(2, 8, 2), write_file = FALSE)
#' print(l$lbin_vector)
#' print(head(l$lencomp))
#'
#' a <- change_bin(f_in, file_out = NULL, type = "age",
#'   bin_vector = seq(2, 8, 2), write_file = FALSE)
#' print(a$agebin_vector)
#' print(head(a$agecomp))

change_bin <- function(file_in, file_out, bin_vector, type = c("len", "age"), pop_bin=NULL,
  write_file = TRUE) {

  type <- match.arg(type)
  if(!is.numeric(bin_vector)) {
    stop("bin_vector must be numeric")
  }
  if(length(bin_vector) == 1) {
    warning(paste("length(bin_vector) == 1; are you sure you",
       "input a full numeric vector of bins and not a bin size?"))
  }
  if(length(pop_bin)!=1 & !is.null(pop_bin)) {
    stop("pop bin should be a real number")
  }

  datfile <- SS_readdat(file = file_in, verbose = FALSE)
  if(datfile$Ngenders > 1) {
    stop(paste("_Ngenders is greater than 1 in the operating model.",
        "change_bin only works with single-gender models."))
  }

  if(datfile$lbin_method != 2 & !is.null(pop_bin)) {
    stop(paste("the current model doesn't support a change in 'pop_bin' with a 'lbin_method' different than option 2"))
  }

  if(!is.null(pop_bin)) datfile$binwidth <- pop_bin

  if(type[1] == "len") {
    datfile$lbin_vector <- bin_vector
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

  if(type[1] == "age") {
    datfile$agebin_vector <- bin_vector
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

  if(write_file) {
    SS_writedat(datlist = datfile, outfile = file_out, overwrite=TRUE)
  }

  invisible(datfile)
}
