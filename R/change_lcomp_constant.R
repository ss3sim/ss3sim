#' Set the robustification constant for length-composition data
#'
#' This function replaces the robustification value for length-composition data
#' in a `.dat` file that was read in using [r4ss::SS_readdat()]
#' with those specified in `lcomp_constant`.
#' It then writes a new file with name `outfile` into the working directory.
#'
#' @details The robustification constant is added to both the observed and
#'   expected proportions of length composition data, before being normalized
#'   internally. It is designed to help stabilize the model, but is unclear how
#'   and when to use it for optimal effect. The same value is used for all
#'   length data.
#' @param lcomp_constant The new value to be used. Must be a numeric value, as
#'   a proportion. For example 0.1 means 10 percent. See the Stock Synthesis manual for
#'   further information. A NULL value indicates no action resulting in using
#'   the current value, and a value of 0 will throw an error since that leads to
#'   an error when zeroes exist in the data. Instead use a very small value like
#'   1e-07.
#' @template dat_list
#' @template outfile
#' @return A modified Stock Synthesis `.dat` file, and that file returned invisibly
#'   (for testing) as a vector of character lines.
#' @author Cole Monnahan

change_lcomp_constant <- function(lcomp_constant, dat_list, outfile = NULL) {
  if (is.null(lcomp_constant)) {
    return(invisible(dat_list))
  }
  stopifnot(is.numeric(lcomp_constant))
  if (lcomp_constant <= 0) stop("lcomp_constant must be greater than 0")

  # The data sections are repeated in the data.ss_new files, so only use first one
  dat_list$add_to_comp[1] <- lcomp_constant

  if (!is.null(outfile)) {
    ss_version <- get_ss_ver_dl(dat_list)
    r4ss::SS_writedat(dat_list, outfile,
      overwrite = TRUE,
      version = ss_version, verbose = FALSE
    )
  }

  invisible(dat_list)
}
