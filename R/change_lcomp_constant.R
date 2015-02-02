#' Set the robustification constant for length composition data.
#'
#' This function replaces the robustification value for length composition data
#' in a \code{dat} file (\code{file_in}) with those specified in
#' \code{lcomp_constant}. It then writes a new file with name \code{file_out}
#' into the working directory. If used with \code{\link{run_ss3sim}} the case
#' file should be named \code{lcomp_constant}. A suggested case letter is
#' \code{C}.
#'
#' @details The robustification constant is added to both the observed and
#'   expected proportions of length composition data, before being normalized
#'   internally. It is designed to help stabilize the model, but is unclear how
#'   and when to use it for optimal effect. The same value is used for all
#'   length data.
#' @param lcomp_constant *The new value to be used. Must be a numeric value, as
#'   a proportion. For example 0.1 means 10 percent. See the SS3 manual for
#'   further information. A NULL value indicates no action resulting in using
#'   the current value, and a value of 0 will throw an error since that leads to
#'   an error when zeroes exist in the data. Instead use a very small value like
#'   1e-07.
#' @param datfile Input SS3 dat file \bold{as returned in list object form from
#'   \code{\link[r4ss]{SS_readdat}}}.
#' @param file_out Output SS3 dat file. Typically the same as \code{file_in}.
#' @param write_file Should the data file be written to disk?
#' @return A modified SS3 \code{.dat} file, and that file returned invisibly
#'   (for testing) as a vector of character lines.
#' @template casefile-footnote
#' @author Cole Monnahan
#' @importFrom r4ss SS_writedat

#' @examples
#' ## Create a temporary folder for the output:
#' temp_path <- file.path(tempdir(), "ss3sim-lcomp-constant-example")
#' dir.create(temp_path, showWarnings = FALSE)
#' ## Run the function with built-in data file.
#' dat_file <- system.file("extdata", "example-om", "data.ss_new",
#'   package = "ss3sim")
#' input_dat <- r4ss::SS_readdat(dat_file)
#' test <- change_lcomp_constant(lcomp_constant = .1234, input_dat,
#'   file_out = paste0(temp_path, "/test.dat"))
#' ## Look at the changes
#' print(test$add_to_comp)
#' unlink(temp_path)
change_lcomp_constant <- function(lcomp_constant, datfile, file_out,
  write_file = TRUE){

  if(is.null(lcomp_constant)) return(invisible(NULL))
  stopifnot(is.numeric(lcomp_constant))
  if(lcomp_constant <= 0) stop("lcomp_constant must be greater than 0")

  # The data sections are repeated in the data.ss_new files, so only use first one
  datfile$add_to_comp[1] <- lcomp_constant

  if(write_file) SS_writedat(datfile, file_out, overwrite = TRUE, verbose = FALSE)

  invisible(datfile)
}
