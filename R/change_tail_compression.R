#' Replace tail compression value for length composition data.
#'
#' This function replaces the tail compression value for length composition data
#' in a \code{dat} file (\code{file_in}) with those specified in
#' \code{tail_compression}. It then writes a new file with name \code{file_out}
#' into the working directory. If used with \code{\link{run_ss3sim}} the case
#' file should be named \code{tail_compression}. A suggested case letter is
#' \code{T}.
#'
#' @param tail_compression *The new tail_compression value to be used. Must be a
#'   numeric value, as a proportion. For example 0.1 means 10 percent. See the
#'   SS3 manual for further information. A NULL value indicates no action, a
#'   negative value indicates to SS3 to ignore it (not use that feature).
#' @param datfile Input SS3 dat file \bold{as returned in list object form from
#'   \code{\link[r4ss]{SS_readdat}}}.
#' @param file_out Output SS3 dat file name.
#' @return A modified SS3 \code{.dat} file, and that file returned invisibly
#'   (for testing) as a vector of character lines.
#' @template casefile-footnote
#' @author Cole Monnahan
#' @importFrom r4ss SS_writedat
#'
#' @examples
#' ## Create a temporary folder for the output:
#' temp_path <- file.path(tempdir(), "ss3sim-tail-example")
#' dir.create(temp_path, showWarnings = FALSE)
#' ## Run the function with built-in data file.
#' dat_file <- system.file("extdata", "example-om", "data.ss_new",
#' package = "ss3sim")
#' input_dat <- r4ss::SS_readdat(dat_file)
#' test <- change_tail_compression(tail_compression = .1234, input_dat,
#'  file_out = paste0(temp_path, "/test.dat"))
#' ## Look at the changes
#' print(test$comp_tail_compression)
#' ## Clean up the temp files
#' unlink(temp_path)

change_tail_compression <- function(tail_compression, datfile, file_out){

  if(is.null(tail_compression)) return(invisible(NULL))
  stopifnot(is.numeric(tail_compression))

  # The data sections are repeated in the data.ss_new files, so only use first one
  datfile$comp_tail_compression[1] <- tail_compression
  SS_writedat(datfile, file_out, overwrite = TRUE, verbose = FALSE)

  return(invisible(datfile))
}
