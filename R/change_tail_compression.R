#' Replace tail compression value for length composition data
#'
#' This function replaces the tail compression value for length-composition data
#' in a `.dat` file that was read in using [r4ss::SS_readdat()]
#' with those specified in
#' `tail_compression`. It then writes a new file with name `dat_file_out`
#' into the working directory.
#'
#' @param tail_compression *The new tail_compression value to be used. Must be a
#'   numeric value, as a proportion. For example 0.1 means 10 percent. See the
#'   Stock Synthesis manual for further information. A NULL value indicates no action, a
#'   negative value indicates to Stock Synthesis to ignore it (not use that feature).
#' @template dat_list
#' @template outfile
#' @return A modified Stock Synthesis `.dat` file is returned invisibly.
#' @author Cole Monnahan
#' @export

change_tail_compression <- function(tail_compression, dat_list,
                                    outfile = NULL) {
  if (is.null(tail_compression)) {
    return(invisible(dat_list))
  }
  stopifnot(is.numeric(tail_compression))

  # The data sections are repeated in the data.ss_new files, so only use first one
  dat_list$comp_tail_compression[1] <- tail_compression
  if (!is.null(outfile)) {
    r4ss::SS_writedat(dat_list,
      outfile,
      overwrite = TRUE,
      verbose = FALSE
    )
  }

  invisible(dat_list)
}
