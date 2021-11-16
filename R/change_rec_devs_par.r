#' Replace recruitment deviations
#'
#' This function replaces the recruitment deviations in the
#' `ss.par` file with those specified in `recdevs_new`, as
#' well as a comment (for debugging). It then writes a new file with
#' name `par_file_out` into the working directory.
#'
#' @param recdevs_new A vector of new recruitment deviations.
#' @template par_file_in
#' @template par_file_out
#' @return A modified Stock Synthesis `.par` file.
#' @author Cole Monnahan
#' @export
#'

change_rec_devs_par <- function(recdevs_new,
                                par_file_in = "ss.par",
                                par_file_out = "ss.par") {
  ## This is the pattern on the line before the vector of current recdevs
  pattern <- "# recdev1"

  if (!file.exists(par_file_in)) stop(paste("File", par_file_in, "not found"))
  par <- readLines(par_file_in, warn = FALSE)
  which.line <- grep(pattern = pattern, x = par) + 1

  ## grab the old ones, note there is a leading space that needs to be
  ## deleted
  recdevs.old <- par[which.line]
  recdevs.old <- gsub("^\\s+|\\s+$", "", recdevs.old) # remove leading blank
  recdevs.old <- gsub("\\s+", " ", recdevs.old) # remove >1 blanks
  recdevs.old <- as.numeric(unlist(strsplit(recdevs.old, split = " ")))
  num.years <- length(recdevs.old)

  ##  Cut off extra recdevs:
  recdevs_new <- recdevs_new[seq_along(recdevs.old)]

  ## Check that the length of the recdevs matches up
  if (length(recdevs_new) != length(recdevs.old)) {
    stop("The new recdev vector isn't the same length as what is
      currently in the ss.par file")
  }

  ## replace w/ new recdevs, adding back in that leading space
  par[which.line] <- paste0(" ", recdevs_new, collapse = "")
  ## Write it back to file
  writeLines(par, con = par_file_out)
}
