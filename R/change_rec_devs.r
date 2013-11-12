#' Replace recruitment deviations
#'
#' This function replaces the recruitment deviations in the
#' \code{ss3.par} file with those specified in \code{recdevs_new}, as
#' well as a comment (for debugging). It then writes a new file with
#' name \code{file_out} into the working directory.
#'
#' @param recdevs_new A vector of new recruitment deviations.
#' @param file_in Input SS3 par file.
#' @param file_out Output SS3 par file.
#' @author Cole Monnahan
#' @export

#' @examples \dontrun{
#' par_file <- system.file("extdata", "models", "cod-om", "ss3.par",
#'   package = "ss3sim")
#' change_rec_devs(recdevs_new = rlnorm(100), file_in = par_file,
#'   file_out = "test.par")
#' # Now go look at the deviations after "recdev1:" in test.par in your
#' # working directory.
#' unlink("test.par")
#' }

change_rec_devs <- function(recdevs_new, file_in="ss3.par", file_out="ss3.par"){
  ## This is the pattern on the line before the vector of current recdevs
  pattern <- "# recdev1"

  if(!file.exists(file_in)) stop(paste("File", file_in,"not found"))
  par <- readLines(file_in)
  which.line <- grep(pattern=pattern, x=par)+1

  ## grab the old ones, note there is a leading space that needs to be
  ## deleted
  recdevs.old <- par[which.line]
  recdevs.old <- gsub("^\\s+|\\s+$", "", recdevs.old) # remove leading blank
  recdevs.old <- gsub("\\s+", " ", recdevs.old)       # remove >1 blanks
  recdevs.old <- as.numeric(unlist(strsplit(recdevs.old, split= " ")))
  num.years <- length(recdevs.old)

  ## Cut off extra recdevs:
  recdevs_new <- recdevs_new[1:length(recdevs.old)]

  ## Check that the length of the recdevs matches up
  if(length(recdevs_new) != length(recdevs.old)){
    stop("The new recdev vector isn't the same length as what is
      in the ss3.par file")
  }

  ## replace w/ new recdevs, adding back in that leading space
  par[which.line] <- paste0(" ", recdevs_new, collapse="")
  ## Write it back to file
  writeLines(par, con=file_out)
}

