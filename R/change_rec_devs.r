#' Replace recruitment deviations
#'
#' This function replaces the recruitment deviations in the
#' \code{ss3.par} file with those specified in \code{recdevs_new}, as
#' well as a comment (for debugging). It then writes a new file with
#' name \code{par_file_out} into the working directory.
#'
#' @param recdevs_new A vector of new recruitment deviations.
#' @template par_file_in
#' @template par_file_out
#' @return A modified SS3 \code{.par} file.
#' @author Cole Monnahan
#' @details This function does not need to be specified in a case file if you
#'   are running and ss3sim simulation through case files with
#'   \code{\link{run_ss3sim}}.
#' @export
#'
#' @examples
#' # Create a temporary folder for the output:
#' temp_path <- file.path(tempdir(), "ss3sim-recdev-example")
#' dir.create(temp_path, showWarnings = FALSE)
#'
#' par_file <- system.file("extdata", "models", "cod-om", "ss3.par",
#'   package = "ss3sim")
#' change_rec_devs(recdevs_new = rlnorm(100), par_file_in = par_file,
#'   par_file_out = paste0(temp_path, "/test.par"))

change_rec_devs <- function(recdevs_new, par_file_in = "ss3.par",
  par_file_out="ss3.par"){
  ## This is the pattern on the line before the vector of current recdevs
  pattern <- "# recdev1"

  if(!file.exists(par_file_in)) stop(paste("File", par_file_in,"not found"))
  par <- readLines(par_file_in, warn = FALSE)
  which.line <- grep(pattern=pattern, x=par)+1

  ## grab the old ones, note there is a leading space that needs to be
  ## deleted
  recdevs.old <- par[which.line]
  recdevs.old <- gsub("^\\s+|\\s+$", "", recdevs.old) # remove leading blank
  recdevs.old <- gsub("\\s+", " ", recdevs.old)       # remove >1 blanks
  recdevs.old <- as.numeric(unlist(strsplit(recdevs.old, split= " ")))
  num.years <- length(recdevs.old)

  ##  Cut off extra recdevs:
  recdevs_new <- recdevs_new[1:length(recdevs.old)]

  ## Check that the length of the recdevs matches up
  if(length(recdevs_new) != length(recdevs.old)){
    stop("The new recdev vector isn't the same length as what is
      currently in the ss3.par file")
  }

  ## replace w/ new recdevs, adding back in that leading space
  par[which.line] <- paste0(" ", recdevs_new, collapse="")
  ## Write it back to file
  writeLines(par, con = par_file_out)
}

