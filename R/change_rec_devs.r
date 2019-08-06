#' Replace recruitment deviations
#'
#' This function replaces the recruitment deviations in the
#' control file with those specified in \code{recdevs}. 
#' The new control file is then written to the disk if 
#' \code{ctl_file_out} is specified. 
#' It is imperative that the path provided in \code{ctl_file_in}
#' be to a \code{ss_new} file so \code{change_rec_devs} can 
#' properly determine how many recruitment deviations to input
#' into the control file. 
#'
#' This function does not need to be specified in a case file if you
#' are running an ss3sim simulation using \code{\link{run_ss3sim}}.
#' 
#' @param recdevs A vector of recruitment deviations to be entered into
#' the SS control file. A single value will be repeated for every value in
#' \code{years} or \code{length(years) == length(recdevs)} must be true.
#' @template ctl_file_in
#' @template ctl_file_out
#' @return A modified SS control file.
#' @author Kelli Faye Johnson
#' @export
#'
#' @examples
#' d <- system.file(file.path("extdata", "models"), package = "ss3sim")
#' change_rec_devs(recdevs = rlnorm(100), 
#'   ctl_file_in = file.path(d, "cod-om", "codOM.ctl"),
#'   ctl_file_out = file.path(tempdir(), "control_recdevs.ss"))

change_rec_devs <- function(recdevs, 
  ctl_file_in, ctl_file_out = "control_recruitment.ss") {

  ctl <- readLines(ctl_file_in)
  devs <- grep("#\\s*[0-9]+[RFE]", ctl, value = TRUE)
  years <- unlist(strsplit(
    gsub("^_", "", 
    gsub("^#\\s*|\\s|R|F|E", "_", devs)), "_+"))
  n.years <- length(years)
  recdevs <- recdevs[seq_along(years)]
  newdata <- data.frame(
    "Yr" = years,
    "recdev" = recdevs)
    
  locations <- grep("do_recdev", ctl)
  ctl[locations] <- gsub("^[0-4]\\s*", "1 ", trimws(ctl[locations]))
  ctl[locations + 4] <- gsub("^[0-1]\\s*", "1 ", trimws(ctl[locations + 4]))
  locations <- grep("read_recdevs|Fishing Mortality info", ctl)
  if (length(locations) > 2) stop("Fishing Mortality info was found more", 
    " than once in the control file")
  if (length(locations) == 1) {
    locations[2] <- grep("F ballpark$", ctl)
  }
  ctl[locations[1]] <- gsub("^[0-9]+\\s*", paste0(n.years, " "), 
    trimws(ctl[locations[1]]))
  ctl <- ctl[-((locations[1] + 1):(locations[2] - 1))]
  ctl <- append(ctl, 
    values = apply(newdata, 1, paste, collapse = " "),
    after = locations[1])
  
  # Write new control file
  if (!is.null(ctl_file_out)) {
    writeLines(ctl, con = ctl_file_out)
    close(file(ctl_file_out))
  }
  invisible(ctl)
}

