#' Replace recruitment deviations
#'
#' This function replaces the recruitment deviations in the
#' control file of a Stock Synthesis model with those specified in the argument
#' \code{recdevs}. The new control file is then written to the disk if
#' \code{ctl_file_out} is specified.
#' It is imperative that the path provided in \code{ctl_file_in}
#' be to a \code{ss_new} file so \code{change_rec_devs} can
#' properly determine where to place the recruitment deviations
#' in the control file.
#'
#' This function does not need to be specified in a case file if you
#' are running an ss3sim simulation using \code{\link{run_ss3sim}}.
#'
#' @param recdevs A vector of recruitment deviations to be entered into
#' the SS control file. The vector must be the same length as the vector
#' of recruitment deviations that are commented out in the \code{ss_new}
#' control file. This vector can be found by searching for
#' \code{# all recruitment deviations} within the file.
#' If a single value is provided instead of a vector, the value
#' will be repeated for every recruitment deviation in the model.
#' Alternatively, users can supply a named vector with each name being a year
#' of the model. Missing years will be filled in with values of zero.
#' @template ctl_file_in
#' @template ctl_file_out
#' @return A modified SS control file.
#' @author Kelli Faye Johnson
#' @importFrom utils type.convert
#' @export
#'
#' @examples
#' d <- system.file(file.path("extdata", "models"), package = "ss3sim")
#' change_rec_devs(recdevs = rlnorm(101),
#'   ctl_file_in = file.path(d, "cod-om", "codOM.ctl"),
#'   ctl_file_out = file.path(tempdir(), "control_recdevs.ss"))
#' # Change the recruitment deviations in years 2:11
#' change_rec_devs(recdevs = setNames(rlnorm(10), 2:11),
#'   ctl_file_in = file.path(d, "cod-om", "codOM.ctl"),
#'   ctl_file_out = file.path(tempdir(), "control_recdevsInitial.ss"))
#' sapply(dir(tempdir(), pattern = "control_.+ss", full.names = TRUE), unlink)

change_rec_devs <- function(recdevs,
  ctl_file_in, ctl_file_out = "control_recruitment.ss") {

  ctl <- readLines(ctl_file_in)
  default_err_msg <- paste0(" Please make sure you are using a control file\n",
    "that was run through SS to get a control.ss_new\n",
    "so that it has the default SS comments that ",
    "ss3sim expects.")

  vecnames <- type.convert(names(recdevs), as.is = TRUE)
  if (!all(is.numeric(vecnames))) {
    devs <- grep("#\\s*[0-9]+[RFE]", ctl, value = TRUE)
    years <- unlist(strsplit(
      gsub("^_", "",
      gsub("^#\\s*|\\s|R|F|E", "_", devs)), "_+"))
    if (length(recdevs) == 1) recdevs <- rep(recdevs, length.out = n.years)
  } else {years <- vecnames}
  n.years <- length(years)
  if(n.years == 0) {
    stop("The number of recdevs and their associated years could not be ",
         "determined\nfrom the control file.", default_err_msg)
  }
  if(length(recdevs) != n.years) {
    stop("The length of recdevs was, ", sum(!is.na((recdevs))),
    	", and does not match the number of years in the model.\n",
      "Please change input argument 'recdevs' to have either length of 1,\n",
      "which will be repeated for each year in the model, or ", n.years, ".")
   }
  newdata <- data.frame(
    "Yr" = years,
    "recdev" = recdevs)
  if(anyNA(newdata)) {
    stop("change_rec_devs is attempting to add in NA values for recruitment\n",
       "deviations values or associated years in the control file.")
  }
  locations <- grep("do_recdev", ctl) # find start of recdevs section
  #turn on recruitment devs and advanced options
  ctl[locations] <- gsub("^[0-4]\\s*", "1 ", trimws(ctl[locations]))
  # TODO: add ability to turn on advanced options with this script if they are
  # off. For now, provide an informative error.
  read_adv_option <- as.integer(strsplit(trimws(ctl[locations + 4]), "\\s+")
                                [[1]][1])
  if(read_adv_option == 0) {
    stop("Currently ss3sim can only add recruitment deviations to control ",
      "files that have advanced recruitment options turned on. In the OM ",
      "control file, please change value on line with comment '# (0/1) to read",
      " 13 advanced options' from 0 to 1 and add values for other advanced ",
      "options into the OM control file.")
    #following line turns on advanced recdevs options:
    # ctl[locations + 4] <- gsub("^[0-1]\\s*", "1 ", trimws(ctl[locations + 4]))
    # Add other code to add in the other lines needed instead of stopping.
  } else if(read_adv_option != 1){
    stop("The value in the OM control file to read advanced options is ",
         read_adv_option, ", but should be 1.")
  }
  # Find location to insert recdevs.
  locations <- grep("read_recdevs", ctl)
  if(length(locations) == 0) {
    stop("ss3sim could not find the line with comment '#_read_recdevs' in the ",
         "OM control file.", default_err_msg)
  }
  locations <- c(locations, grep("Fishing Mortality info", ctl))
  if (length(locations) > 2) {
    stop("Fishing Mortality info was found more",
    " than once in the control file")
  }
  if (length(locations) == 1) {
    tmp_loc <- grep("F ballpark$", ctl)
    if(length(tmp_loc) == 1){
      locations[2] <- grep("F ballpark$", ctl)
    } else {
      stop("Could not find spot where Fishing Mortality info starts.",
           default_err_msg)
    }
  }
  # insert recdevs.
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

