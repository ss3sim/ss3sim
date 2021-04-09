#' Add or remove a q in an SS control file
#'
#' This function adds or removes a q setup line in an SS control file.
#' 
#' @details
#' Parameters that are added are simple versions of the structures available
#' within Stock Synthesis. For example, the current functionality of
#' \code{change_q} does not allow for modeling an additional parameter for
#' added variance for the given survey of interest. Though it will remove
#' that parameter if present for a given fleet that no longer has survey data.
#' Additionally, the float term is not used and is instead set to zero.
#' 
#' \code{change_q} can add and remove parameters in the control file
#' simultaneously, and thus, it is not necessary to call the function twice
#' when wanting to perform both operations. A helper function,
#' \code{\link{check_q}} is a available to determine which fleets one should
#' remove and add.
#' @param string_add A vector of character strings with the fleetnames to add.
#' Numeric values representing the fleet number are also allowed or any
#' combination thereof.
#' @param string_remove A vector of character strings with the fleetnames to remove.
#' Numeric values representing the fleet number are also allowed or any
#' combination thereof.
#' @template ctl_list
#' @template dat_list
#' @template ctl_file_in
#' @template dat_file_in
#' @template ctl_file_out
#' @template overwrite
#' @template verbose
#' @return A modified SS control file list with the same structure as that read
#' in by \code{\link[r4ss]{SS_readctl}}.
#' @seealso \code{\link{change_q}}
#' @author Kelli Faye Johnson
change_q <- function(string_add = NULL, string_remove = NULL,
  ctl_list, dat_list, ctl_file_in = NULL, dat_file_in = NULL, ctl_file_out = NULL,
  overwrite = FALSE, verbose = FALSE) {

  if(!is.null(dat_file_in)) {
    dat_list <- r4ss::SS_readdat(verbose = FALSE, echoall = FALSE,
      file = dat_file_in)
  }
  if(!is.null(ctl_file_in)) {
    ctl_list <- r4ss::SS_readctl(verbose = FALSE, echoall = FALSE,
      file = ctl_file_in,
      use_datlist = TRUE, datlist = dat_list)
  }
  add <- remove <- NULL

  for(ii in c("remove", "add")) {
    thisloop <- get(paste0("string_", ii))
    if(is.null(thisloop)) next
    chars <- grepl("[a-zA-Z]", thisloop)
    temp <- unlist(type.convert(ifelse(grepl("[a-zA-Z]", thisloop),
      mapply(grep, thisloop, 
        MoreArgs = list(x = dat_list[["fleetnames"]], ignore.case = TRUE)),
      thisloop), as.is = TRUE))
    if(length(thisloop) != length(temp) & verbose) {
      warning("Not all fleets in string.", ii, " were present to be removed.",
        "\nThose not present will be ignored.")
    }
    temp <- temp[order(temp)]
    if(!all(temp %in% seq(dat_list$Nfleets))) {
      warning("Not all fleets in string.", ii, " were present to be removed.")
      temp <- temp[temp %in% seq(dat_list$Nfleets)]
    }
    assign(ii, temp)
  }

  if(!is.null(string_remove) & length(remove) > 0) {
    ctl_list$Q_options <- ctl_list$Q_options[!ctl_list$Q_options$fleet %in% remove, ]
    if(NROW(ctl_list[["Q_options"]]) == 0) {
      ctl_list$Q_options <- NULL
      ctl_list$Q_parms <- NULL
    } else {
      ctl_list$Q_parms <- ctl_list$Q_parms[
        !grepl(
          paste0("\\(", remove, "\\)$", collapse = "|"),
          row.names(ctl_list$Q_parms)
        ), ]
    }
  }

  if(!is.null(string_add) & length(add) > 0) {
    # todo: add ability to estimate extra SE
    ctl_list$Q_options <- rbind(
      data.frame("fleet" = add, "link" = 1, "link_info" = 1,
        "extra_se" = 0, "biasadj" = 0, "float" = 1),
      ctl_list$Q_options)
    ctl_list$Q_options <- ctl_list$Q_options[order(ctl_list$Q_options$fleet), ]
    row.names(ctl_list$Q_options)[add] <- dat_list$fleetnames[add]
    temp <- data.frame("LO" = -3, "HI" = 3, "INIT" = 0.0, "PRIOR" = 0,
        "PR_SD" = 99, "PR_type" = 0, "PHASE" = -5, "env_var&link" = 0,
        "dev_link" = 0, "dev_minyr" = 0, "dev_maxyr" = 0,
        "dev_PH" = 0, "Block" = 0, "Block_Fxn" = 0)
    temp[seq(dat_list$Nfleets), ] <- temp[1, ]
    temp <- temp[add, ]
    row.names(temp) <- paste0("LnQ_base_", dat_list$fleetnames[add], "(", add, ")")
    ctl_list$Q_parms <- rbind(temp, ctl_list$Q_parms)
    ctl_list$Q_parms <- ctl_list$Q_parms[order(
      type.convert(gsub("a-zA-Z_\\(\\)", "", row.names(ctl_list$Q_parms)), as.is = TRUE)), ]
  }

  if (!is.null(ctl_file_out)) {
    SS_writectl(ctllist = ctl_list, outfile = ctl_file_out,
      verbose = FALSE, overwrite = overwrite)
  }
  invisible(ctl_list)
}

#' Check if desired q parameters exist in control file
#' 
#' Check a Stock Synthesis control file to determine if the desired fleets
#' have q parameters.
#' 
#' @template ctl_list
#' @param Nfleets The number of fleets in the model. This can be determined
#' from the data file or using \code{\link[r4ss]{SS_readdat}}, of which
#' \code{Nfleets} is a named element.
#' @param desiredfleets A numeric vector specifying which fleets you want
#' to have q parameters for.
#' @export
#' @return A list with two vectors specifying which fleets to add and which
#' to remove from the control file.
#' @seealso \code{change_q}
#' @examples
#' dat <- r4ss::SS_readdat(
#'   dir(system.file("extdata", "models", "cod-om", package = "ss3sim"),
#'  "\\.dat", full.names = TRUE), verbose = FALSE)
#' ctl <- r4ss::SS_readctl(
#'   dir(system.file("extdata", "models", "cod-om", package = "ss3sim"),
#'  "\\.ctl", full.names = TRUE), verbose = FALSE, echoall = FALSE,
#'  use_datlist = TRUE, datlist = dat)
#' stopifnot(check_q(ctl, dat$Nfleets, desiredfleets = 1)$remove == 2)
#' stopifnot(all(mapply(is.null, check_q(ctl, dat$Nfleets, desiredfleets = 1:2))))
#' stopifnot(check_q(ctl, dat$Nfleets, desiredfleets = 1:3)$add == 3)
#' stopifnot(check_q(ctl, dat$Nfleets, desiredfleets = 2:3)$remove == 1)
#'
check_q <- function(ctl_list, Nfleets, desiredfleets) {
  #figure out which are needed and which are not
  no_q <- seq(Nfleets)[!seq(Nfleets) %in% desiredfleets]
  yes_q <- seq(Nfleets)[seq(Nfleets) %in% desiredfleets]
  # figure out which need to be removed
  remove <- ctl_list$Q_options$fleet[ctl_list$Q_options$fleet %in% no_q]
  add <- yes_q[!(yes_q %in% ctl_list$Q_options$fleet)]
  if(length(add) == 0) add <- NULL
  if(length(remove) == 0) remove <- NULL
  return(list("add" = add, "remove" = remove))
}
