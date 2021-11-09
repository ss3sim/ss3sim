#' Add or remove catchability parameters in a Stock Synthesis control file
#'
#' Catchability, \eqn{q}, is the proportionality parameter between
#' fishing effort and population abundance.
#' That is, when \eqn{q=1},
#' a survey of the population is thought to encompass the entire population.
#' Removing or adding a catch-per-unit effort time series
#' can require changes to the control file because
#' each catch-per-unit-effort time series requires set up and parameter lines.
#'
#' @details
#' In Stock Synthesis, environmental time series can also be modeled as
#' catch-per-unit-effort time series.
#' Readers interested in the complete range of functionality should see the
#' [section of the Stock Synthesis user manual on catchability](
#' https://nmfs-stock-synthesis.github.io/ss-documentation/SS330_User_Manual.html#catchability).
#' `code{change_q} has limited functionality relative to
#' what is available in Stock Synthesis.
#' For example, `change_q` cannot add a parameter to estimate additional variance.
#' Though it will remove additional variance parameters for
#' fleets that no longer have survey data.
#' Additionally, the float term is not used within ss3sim and is set to zero.
#'
#' This function can add and remove parameters in the control file simultaneously.
#' Thus, it is not necessary to call it twice to perform both operations.
#'
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
#' @return A modified Stock Synthesis control file list with
#' the same structure returned by [r4ss::SS_readctl()].
#' @seealso [check_q()] can determine which fleets should removed or added.
#' @author Kelli F. Johnson
change_q <- function(string_add = NULL, string_remove = NULL,
  ctl_list, dat_list, ctl_file_in = NULL, dat_file_in = NULL, ctl_file_out = NULL,
  overwrite = FALSE, verbose = FALSE) {

  if(!is.null(dat_file_in)) {
    dat_list <- r4ss::SS_readdat(verbose = FALSE, echoall = FALSE,
      file = dat_file_in)
  }
  if(!is.null(ctl_file_in)) {
    ctl_list <- r4ss::SS_readctl(verbose = FALSE,
      file = ctl_file_in,
      use_datlist = TRUE, datlist = dat_list)
  }
  add <- remove <- NULL

  for(ii in c("remove", "add")) {
    thisloop <- get(paste0("string_", ii))
    if(is.null(thisloop)) next
    chars <- grepl("[a-zA-Z]", thisloop)
    temp <- unlist(utils::type.convert(ifelse(grepl("[a-zA-Z]", thisloop),
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
      utils::type.convert(gsub("a-zA-Z_\\(\\)", "", row.names(ctl_list$Q_parms)), as.is = TRUE)), ]
  }

  if (!is.null(ctl_file_out)) {
    r4ss::SS_writectl(ctllist = ctl_list, outfile = ctl_file_out,
      verbose = FALSE, overwrite = overwrite)
  }
  invisible(ctl_list)
}

#' Check if desired \eqn{q} parameters exist in control file
#' 
#' Check a Stock Synthesis control file to determine if the desired fleets
#' have q parameters.
#' 
#' @template ctl_list
#' @param Nfleets The number of fleets in the model.
#' This can be determined manually from the data file or
#' using the R object returned from [r4ss::SS_readdat()], of which
#' `Nfleets` is a named element.
#' @param desiredfleets A numeric vector specifying which fleets should have
#' catchability parameters.
#'
#' @export
#' @return A list with two vectors specifying which fleets to add and which
#' to remove from the control file.
#' @seealso [change_q()]
#' @examples
#' dat <- r4ss::SS_readdat(
#'   file = dir(
#'     path = system.file("extdata", "models", "cod-om", package = "ss3sim"),
#'     pattern = "\\.dat",
#'     full.names = TRUE
#'   ),
#'   verbose = FALSE
#' )
#' ctl <- r4ss::SS_readctl(
#'   file = dir(
#'     path = system.file("extdata", "models", "cod-om", package = "ss3sim"),
#'     pattern = "\\.ctl",
#'     full.names = TRUE
#'   ),
#'   verbose = FALSE,
#'   use_datlist = TRUE, datlist = dat
#' )
#' stopifnot(check_q(ctl, dat[["Nfleets"]], desiredfleets = 1)[["remove"]] == 2)
#' stopifnot(all(mapply(is.null, check_q(ctl, dat[["Nfleets"]], desiredfleets = 1:2))))
#' stopifnot(check_q(ctl, dat[["Nfleets"]], desiredfleets = 1:3)[["add"]] == 3)
#' stopifnot(check_q(ctl, dat[["Nfleets"]], desiredfleets = 2:3)[["remove"]] == 1)
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
