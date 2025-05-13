#' Change catch in the Stock Synthesis data list
#'
#' Change catch in the data so at least all combinations of
#' fleet, month, and year,
#' needed for catch are available.
#' Equilibrium years are generated if
#' there are equilibrium parameters in the control list.
#'
#' @param dat_list A data file read in using [r4ss::SS_readdat()].
#' @param ctl_list A control file read in using [r4ss::SS_readctl()].
#' The start and end year of the resulting data list will be based on years
#' with positive fishing mortality values, and
#' equilibrium catches will be non-zero only if
#' there is a equilibrium fishing mortality parameter for that fleet and month
#' combination.
#' @return A modified Stock Synthesis data file as a list in R.
#' @seealso [change_f()] changes the fishing mortality, \eqn{F}, parameters
#' using the control file, but these \eqn{F} values will only be implemented
#' for years with corresponding entries in the Stock Synthesis data file.
#' Thus, this function must be implemented after [change_f()].
#'
#' @author Kathryn L. Doering
#'
change_catch <- function(dat_list,
                         ctl_list) {
  #### Pull out F values from control file
  # Control file uses month instead of seas for column names
  newvals <- ctl_list[["F_setup2"]]
  colnames(newvals) <- tolower(colnames(newvals))
  colnames(newvals) <- gsub(
    pattern = "y[a-z]+",
    replacement = "year",
    x = colnames(newvals)
  )
  colnames(newvals) <- gsub(
    pattern = "month",
    replacement = "seas",
    x = colnames(newvals)
  )
  newvals <- newvals[, c("year", "seas", "fleet")]
  newvals[, "catch"] <- 1
  newvals[, "catch_se"] <- 0.01
  rownames(newvals) <- NULL

  # Find equilibrium values
  if (!is.null(ctl_list[["init_F"]])) {
    fleetnames <- gsub("InitF_|Flt[0-9]+$", "", rownames(ctl_list[["init_F"]]))
    equil <- as.data.frame(do.call("rbind", lapply(strsplit(
      x = fleetnames,
      split = "seas_|_flt_"
    ), "[", 2:3)))
    colnames(equil) <- c("seas", "fleet")
    equil[, "year"] <- -999
    equil[, "catch"] <- 1
    equil[, "catch_se"] <- 0.01
    newvals <- rbind(
      newvals,
      equil[, c("year", "seas", "fleet", "catch", "catch_se")]
    )
  }

  #### Find min and max year for data file
  min <- min(newvals[newvals[["year"]] >= 0, "year"])
  max <- max(newvals[["year"]])
  dat_list[["styr"]] <- ifelse(
    test = min < dat_list[["styr"]],
    yes = min,
    no = dat_list[["styr"]]
  )
  dat_list[["endyr"]] <- ifelse(
    test = max > dat_list[["endyr"]],
    yes = max,
    no = dat_list[["endyr"]]
  )

  #### Insert dummy catch data
  dat_list[["catch"]] <- newvals[
    order(newvals[["fleet"]], newvals[["year"]], newvals[["seas"]]),
  ]

  return(dat_list)
}
