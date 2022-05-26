#' Adds or removes catchability parameters from a control file
#'
#' Manipulates the control list to simultaneously add and remove
#' elements related to time series data on trends.
#'
#' @details
#' Catchability, \eqn{q},
#' represents the proportionality constant between
#' data on trends and estimated population abundance.
#' Thus a survey thought to encapsulate the entire population, e.g.,
#' an acoustic survey of the entire area, will have \eqn{q = 1}.
#' In Stock Synthesis, environmental time series are modeled similarly to
#' a survey or catch-per-unit-effort time series and thus will also have
#' a catchability term.
#' Readers interested in the complete range of functionality should see the
#' [catchability section of the Stock Synthesis user manual](https://nmfs-stock-synthesis.github.io/doc/SS330_User_Manual.html#catchability).
#' `change_q()` has limited functionality relative to
#' what is available in Stock Synthesis.
#' For example, `change_q()` cannot add parameters for additional variance.
#' Though it will remove additional variance parameters for
#' fleets that no longer have survey data.
#' Additionally, the float term is not used within {ss3sim} and is set to zero.
#'
#' @param string_add A vector of fleet names and/or integers representing
#'   fleets that need \eqn{q} parameters added to the control file.
#' @param string_remove  A vector of fleet names and/or integers representing
#'   fleets that need \eqn{q} parameters removed from the control file.
#' @template ctl_list
#' @param dat_list Deprecated with {ss3sim} version 1.19.1 because users can
#'   obtain fleet information from `ctl_list`.
#' @param ctl_file_in Deprecated with {ss3sim} version 1.19.1 because users can
#'   pass list as read in by [r4ss::SS_readctl()] rather than specifying the
#'   file name to be read in.
#' @param dat_file_in Deprecated with {ss3sim} version 1.19.1 because users can
#'   pass list as read in by [r4ss::SS_readdat()] rather than specifying the
#'   file name to be read in.
#' @param ctl_file_out Deprecated with {ss3sim} version 1.19.1 because {ss3sim}
#'   uses the returned list internally rather than the saved control file.
#' @param overwrite Deprecated with {ss3sim} version 1.19.1 because the file is
#'   no longer being saved to the disk. So, there is nothing to overwrite.
#' @param verbose Deprecated with {ss3sim} version 1.19.1 because all messages
#'   were removed.
#' @export
#' @return A modified Stock Synthesis control list.
#' @seealso
#' * [check_q()] determines which fleets should removed or added.
#' * [r4ss::SS_readctl()] reads in the control file passed to `ctl_list`.
#' * [find_position()] allows `string_*` to use strings or integers.
#' @author Kelli F. Johnson
#'
#' @examples
#' removedfleet1 <- change_q(string_remove = 1, ctl_list = codomctl)
#' removedfleet2 <- change_q(string_remove = 2, ctl_list = codomctl)
#' removedfleets <- change_q(
#'   string_remove = c("Fishery", 2),
#'   ctl_list = codomctl
#' )
#' testthat::expect_null(removedfleets[["Q_options"]])
#' newctl <- codomctl
#' newctl[["fleetnames"]] <- c(newctl[["fleetnames"]], "testfleet")
#' newctl[["Nfleets"]] <- length(newctl[["fleetnames"]])
#' newctl <- change_q(string_add = "testfleet", ctl_list = newctl)
#' testthat::expect_equal(newctl[["Q_options"]][, "fleet"], 1:3)
change_q <- function(string_add = NULL,
                     string_remove = NULL,
                     ctl_list,
                     dat_list = lifecycle::deprecated(),
                     ctl_file_in = lifecycle::deprecated(),
                     dat_file_in = lifecycle::deprecated(),
                     ctl_file_out = lifecycle::deprecated(),
                     overwrite = lifecycle::deprecated(),
                     verbose = lifecycle::deprecated()) {
  if (lifecycle::is_present(dat_list)) {
    lifecycle::deprecate_warn(
      when = "1.19.1",
      what = "ss3sim::change_q(dat_list = )"
    )
  }
  if (lifecycle::is_present(ctl_file_in)) {
    lifecycle::deprecate_warn(
      when = "1.19.1",
      what = "ss3sim::change_q(ctl_file_in = )"
    )
  }
  if (lifecycle::is_present(dat_file_in)) {
    lifecycle::deprecate_warn(
      when = "1.19.1",
      what = "ss3sim::change_q(dat_file_in = )"
    )
  }
  if (lifecycle::is_present(ctl_file_out)) {
    lifecycle::deprecate_warn(
      when = "1.19.1",
      what = "ss3sim::change_q(ctl_file_out = )"
    )
  }
  if (lifecycle::is_present(overwrite)) {
    lifecycle::deprecate_warn(
      when = "1.19.1",
      what = "ss3sim::change_q(overwrite = )"
    )
  }
  if (lifecycle::is_present(verbose)) {
    lifecycle::deprecate_warn(
      when = "1.19.1",
      what = "ss3sim::change_q(verbose = )"
    )
  }

  if (is.null(string_add) & is.null(string_remove)) {
    return(ctl_list)
  }

  # Finds which fleet numbers to add and remove
  add <- find_position(string_add, ctl_list[["fleetnames"]])
  remove <- find_position(string_remove, ctl_list[["fleetnames"]])

  # Removes q from Q_options and Q_parms
  if (!is.null(string_remove) & length(remove) > 0) {
    ctl_list[["Q_options"]] <- ctl_list[["Q_options"]][
      !ctl_list[["Q_options"]][, "fleet"] %in% remove,
    ]
    if (NROW(ctl_list[["Q_options"]]) == 0) {
      ctl_list[["Q_options"]] <- NULL
      ctl_list[["Q_parms"]] <- NULL
    } else {
      ctl_list[["Q_parms"]] <- ctl_list[["Q_parms"]][
        !grepl(
          paste0("\\(", remove, "\\)$", collapse = "|"),
          row.names(ctl_list[["Q_parms"]])
        ),
      ]
    }
  }

  # Adds q to Q_options and Q_parms
  if (!is.null(string_add) & length(add) > 0) {
    # todo: add ability to estimate extra SE
    ctl_list[["Q_options"]] <- rbind(
      data.frame(
        "fleet" = add,
        "link" = 1,
        "link_info" = 1,
        "extra_se" = 0,
        "biasadj" = 0,
        "float" = 1
      ),
      ctl_list[["Q_options"]]
    )
    ctl_list[["Q_options"]] <- ctl_list[["Q_options"]][
      order(ctl_list[["Q_options"]][, "fleet"]),
    ]
    row.names(ctl_list[["Q_options"]]) <- ctl_list[["fleetnames"]][
      ctl_list[["Q_options"]][, "fleet"]
    ]
    q2add <- data.frame(
      "LO" = rep(-20, ctl_list[["Nfleets"]]),
      "HI" = 20,
      "INIT" = 0.0,
      "PRIOR" = 0,
      "PR_SD" = 99,
      "PR_type" = 0,
      "PHASE" = -5,
      "env_var&link" = 0,
      "dev_link" = 0,
      "dev_minyr" = 0,
      "dev_maxyr" = 0,
      "dev_PH" = 0,
      "Block" = 0,
      "Block_Fxn" = 0,
      check.names = FALSE
    )[add, ]
    row.names(q2add) <- paste0(
      "LnQ_base_",
      ctl_list[["fleetnames"]][add],
      "(", add, ")"
    )
    ctl_list[["Q_parms"]] <- rbind(q2add, ctl_list[["Q_parms"]])
    ctl_list[["Q_parms"]] <- ctl_list[["Q_parms"]][order(
      utils::type.convert(
        gsub("a-zA-Z_\\(\\)", "", row.names(ctl_list[["Q_parms"]])),
        as.is = TRUE
      )
    ), ]
  }

  return(invisible(ctl_list))
}

#' Check if desired \eqn{q} parameters exist in control file list
#'
#' Check a Stock Synthesis control file to determine if the desired fleets
#' have q parameters set up.
#'
#' @template ctl_list
#' @param Nfleets Deprecated with {ss3sim} version 1.19.1 because
#'   the number of fleets is available in `ctl_list`.
#' @param desiredfleets A numeric vector specifying which fleets should have
#' catchability parameters.
#'
#' @export
#' @return A list with two vectors, `add` and `remove`,
#' specifying which fleets to add and which to remove from the control file.
#' @seealso [change_q()] for actually adding or removing the fleets.
#' @examples
#' # Keep just the fishery
#' stopifnot(check_q(ctl_list = codomctl, desiredfleets = 1)[["remove"]] == 2)
#' # All elements of the returned list should be NULL
#' # because the model only has two \eqn{q} parameters
#' stopifnot(all(mapply(is.null, check_q(codomctl, desiredfleets = 1:2))))
#' # Fleet 3 is not present
#' stopifnot(check_q(codomctl, desiredfleets = 1:3)[["add"]] == 3)
#' stopifnot(check_q(codomctl, desiredfleets = 2:3)[["remove"]] == 1)
check_q <- function(ctl_list,
                    Nfleets = lifecycle::deprecated(),
                    desiredfleets) {
  if (lifecycle::is_present(Nfleets)) {
    lifecycle::deprecate_warn(
      when = "1.19.1",
      what = "ss3sim::change_q(Nfleets = )"
    )
  }
  # figure out which need to be removed
  remove <- ctl_list[["Q_options"]][
    !(ctl_list[["Q_options"]][, "fleet"] %in% desiredfleets),
    "fleet"
  ]
  add <- desiredfleets[!(desiredfleets %in% ctl_list[["Q_options"]][, "fleet"])]
  if (length(add) == 0) add <- NULL
  if (length(remove) == 0) remove <- NULL
  return(list("add" = add, "remove" = remove))
}
