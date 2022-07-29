#' Check and standardize list components of sampling functions
#'
#' @param fleets Fleet numbers as a vector.
#' @param years Number of years as a list.
#'   The number of list components should be one or the same length as fleets.
#'   Within the list components
#'   should be a vector of years to correspond with each fleet.
#' @param other_input Some other input to interpret.
#'   The number of list components should be one or the same length as fleets.
#'   Within the list components
#'   should be a vector of length 1 the same length as the vectors within years.
#' @param return_val If `other_input`, return the manipulated other_input value;
#'   if `years`, return the manipulated year input.
#'   If "both" return both as list components.
#' @param other_input_name Only necessary if `both` is used as the return value.
#'
standardize_sampling_args <- function(fleets,
                                      years,
                                      other_input,
                                      return_val = "other_input",
                                      other_input_name = "other_input") {
  # function input checks
  if (!return_val %in% c("other_input", "years", "both")) {
    stop(
      "The parameter return_val is ", return_val, ", which is not a valid ",
      "option. Please set it as either 'other_input', 'years', or 'both'."
    )
  }
  #  check inputs
  msg <- NULL
  if (!is.atomic(fleets)) msg <- c(msg, "fleets is not a vector.")
  if (!is.list(years)) msg <- c(msg, "'years' is not a list.")
  if (!is.list(other_input)) {
    msg <- c(msg, paste0(
      other_input_name,
      " is not a list."
    ))
  }
  if (!is.null(msg)) {
    stop(
      "Input(s) were not the correct type: ",
      paste0(msg, collapse = " ")
    )
  }
  # manipulate values
  # make sure years and other_inputs has the same number of list components as
  # the number of fleets
  if (length(years) == 1 & length(fleets) > 1) {
    years <- rep(years, length(fleets))
  }
  if (length(other_input) == 1 & length(fleets > 1)) {
    other_input <- rep(other_input, length(fleets))
  }
  # make sure other_inputs has the same length vectors as the number of years
  for (i in seq_len(length(years))) {
    tmp_yrs <- years[[i]]
    tmp_o_i <- other_input[[i]]
    if (length(tmp_o_i) == 1 & length(tmp_yrs) > 1) {
      tmp_o_i <- rep(tmp_o_i, length.out = length(tmp_yrs))
      other_input[[i]] <- tmp_o_i
    }
    if (length(tmp_o_i) != length(tmp_yrs)) {
      stop(other_input_name, " did not have the correct dimensions.")
    }
  }

  if (length(other_input) != length(years)) {
    stop(other_input_name, " did not have the correct dimensions.")
  }
  # return
  if (return_val == "other_input") {
    to_return <- other_input
  } else if (return_val == "years") {
    to_return <- years
  } else if (return_val == "both") {
    to_return <- list(years, other_input)
    names(to_return) <- c("years", other_input_name)
  }
  to_return
}
