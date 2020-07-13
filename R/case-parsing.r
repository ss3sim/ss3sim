#' Take a csv file, read it, and turn the first column into the list
#' names and the second column into the list values.
#' @param file The file name as character
get_args <- function(file) {

  x <- read.csv(file, stringsAsFactors = FALSE, col.names =
      c("arg", "val"), header = FALSE, strip.white = TRUE, sep = ";",
    comment.char = "#", quote = "")
  y <- as.list(x$val)
  names(y) <- x$arg

  # turn into correct class:
  lapply(y, function(z) {
    if(!is.na(z)) {
      y <- tryCatch(eval(parse(text = z)),
        error = function(e) as.character(z))
      if(is.function(y)) { # or function contents will be parsed!
        as.character(z)
      } else {
        y
      }
    } else {
      NA
    }
  })
}

#' Take a scenario ID and a case type and return the case number
#'
#' @param scenario A character object with the cases. E.g.
#'   \code{"M1-F1-D1-R1"}
#' @param case The case you want to extract. E.g. \code{"M"}
# @examples
# get_caseval("M2-F1-D1-R1", "M")
# get_caseval("M2-F1-D1-R1", "F")
get_caseval <- function(scenario, case) {
  if(!grepl("-", scenario))
    stop("Your case string doesn't contain your delimiter.")
  if(!is.character(scenario))
    stop("cases must be of class character")
  if(!is.character(case))
    stop("case must be of class character")
  x <- strsplit(scenario, "-")[[1]]
  # get the case number that is up to 9 digits long:
  as.numeric(substr(x[grep(case, x)], 2, 9))
}

#' Take a scenario ID and return argument lists
#'
#' This function calls a number of internal functions to go from a unique
#' scenario identifier like \code{"D1-E2-F3-M0-R4-cod"} and read the
#' corresponding input files (e.g. \code{"M0-cod.txt"}) that have two columns:
#' the first column contains the argument names and the second column contains
#' the argument values. The two columns should be separated by a semicolon.
#' The output is then returned in a named list with the intention of passing
#' these to \code{\link{run_ss3sim}} or \code{\link{ss3sim_base}}.
#'
#' @param folder The folder to look for input files in.
#' @param scenario A character object that has the cases separated by the "-"
#'   delimiter. The combination of cases and stock ID is referred to as a
#'   scenario. E.g. \code{"D0-E0-F0-M0-R0-cod"}. See the Details section.
#' @param ext The file extension of the input files. Defaults to
#'   \code{".txt"}.
#' @param case_files A named list that relates the case IDs (e.g. \code{"D"})
#'   to the files to read the arguments from (e.g. \code{c("index", "lcomp",
#'   "agecomp")}). See the Details section.
#' @return A (nested) named list. The first level of the named list refers to
#' the \code{case_files}. The second level of the named list refers to the
#' argument names (the first column in the input text files). The contents of
#' the list are the argument values themselves (the second column of the input
#' text files).
#'
#' @details Let's start with an example scenario \code{"D0-E1-F0-M0-R0-cod"}.
#' The single capital letters refer to case IDs. The numbers refer to the case
#' numbers. The last block of text (\code{cod}) represents the stock ID (any
#' alphanumeric string of text will work) and is to help the user identify
#' different "stocks" (intended to represent different SS3 model setups).
#'
#' The stock IDs should correspond to how the case files are named and the
#' case IDs should correspond to the cases described by the \code{case_files}.
#' The case file names will correspond to the list values plus the stock ID.
#' For example \code{list(D = c("index", "lcomp", "agecomp"))} combined with
#' the stock ID \code{cod} means that the case \code{D1} will refer to the
#' case files \code{index-cod.txt, lcomp-cod.txt, agecomp-cod.txt}.
#'
#' The case argument plain text files should have arguments in the first
#' column that should be passed on to functions. The names should match
#' exactly. The second column (delimited by a semicolon) should contain the
#' values to be passed to those arguments. Multiple words should be enclosed
#' in quotes.
#'
#' You can use any simple \R syntax to declare argument values. For example:
#' \code{c(1, 2, 4)}, or \code{seq(1, 100)}, or \code{1:100}, or
#' \code{matrix()}, or \code{NULL}. Character objects don't need to be quoted,
#' but can be if you'd like. However, be careful not to use the delimiter (set
#' up as a semicolon) anywhere else in the file besides to denote columns. You
#' can add comments after any \code{#} symbol just like in R.
#'
#' Internally, the functions evaluate in \R any entries that have no
#' character values (e.g. \code{1:100}), or have an alpha-numeric character
#' followed by a \code{(}. Anything that is character only or has character
#' mixed with numeric but doesn't have the regular expression
#' \code{"[A-Za-z0-9]("} gets turned into a character argument. (\code{NA} and
#' \code{NULL} are special cases that are also passed on directly.)
#'
#' @examples
#' # Find the example data folders:
#' case_folder <- system.file("extdata", "eg-cases", package =
#'   "ss3sim")
#'
#' # An example using the cases defined by default:
#' get_caseargs(case_folder, scenario = "D0-F0-cod")
#'
#' # With a custom time-varying case for selectivity, which we'll call
#' # the S case. Here, we'll need to define which file the case S should
#' # read from ("S*-cod.txt"):
#' get_caseargs(case_folder, scenario = "D0-E0-F0-M0-R0-S0-cod",
#'   case_files = list(E = "E", D = c("index", "lcomp", "agecomp"), F =
#'     "F", M = "M", R = "retro", S = "S"))
#' @export

get_caseargs <- function(folder, scenario, ext = ".txt",
  case_files = list(F = "F", D = c("index", "lcomp", "agecomp"))) {
  case_vals <- names(case_files)

  # take the text before the last hyphen:
  spp <- substr(scenario, max(grep("[0-9]",
    strsplit(scenario, NULL)[[1]])) + 2, nchar(scenario))

  # remove the stock ID from the scenario:
  scenario <- substr(scenario, 1, nchar(scenario) - nchar(spp) - 1)

  # Check that all cases are contained in the scenario:
  out_sink <- sapply(case_vals, function(x) {
    if (!grepl(x, scenario))
      stop(paste("Case", x, "isn't contained in scenario", scenario, ".\n"))
  })

  # Check that all scenario-declared cases have files:
  scenario_cases <- gsub("[0-9]*", "", strsplit(scenario, "-")[[1]])
  missing_casefiles <- scenario_cases[!scenario_cases %in% case_vals]
  if(length(missing_casefiles) > 0) {
    stop(paste("The case", missing_casefiles,
      "is declared in your scenario ID but not in the argument case_files.\n"))
  }

  # Add case values to the letters:
  case_vals <- sapply(case_vals, function(x)
    get_caseval(scenario, x))

  args_out <- vector("list", length = length(case_files))
  names(args_out) <- names(case_files)
  for(i in seq_along(case_files)) {
    args_out[[i]] <- paste0(case_files[[i]], case_vals[i], "-", spp, ext)
  }
  args_out2 <- unlist(args_out)
  names(args_out2) <- unlist(case_files)
  argvalues_out <- lapply(args_out2, function(x) get_args(file.path(folder, x)))
  # now, check for all "function_type = change_tv" and concatenate these
  # into a list to pass to change_param()

  change_param_args <- sapply(argvalues_out, function(x) {
    if ("function_type" %in% names(x)) {
      if (x$function_type == "change_tv") {
        y <- list(temporary_name = x$dev)
        names(y) <- x$param
        y
      }}})

  # remove elements that aren't time varying:
  args_null <- sapply(change_param_args, function(x) is.null(x))
  if (!length(which(args_null)) == length(args_null)) { # some are time varying
    change_param_args[which(args_null)] <- NULL
    # and re-arrange to pass to change_params
    change_param_args_short <- lapply(change_param_args, "[[", 1)
    names(change_param_args_short) <- sapply(change_param_args, function(x) names(x))
  } else {
    change_param_args_short <- NULL
  }


  # remove time varying elements from argvalues_out:
  argvalues_out <- argvalues_out[which(args_null)]
  # remove weight_comps values also, b/c don't expect them to match the
  # same function names. Maybe make its own check to check inputs.
  which_not_wt <- lapply(argvalues_out, function(x) {
    if(is.null(x[["function_type"]])) {
      r <- TRUE
    } else if(x[["function_type"]] == "weight_comps"){
      r <- FALSE
    } else {
      r <- TRUE
    }
    r
  })
  which_not_wt <- unlist(which_not_wt)
  weight_comps_params <- argvalues_out[(!which_not_wt)]
  if(length(weight_comps_params) > 1) {
    stop("Multiple cases have function type = 'weight_comps', but there should",
         "only be 1 case with this function type.")
  }
  argvalues_out <- argvalues_out[which_not_wt]


  # test that all specified arguments match function arguments:
  for (i in seq_along(argvalues_out)) {
    change_case_function <- paste0("change_", tolower(names(argvalues_out)[i]))
    sample_case_function <- paste0("sample_", tolower(names(argvalues_out)[i]))
    # special case... legacy effect:
    if (change_case_function == "change_r")
      change_case_function <- "change_retro"

    fxn_name <- change_case_function # might be changed later

    fxn_formals <- tryCatch(names(formals(change_case_function)),
      error = function(e) "")
    if (fxn_formals[1] == "..." | fxn_formals[1] == "") {
      fxn_formals <- names(formals(sample_case_function))
      fxn_name <- sample_case_function
    }
    matches <- names(argvalues_out[[i]]) %in% fxn_formals
    if (sum(matches) != length(matches)) {
      stop(paste(names(argvalues_out[[i]])[!matches],
        "is not an argument in the function ", fxn_name, ".\n"))
    }
  }

  # check weight_comps_params
  if(length(weight_comps_params) == 1) {
    fxn_name <- "weight_comps"
    fxn_formals <- tryCatch(names(formals(fxn_name)),
                            error = function(e) "")
    wc_to_check <- weight_comps_params[[1]][which(names(weight_comps_params[[1]]) != "function_type")]
    matches <- names(wc_to_check) %in% fxn_formals
    if (sum(matches) != length(matches)) {
      stop(paste(names(weight_comps_params[[1]])[!matches],
                 "is not an argument in the function ", fxn_name, ".\n"))
    }
  }
  # and concatenate on the time varying arguments, weight comps arguments
  argvalues_out <- c(argvalues_out, list(tv_params = change_param_args_short))
  argvalues_out <- c(argvalues_out, weight_comps_params)
}

#' return the weight_comps args
#'
#' @param args List of args created by \code{get_caseargs()}.
get_weight_comps_args <- function(args) {
  # same function names. Maybe make its own check to check inputs.
  which_wt <- lapply(args, function(x) {
    if(is.null(x[["function_type"]])) {
      r <- FALSE
    } else if(x[["function_type"]] == "weight_comps"){
      r <- TRUE
    } else {
      r <- FALSE
    }
    r
  })
  which_wt <- unlist(which_wt)
  if(any(which_wt == TRUE)) {
    weight_comps_params <- args[(which_wt)]
    if(length(weight_comps_params) > 1) {
      stop("Multiple cases have function type = 'weight_comps', but there should",
         "only be 1 case with this function type.")
    }
    weight_comps_params <- weight_comps_params[[1]]
  } else {
    weight_comps_params <- NULL
  }
  weight_comps_params
}

#' Substring from right
#'
#' @references
#' http://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r
#' @param x A character object
#' @param n The number of characters from the right to extract
substr_r <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


#' Function to check and standardize list components of sampling functions
#'
#'
#' @param fleets Fleet numbers as a vector
#' @param years Number of years as a list. The number of list components should
#'  be one or the same length as fleets. Within the list components should be
#'  a vector of years to correspond with each fleet.
#' @param other_input Some other input to interpret. The number of list
#'  components should be one or the same length as fleets. Within the list
#'  components should be a vector of length 1 the same length as the vectors
#'  within years.
#' @param return_val If "other_input", return the manipulated other_input value;
#'  if "years", return the manipulated year input. If "both" return both as list
#'  components
#' @param other_input_name Only necessary if "both" is used as the return
#'   value
#'
standardize_sampling_args <- function(fleets, years, other_input, return_val = "other_input", other_input_name = "other_input") {
  # function input checks
  if(!return_val %in% c("other_input", "years","both")) {
    stop("The parameter return_val is ", return_val, ", which is not a valid ",
         "option. Please set it as either 'other_input', 'years', or 'both'.")
  }
  #  check inputs
  msg <- NULL
  if(!is.atomic(fleets)) msg <- c(msg, "fleets is not a vector.")
  if(!is.list(years)) msg <- c(msg, "'years' is not a list.")
  if(!is.list(other_input)) msg <- c(msg, paste0(other_input_name,
                                                 " is not a list."))
  if(!is.null(msg)) {
    stop("Input(s) were not the correct type: ",
         paste0(msg, collapse = " "))
  }
  # manipulate values
  # make sure years and other_inputs has the same number of list components as
  # the number of fleets
  if(length(years) == 1 & length(fleets) > 1) {
    years <- rep(years, length(fleets))
  }
  if(length(other_input) == 1 & length(fleets > 1)) {
    other_input <- rep(other_input, length(fleets))
  }
  # make sure other_inputs has the same length vectors as the number of years
  for(i in seq_len(length(years))) {
    tmp_yrs <- years[[i]]
    tmp_o_i <- other_input[[i]]
    if(length(tmp_o_i) == 1 & length(tmp_yrs) >1) {
      tmp_o_i <- rep(tmp_o_i, length.out = length(tmp_yrs))
      other_input[[i]] <- tmp_o_i
    }
    if(length(tmp_o_i) != length(tmp_yrs)) {
      stop(other_input_name, " did not have the correct dimensions.")
    }
  }

  if(length(other_input) != length(years)) {
    stop(other_input_name, " did not have the correct dimensions.")
  }
  # return
  if(return_val == "other_input") {
    to_return <- other_input
  } else if (return_val == "years") {
    to_return <- years
  } else if (return_val == "both") {
    to_return <- list(years, other_input)
    names(to_return) <- c("years", other_input_name)
  }
  to_return
}
