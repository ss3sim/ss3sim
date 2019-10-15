#' Methods to include parameters in an SS3operating model
#'
#' \code{change_o} takes an SS3 \code{.ctl}file
#' and implements parameter value changes that are NOT time varying.
#' \code{change_o} is specifically set up to work with an operating model
#' \code{.ctl} file.
#'
#' @param change_o_list *A list of named vectors. Names correspond to parameters
#'   in the operating model and the vectors correspond to deviations.
#'   Alternatively, \code{par_name} and \code{par_init} can be passed to this
#'   function.
#' @template ctl_file_in
#' @template ctl_file_out
#' @param par_name *A character vector of parameter names to pass in. NULL unless
#'   want to use instead of \code{change_o_list}.
#' @param par_int *A numeric vector of p arameter initial values to pass in. NULL
#'   unless want to use instead of \code{change_o_list}. Must have the same
#'   length and be in the sampe order as \code{par_names}, as the names should
#'   correspond with their initial values.
#' @template verbose
#' @author Kathryn Doering
#' @family change functions
#' @return The function creates modified versions of the  \code{.ctl} files. The
#'   function also returns \code{change_o_list} invisibly.
#' @template casefile-footnote
#'
#' @section Specifying the \code{change_o_list}:
#'
#' Parameters initial values will change according to the values passed to
#' \code{change_o_list}. Each parameter should have a single value specified.
#'
#' Parameter names must be unique and match the full parameter name in the
#' \code{.ctl} file.
#'
#' @section Passing arguments to \code{change_o} through \code{\link{run_ss3sim}}:
#' (1) create a case file with an arbitrary letter
#' not used elsewhere (anything but D, E, F, or R) and (2) include the line
#' \code{function_type; change_o} in your case file. For example, you might
#' want to use M for natural mortality, S for selectivity, or G for growth.
#'
#' @importFrom r4ss SS_parlines SS_changepars
#'
#' @export


change_o <- function(change_o_list,
                     ctl_file_in = "control.ss_new", ctl_file_out = "om.ctl",
                     par_name = NULL, par_int = NULL, verbose = FALSE) {
  # make par_name and par_int into change_o_list, if passing variable that way.
  if ((!is.null(par_name)) & (!is.null(par_int))){
    if(length(par_name) != length(par_int)){
      stop("par_name and par_int did not have the same length. Par_name had value(s): ",
           paste0(par_name, collapse = ", "), ", while par_int had value(s): ",
           paste0(par_int, collapse = ", "), ".")
    }
    # make into a list
    if(verbose) {
      message("Using par_name and par_int instead of change_o_list to pass ",
            " values into function change_o")
    }
    change_o_list <- lapply(par_int, function(x) x)
    names(change_o_list) <- par_name
  } else if (!(is.null(par_name) & is.null(par_int))){ # check for valid values
    stop("Please make sure if you want to pass in arguments to change_o via ",
         "par_name and par_int that BOTH have values. Par_name had value(s): ",
         paste0(par_name, collapse = ", "), ", while par_int had value(s): ",
         paste0(par_int, collapse = ", "), ".")
  }

  # read in necessary ss files.
  ss3.ctl.parlines <- SS_parlines(ctl_file_in, verbose = FALSE)
  # check for valid input ------------------------------------------------------
  # check that the variables can all be found and warn user if not
  if(any(!(names(change_o_list) %in% ss3.ctl.parlines$Label))) {
    tmp_names <- names(change_o_list)
    tmp_names <- tmp_names[-which(tmp_names %in% ss3.ctl.parlines)]
    stop("The variables", paste(tmp_names, collapse = ", "), ", which the user",
    " requested to modify in the OM could not be found in the OM control file.",
    " Please check that this variable is in the OM control file.")
  }
  # make sure only 1 value for each variable
  if(any(lapply(change_o_list, function(x) length(x)) != 1)) {
    tmp_pos <- which(lapply(change_o_list, function(x) length(x)) != 1)
    stop("The function change_o should only be used for changing single ",
         "initial values in the OM control file. However, multiple values for ",
         "the parameter", names(change_o_list)[tmp_pos], "were passed. If you ",
         "wish to implement any parameter as time varying, please use the ",
         "function change_tv instead and note that the input should be ",
         "additive with the value already specified.")
  }
  # change the intial values ---------------------------------------------------
  #make into a named vector
  change_o_vec <- vapply(change_o_list, function(x) x[[1]], FUN.VALUE = 0.1 )
  # pass to SS_changepars
  SS_changepars(dir = NULL, ctlfile = ctl_file_in,
                newctlfile = ctl_file_out, strings = names(change_o_vec),
                newvals = change_o_vec, verbose = FALSE)
  # TODO: the way SS_changepars is written means it is necessary to write to file.
  # May need to explore ways so a file does not need to be written, but a ctl r
  # object can be passed back with the changes.
  invisible(change_o_list)
}
