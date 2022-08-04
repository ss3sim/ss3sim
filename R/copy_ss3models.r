#' Copy the OM or EM into a scenario directory
#'
#' @param model_dir A directory containing an OM or EM.
#' @param iteration An integer specifying the iteration of interest.
#' @param scenario A string giving the scenario name which will be used in the
#'   resulting directory name. If you want this directory created somewhere
#'   other than your current working directory, you can pass a full file path
#'   with the last level being the new scenario name. All intermediate
#'   directories that do not exist will be created.
#' @param type Either `"om"` or `"em"` depending on which type of model folder
#'   needs created.
#'
#' @author Sean C. Anderson, Kelli F. Johnson
#' @export
#'
#' @return
#' An invisible boolean for whether that iteration already existed.
#'
#' @examples
#' # Locate the package data:
#' om_folder <- system.file(
#'   "extdata", "models", "cod-om",
#'   package = "ss3sim"
#' )
#'
#' # Copy the operating model:
#' copy_ss3models(
#'   model_dir = om_folder,
#'   scenario = "D0-F0-testing"
#' )
#' # Now look at your working directory in your file system
#'
#' # Copy the EM
#' copy_ss3models(
#'   model_dir = om_folder,
#'   type = "em",
#'   scenario = "D1-F0-testing"
#' )
#' # Scenario argument affects the folder names.
#'
#' # Clean up:
#' unlink("D0-F0-testing", recursive = TRUE)
#' unlink("D1-F0-testing", recursive = TRUE)
copy_ss3models <- function(model_dir,
                           scenario,
                           iteration = 1,
                           type = c("om", "em")) {
  type <- match.arg(type)
  stopifnot(all(lapply(list(model_dir, scenario, iteration), length) == 1))
  from <- normalizePath(model_dir, mustWork = TRUE)
  to <- file.path(scenario, iteration)
  dir.create(to, showWarnings = FALSE, recursive = TRUE)
  if (file.exists(file.path(to, type))) {
    warning(
      to, " / ", type, " already exists. ",
      "Have you already run this model? Skipping this iteration."
    )
    return(invisible(TRUE))
  } else {
    file.copy(from, to, recursive = TRUE)
    orig_model_folder <- basename(from)
    file.rename(file.path(to, orig_model_folder), file.path(to, type))
    # sanity check and rename for consistency:
    verify_input(model_dir = file.path(to, type), type = type)
    return(invisible(FALSE))
  }
}
