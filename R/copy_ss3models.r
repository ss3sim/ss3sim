#' Copy the operating and estimation models and create a folder
#' structure
#'
#' @param model_dir A directory containing the operating or estimation
#' model. Each folder should be named according to a scenario ID.
#' @param iterations A numeric vector of the iterations to copy to.
#' The function will create the folders as needed.
#' @param scenarios Which scenarios to copy to. Supply a vector of
#' character elements.
#' @param type Are you copying operating or estimation models? This
#' affects whether the model folder gets named "om" or "em"
#' @author Sean C. Anderson, Kelli F. Johnson
#' @export
#' @return An invisible boolean for whether that iteration already
#' existed. A set of nested folders starting with the scenario ID, then the
#' iterations, then "om" or "em", and then the Stock Synthesis model files.
#'
#' @examples
#' # Locate the package data:
#' om_folder <- system.file("extdata", "models", "cod-om",
#'   package =
#'     "ss3sim"
#' )
#'
#' # Copy the operating model:
#' copy_ss3models(
#'   model_dir = om_folder, type = "om", iterations =
#'     1:3, scenarios = "D0-F0-testing"
#' )
#' # Now look at your working directory in your file system
#'
#' # Copy the estimation model with two scenario IDs:
#' copy_ss3models(
#'   model_dir = om_folder, type = "em", iterations = 1:2,
#'   scenarios = c("D1-F0-testing", "D2-F0-testing")
#' )
#' # (Note that all the scenario argument does here is affect the
#' # folder names.)
#'
#' # Clean up:
#' unlink("D0-F0-testing", recursive = TRUE)
#' unlink("D1-F0-testing", recursive = TRUE)
#' unlink("D2-F0-testing", recursive = TRUE)
copy_ss3models <- function(model_dir, scenarios,
                           iterations = 1:100, type = c("om", "em")) {
  type <- type[1]
  if (!type %in% c("om", "em")) {
    stop("The value passed to the argument type must be either om or em.")
  }
  for (sc in scenarios) {
    for (it in iterations) {
      from <- normalizePath(model_dir, mustWork = TRUE)
      to <- file.path(sc, it)
      dir.create(to, showWarnings = FALSE, recursive = TRUE)
      if (file.exists(file.path(to, type))) {
        warning(paste0(to, "/", type, " already exists. Have you already run this model? Skipping this iteration"))
        iteration_exists <- TRUE
      } else {
        file.copy(from, to, recursive = TRUE)
        orig_model_folder <- basename(from)
        file.rename(file.path(to, orig_model_folder), file.path(to, type))
        ## sanity check and rename for consistency:
        verify_input(model_dir = file.path(to, type), type = type)
        iteration_exists <- FALSE
      }
    }
  }
  return(invisible(iteration_exists))
}
