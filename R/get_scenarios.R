#' Identify scenarios in `directory`
#'
#' Find scenario directories, where it is known if a directory was derived from
#' a simulation and contains iterations of the operating and estimating models
#' if there are directories with numeric names that contain om and/or em
#' directories, i.e., "1", "2", "3", ..., "100".
#' @param directory The directory or vector of directories that you want to
#'   search for scenarios. The search is recursive, and thus, it is in one's
#'   best interest to provide a shorter path name rather than one high up in the
#'   call stack.
#' @param full A logical entry. If `TRUE`, the full path name is returned, which
#'   can be helpful if a vector of directories is supplied to `directory`,
#'   otherwise it is impossible to know which scenarios are located where. If
#'   `FALSE`, a vector of names is returned. This is the default behavior.
#' @return A character vector of names of directories that contain output from
#'   this package. Full paths are only provided if `full = TRUE`.
#' @family get results
#' @author Merrill Rudd
#' @export
get_scenarios <- function(directory = getwd(),
                          full = FALSE) {
  if (is.list(directory)) {
    directory <- unlist(directory)
  }
  paths <- fs::dir_ls(directory, type = "directory", recurse = TRUE)
  scenarios <- paths[grepl("[0-9].[eo]m$", paths)] |>
    gsub(
      pattern = "[0-9]{1,+}/[eo]m$",
      replacement = ""
    ) |>
    unique()

  if (length(scenarios) == 0) {
    cli::cli_alert_danger(
      "No scenarios were found in {directory}"
    )
  } else {
    if (!full) {
      out <- basename(scenarios)
    } else {
      out <- scenarios
    }
    return(out)
  }
}
