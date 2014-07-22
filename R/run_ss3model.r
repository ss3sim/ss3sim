#' Run an operating or estimation model for a specified set of scenario IDs
#'
#' This function takes care of calling SS3. Importantly, it parses whether the
#' user is on Unix or Windows and calls the binary correctly. This lower-level
#' function is meant to be called by higher level functions such as
#' \code{\link{run_ss3sim}}, \code{\link{ss3sim_base}}, or your own custom
#' function.
#'
#' @param scenarios Which scenarios to run. Controls which folder contains the
#'   model that SS3 should run on.
#' @param iterations Which iterations to run. Controls which folder contains
#'   the model that SS3 should run on.
#' @param type Are you running the operating or estimation models?
#' @param ss3path The path to your SS3 binary the binary is not in your path.
#'   For example, if \code{SS3} was in the folder \code{/usr/bin/} then
#'   \code{ss3path = "/usr/bin/"}. Make sure to append a slash to the end of
#'   this path. Defaults to \code{NULL}, which means the function will assume
#'   the binary is already in your path. See the vignette for details.
#'   \code{vignette("ss3sim-vignette")}
#' @param hess Calculate the Hessian on estimation model runs?
#' @param admb_options Any additional options to pass to the SS3 command.
#' @param ignore.stdout Passed to \code{system}. If \code{TRUE} then ADMB
#'   output is not printed on screen. This will be slightly faster. Set to
#'   \code{FALSE} to help with debugging.
#' @param admb_pause A length of time (in seconds) to pause after running the
#'   simulation model. This can be necessary on certain computers where file
#'   writing can be slightly delayed. For example, on computers where the files
#'   are written over a network connection. If the output files haven't
#'   finished writing before \R starts looking for the output then the
#'   simulation will crash with an error about missing files. The default
#'   value is set to \code{0.01} seconds, just to be safe.
#' @param ss3_mode Which version of the SS3 executable should be run?
#'   \code{"safe"} or \code{"optimized"}? Safe mode is useful for model building
#'   and testing. Optimized will be slightly faster for running simulations.
#'   Default is safe mode.
#' @param ... Anything else to pass to \code{\link[base]{system}}.
#' @seealso \code{\link{ss3sim_base}}, \code{\link{run_ss3sim}}
#' @author Sean C. Anderson
#' @export

run_ss3model <- function(scenarios, iterations, type = c("om", "em"),
  ss3path = NULL, admb_options = "", hess = FALSE, ignore.stdout =
  TRUE, admb_pause = 0.05, ss_mode = c("safe", "optimized"), ...) {

  ## input checking:
  admb_options <- sanitize_admb_options(admb_options, "-nohess")
  admb_options <- sanitize_admb_options(admb_options, "-noest")
  ss_mode <- ss_mode[1]
  if(!ss_mode %in% c("safe", "optimized")) {
    warning(paste("ss_mode must be one of safe or optimized.",
        "Defaulting to safe mode"))
    ss_mode <- "safe"
  }
  if(ss_mode == "optimized") ss_mode <- "opt"

  os <- .Platform$OS.type
  ss_bin <- paste0("ssv3_24o_", ss_mode)

  if(is.null(ss3path)) ss3path <- ""

  ss_em_options <- ifelse(hess, "", "-nohess")

  for(sc in scenarios) {
    for(it in iterations) {
      message(paste0("Running ", toupper(type), " for scenario: ", sc,
        "; iteration: ", it))
            if(os == "unix") {
        system(paste0("cd ", pastef(sc, it, type), ";", ss3path, paste0(ss_bin, " "),
           ss_em_options, " ", admb_options), ignore.stdout = ignore.stdout, ...)
        rename_ss3_files(path = pastef(sc, it, type), ss_bin = ss_bin,
          extensions = c("par", "rep", "log", "bar"))
      } else {
        wd <- getwd()
        setwd(pastef(sc, it, type))
        system(paste0(ss3path, paste0(ss_bin, " "), ss_em_options, admb_options),
          invisible = FALSE, ignore.stdout = FALSE, ...)
        rename_ss3_files(path = "", ss_bin = ss_bin,
          extensions = c("par", "rep", "log", "bar"))
        setwd(wd)
      }
    }
  }
  Sys.sleep(admb_pause)
}

#' Rename SS3-version-specific files
#'
#' @param path The path to the folder with the files.
#' @param ss_bin A character value giving the SS binary name
#' @param extensions A character vector of file extensions to rename without
#'   periods preceding the values.
#' @author Sean C. Anderson
rename_ss3_files <- function(path, ss_bin, extensions) {
  for(i in seq_along(extensions)) {
    file.rename(from = paste0(path, "/", ss_bin, ".", extensions[i]),
                to   = paste0(path, "/", "ss3",  ".", extensions[i]))
  }
}

#' Check admb options to make sure there aren't flags there shouldn't
#' be
#'
#' @param x The admb options
#' @param exclude A character object (not a vector)
#' @author Sean C. Anderson
sanitize_admb_options <- function(x, exclude = "-nohess") {
  if(length(x) > 1) stop("x should be of length 1")
  if(length(exclude) > 1) stop("exclude should be of length 1")

  x_split <- strsplit(x, " ")[[1]]
  x_split_g <- grep(exclude, x_split)
  if(sum(x_split_g) > 0) {
    warning(paste("Removed admb_option", x_split[x_split_g]))
    x_split_clean <- x_split[-x_split_g]
  } else {
    x_split_clean <- x_split
  }
  paste(x_split_clean, collapse = " ")
}
