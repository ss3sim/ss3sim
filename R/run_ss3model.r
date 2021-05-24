#' Run an operating or estimation model in a directory
#'
#' Call Stock Synthesis (SS) to generate data or fit model to data.
#' The appropriate version of SS is called after determining if the
#' user is on Unix or Windows. This lower-level
#' function is meant to be called by higher level functions such as
#' \code{\link{run_ss3sim}}, \code{\link{ss3sim_base}}, or your own custom
#' function.
#'
#' @details ss3sim requires you to place the SS executable in your
#' path. See the vignette \code{vignette("introduction", package = "ss3sim")} for details on
#' this process. The executables themselves can be downloaded from github
#' \url{https://github.com/ss3sim/ss3sim/tree/master/inst/bin} or the
#' SS website
#' \url{https://vlab.noaa.gov/web/stock-synthesis/document-library/-/document_library/0LmuycloZeIt/view/5042555}.
#' If the github version of ss3sim is installed, rather than the CRAN version,
#' then the executables are automatically downloaded and called from their
#' stored location rather than from the path variable.
#' @template dir
#' @param hess Calculate the Hessian on estimation model runs?
#' @param admb_options Any additional options to pass to the SS command.
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
#' @param show.output.on.console Logical: passed on to
#'   \code{system}.
#' @param ... Anything else to pass to \code{system}.
#' @seealso \code{\link{ss3sim_base}}, \code{\link{run_ss3sim}}
#' @author Sean C. Anderson
#' @export

run_ss3model <- function(dir,
  admb_options = "", hess = FALSE,
  ignore.stdout = TRUE, admb_pause = 0.05,
  show.output.on.console = FALSE, ...) {

  # Input checking:
  admb_options <- sanitize_admb_options(admb_options, "-nohess")
  admb_options <- sanitize_admb_options(admb_options, "-noest")

  os <- .Platform$OS.type
  ss_bin <- "ss"

  bin <- get_bin(ss_bin)

  ss_em_options <- ifelse(hess, "", "-nohess")

  message("Running SS in ", dir)
  if(os == "unix") {
    system(paste0("cd ", dir, ";", paste0(bin, " "),
       ss_em_options, " ", admb_options), ignore.stdout = ignore.stdout, ...)
    rename_ss3_files(path = dir, ss_bin = ss_bin,
      extensions = c("par", "rep", "log", "bar", "cor"))
  } else {
    wd <- getwd()
    on.exit(setwd(wd), add = TRUE)
    setwd(dir)
    system(paste(bin, ss_em_options, admb_options),
      invisible = TRUE, ignore.stdout = ignore.stdout,
           show.output.on.console = show.output.on.console, ...)
    rename_ss3_files(path = ".", ss_bin = ss_bin,
      extensions = c("par", "rep", "log", "bar", "cor"))
  }
  Sys.sleep(admb_pause)
}

#' Rename SS3-version-specific files
#'
#' @details
#' Renaming the files from the name of the exectuable that was used plus the
#' relevant extension to ss3 plus the relevant extension allows users to use
#' any executable, such as ss_safe.exe. Renaming, particularly of the files
#' in the OM folder allows for other functions to expect ss3.par instead of
#' random.par. This consistency is not as relevant now that we are using the
#' control file instead of the par file for most of the parameter manipulation.
#'
#' @param path The path to the folder with the files.
#' @param ss_bin A character value giving the SS binary name
#' @param extensions A character vector of file extensions to rename without
#'   periods preceding the values.
#' @author Sean C. Anderson
rename_ss3_files <- function(path, ss_bin, extensions) {
  for(i in seq_along(extensions)) {
    if (!file.exists(file.path(path, paste0(ss_bin, ".", extensions[i])))) next()
    file.rename(from = file.path(path, paste0(ss_bin, ".", extensions[i])),
                to   = file.path(path, paste0("ss3",  ".", extensions[i])))
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
