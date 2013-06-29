#' Run the operating models for a specified set of scenario IDs
#'
#' This function takes care of calling SS3. Importantly, it parses
#' whether the user is on Unix or Windows and calls the binary
#' correctly. This lower-level function is meant to be called by
#' higher level functions such as \code{\link{run_ss3sim}} and
#' \code{\link{run_fish600}}.
#'
#' @param scenarios Which scenarios to run. Controls which folder
#' contains the model that SS3 should run on.
#' @param iterations Which iterations to run. Controls which folder
#' contains the model that SS3 should run on.
#' @param type Are you running the operating or estimation models?
#' @param ss3path The path to your SS3 binary the binary is not in
#' your path. For example, if \code{SS3} was in the folder
#' \code{/usr/bin/} then \code{ss3path = "/usr/bin/"}. Make sure to
#' append a slash to the end of this path. Defaults to \code{NULL},
#' which means the function will assume the binary is already in your
#' path.
#' @param hess Calculate the Hessian on estimation model runs?
#' @param admb_options Any additional options to pass to the SS3 command.
#' @param ignore.stdout Passed to \code{system}. If \code{TRUE} then
#' ADMB output is not printed on screen. This will be slightly faster.
#' Set to \code{FALSE} to help with debugging.
#' @param ... Anything else to pass to \code{system}.
#' @seealso \code{\link{run_ss3sim}}, \code{\link{run_fish600}}
#' @author Sean C. Anderson
#' @export

run_ss3model <- function(scenarios, iterations, type = c("om", "em"),
  ss3path = NULL, admb_options = "", hess = FALSE, ignore.stdout = TRUE, ...) {

  ## input checking:
  admb_options <- sanitize_admb_options(admb_options, "-nohess")
  admb_options <- sanitize_admb_options(admb_options, "-noest")
    
  os <- .Platform$OS.type 

  if(is.null(ss3path)) ss3path <- ""

  #ss_options <- switch(type, om = "-nohess", em = "")

  ss_em_options <- ifelse(hess, "", "-nohess")

  for(sc in scenarios) {
    for(it in iterations) {
      #print(paste0("Running ", type, " for scenario: ", sc, ";
      #iteration: ", it)) 
      if(os == "unix") {
        system(paste0("cd ", pastef(sc, it, type), ";", ss3path, "SS3 ", 
           ss_em_options, " ", admb_options), ignore.stdout = ignore.stdout, ...)
      } else {
        wd <- getwd()
        setwd(pastef(sc, it, type))
        system(paste0(ss3path, "SS3 ", ss_em_options, admb_options),
          invisible = TRUE, ignore.stdout = ignore.stdout, ...)
        setwd(wd)
      }
    }
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
