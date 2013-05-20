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
#' @param ... Any additional options to pass the SS3 command.
#' @seealso \code{\link{run_ss3sim}}, \code{\link{run_fish600}}
#' @author Sean Anderson
#' @export

run_ss3model <- function(scenarios, iterations, type = c("om", "em"), ss3path = NULL, ...) {

  os <- .Platform$OS.type 

  if(is.null(ss3path)) ss3path <- ""

  ss_options <- switch(type, om = "-nohess", em = "")

  for(sc in scenarios) {
    for(it in iterations) {
      #print(paste0("Running ", type, " for scenario: ", sc, ";
      #iteration: ", it)) 
      if(os == "unix") {
        system(paste0("cd ", pastef(sc, it, type), ";", ss3path, "SS3 ", ss_options, ...))
      } else {
        wd <- getwd()
        setwd(pastef(sc, it, type))
        #warning("Windows command not tested yet")
        system(paste0(ss3path, "SS3 ", ss_options, ...)), invisible = TRUE) 
        setwd(wd)
      }
    }
  }

}

