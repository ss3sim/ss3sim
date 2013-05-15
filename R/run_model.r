#' Run the operating models for a specified set of scenario IDs
#' @param scenarios Which scenarios to run.
#' @param iterations Which iterations to run.
#' @param type Are you running the operating or estimation models?
#' @export
#' @author Sean Anderson

run_model <- function(scenarios, iterations, type = c("om", "em")) {

  os <- .Platform$OS.type 

  ss_options <- switch(type, om = "-nohess", em = "")

  for(sc in scenarios) {
    for(it in iterations) {
      #print(paste0("Running ", type, " for scenario: ", sc, ";
      #iteration: ", it)) 
      if(os == "unix") {
        system(paste("cd", pastef(sc, it, type), ";SS3", ss_options)),
      } else {
        warning("Windows command not tested yet")
        shell(paste("cd", pastef(sc, it, type), "&ss3", ss_options),
          invisible = TRUE) 
      }
    }
  }

}

