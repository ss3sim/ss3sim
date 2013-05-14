#' Run the operating models for a specified set of scenario IDs
#' @param scenarios Which scenarios to run.
#' @param iterations Which iterations to run.
#' @export
#' @author Sean Anderson

run_om <- function(scenarios, iterations) {

ss_call <- switch(.Platform$OS.type, 
  unix = "SS3",
  windows = "SS3.exe"
  )

dir_switch <- switch(.Platform$OS.type, 
  unix = "cd ",
  windows = "I DON'T KNOW"
  )

sep <- switch(.Platform$OS.type, 
  unix = "; ",
  windows = "I DON'T KNOW"
  )

  for(sc in scenarios) {
    for(it in iterations) {
      om <- pastef(sc, it)
      print(paste0("Running scenario: ", sc, "; iteration: ", it)) 
      system2(paste0(dir_switch, pastef(sc, it), sep, ss_call)) 
    }
  }
}

