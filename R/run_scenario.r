#' Run both the operating model and assessment model
#'
#' This function is a wrapper function that can call \code{run_model} for
#' the operating model, manipulate the output (add recruitment
#' deviations, survey the data, etc.), and run the estimation model.
#'
#' @param scenarios Which scenarios to run.
#' @param iterations Which iterations to run.
#' @param type Are you running the both the operating and estimation
#' models or just one or the other?
#' @author Sean Anderson
#' @export

run_scenario <- function(scenarios, iterations, type = c("om and em", "om", "em")) {
  switch(type,
   "om and em" = {
     run_model(scenarios = scenarios, iterations = iterations, type = "om")
# TODO add data manipulation here
     warning("No manipulation steps implemented yet")
     run_model(scenarios = scenarios, iterations = iterations, type = "em")
   },
   "om" = run_model(scenarios = scenarios, iterations = iterations, type = "om"),
   "em" = run_model(scenarios = scenarios, iterations = iterations, type = "em")
   ) 
}


