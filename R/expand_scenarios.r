#' Create vectors of scenario IDs
#'
#' Create vectors of scenarios from case letters, case numbers,
#' and species codes. Scenarios are passed to
#' \code{\link{run_ss3sim}} and \code{\link{get_results_all}}. 
#' Case letters 'D' and 'F' are mandatory and provide the data 
#' sampling and fishing history for the operating model.
#'
#' @param cases A named list of cases. The names in the list are the
#' case IDs and the values are the case values.
#' @param species Vector of 3-letter character IDs designating the
#' species/stock.
#' @author Cole Monnahan and Sean C. Anderson
#' @export
#' @seealso \code{\link{run_ss3sim}}, \code{\link{get_results_all}}
#' @return A character vector of scenario IDs. The case IDs will be
#' alphabetically sorted.
#' @examples
#' expand_scenarios()
#' expand_scenarios(cases = list(D = 0:3, E = 0, F = 0, M = 0, R = 0),
#'   species = "cod")

expand_scenarios <- function(cases = list(D = 0, E = 0, F = 0,
    M = 0, R = 0), species = c("cod", "fla", "sar")) {

  cases <- cases[order(names(cases))]

  cases_all <- c(cases, list(species))
  case_names <- names(cases_all)
  cases_paste <- list()
  for(i in seq_along(cases_all)) {
    cases_paste[[i]] <- paste0(case_names[i], cases_all[[i]])
  }
  df <- expand.grid(cases_paste, stringsAsFactors = FALSE)
  scenarios <- apply(df, 1, paste, collapse = "-")
  return(scenarios)
}
