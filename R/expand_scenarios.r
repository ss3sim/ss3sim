#' Create vectors of scenario IDs
#'
#' Create vectors of scenarios from inputs. For passing to
#' \code{\link{run_ss3sim}}, or \code{\link{get_results_all}}. Default
#' case values are the base case (\code{0}).
#'
#' @param cases A named list of cases. The names in the list are the
#' case IDs and the values are the case values.
#' @param species Vector of 3-letter character IDs designating the
#' species/stock.
#' @author Cole Monnahan and Sean Anderson
#' @export
#' @seealso \code{\link{run_ss3sim}}, \code{\link{get_results_all}}
#' @return A character vector of scenario IDs. The case IDs will be
#' alphabetically sorted.
#' @examples
#' expand_scenarios()
#' expand_scenarios(cases = list(D = 0:3, E = 0, F = 0, M = 0, R = 0),
#' species = "cod")

expand_scenarios <- function(cases = list(D = 0, E = 0, F = 0,
    M = 0, R = 0), species = c("cod", "fla", "sar")) {

  cases <- cases[order(names(cases))]
  ## sapply(species, function(x) if(nchar(x) != 3)
  ##   stop("species ID must be 3 characters"))

  cases_all <- c(cases, list(species))
  case_names <- names(cases_all)
  cases_paste <- list()
  for(i in 1:length(cases_all)) {
    cases_paste[[i]] <- paste0(case_names[i], cases_all[[i]])
  }
  df <- expand.grid(cases_paste, stringsAsFactors = FALSE)
  scenarios <- apply(df, 1, paste, collapse = "-")
  return(scenarios)
}
