#' Create vectors of scenarios from inputs. 
#'
#' Create vectors of scenarios from inputs. For passing to
#' \code{\link{run_fish600}} and \code{\link{get_results_all}}.
#' Default case values are the base case (\code{0}).
#'
#' @param e Integer values of estimation cases to be expanded
#' @param d Integer values of data cases to be expanded
#' @param f Integer values of fishing mortality cases to be expanded
#' @param g Integer values of growth cases to be expanded
#' @param m Integer values of natural mortality cases to be expanded
#' @param r Integer values of retrospective cases to be expanded
#' @param s Integer values of selectivity cases to be expanded
#' @param species Vector of characters in \code{c("cod","fla","sar")}
#' @author Cole Monnahan
#' @export
#' @examples
#' expand_scenarios()
#' expand_scenarios(f=0:2, species="cod")

expand_scenarios <- function(e = 0, d = 0, f = 0, g = 0, m = 0, 
  r = 0, s = 0, species = c("cod", "fla", "sar")) {
  e <- paste0("E", e)
  d <- paste0("D", d)
  f <- paste0("F", f)
  g <- paste0("G", g)
  m <- paste0("M", m)
  r <- paste0("R", r)
  s <- paste0("S", s)
  df <- as.data.frame(expand.grid(e, d, f, g, m, r, s,  
    species, stringsAsFactors = FALSE))
  scenarios <- apply(df, 1, paste, collapse = "-")
  return(scenarios)
}
