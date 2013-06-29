#' Create vectors of scenarios from inputs. 
#'
#' Create vectors of scenarios from inputs. For passing to
#' \code{\link{run_fish600}} and \code{\link{get_results_all}}.
#' Default case values are the base case (\code{0}).
#'
#' @param e Integer values of cases to be expanded
#' @param f Integer values of cases to be expanded
#' @param m Integer values of cases to be expanded
#' @param d Integer values of cases to be expanded
#' @param r Integer values of cases to be expanded
#' @param s Integer values of cases to be expanded
#' @param g Integer values of cases to be expanded
#' @param species Vector of characters in \code{c("cod","fla","sar")}
#' @author Cole Monnahan
#' @export
#' @examples
#' expand_scenarios()
#' expand_scenarios(f=0:2, species="cod")

expand_scenarios <- function(e = 0, f = 0, m = 0, d = 0, 
  r = 0, s = 0, g = 0, species = c("cod", "fla", "sar")) {
  e <- paste0("E", e)
  f <- paste0("F", f)
  m <- paste0("M", m)
  d <- paste0("D", d)
  r <- paste0("R", r)
  s <- paste0("S", s)
  g <- paste0("G", g)
  df <- as.data.frame(expand.grid(e, f, m, d, r, s, g, 
    species, stringsAsFactors = FALSE))
  scenarios <- apply(df, 1, paste, collapse = "-")
  return(scenarios)
}
