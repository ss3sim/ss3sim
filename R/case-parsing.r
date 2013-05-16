#' Take a vector of cases and return the case number
#'
#' @param cases A character object with the cases. E.g.
#' \code{"M1-F1-D1-R1"}
#' @param case The case you want to extract. E.g. \code{"M"}
#' @param delimiter The delimiter between the cases. Defaults to a
#' dash.
#' @examples
#' get_caseval("M1-F1-D1-R1", "M")

get_caseval <- function(cases, case, delimiter = "-") {

  if(!grepl(delimiter, cases)) 
    stop("Your case string doesn't contain your delimiter.")
  if(!is.character(cases))
    stop("cases must be of class character")
  if(!is.character(case))
    stop("case must be of class character")

  x <- strsplit(cases, "-")[[1]]
  as.numeric(substr(cases[grep(case, x)], 2, 2))
}

# Return list of lcomp, agecomp, and index parameters to pass to
# \code{run_ss3sim}

base_index <- function() {

}

base_agecomp <- function() {

}

base_lcomp <- function() {

}

get_m_args <- function(x) {
 #switch(x,
   #1 = {
   #},
   #2 = ,
   #3 = ,
   #4 = )
}

get_f_args <- function() {

}

get_d_args <- function() {

}

get_r_args <- function() {

}

