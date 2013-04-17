#' Alter the Fvalues in an ss3.par file
#'
#' @author Curry James Cunningham
#' @details Changed to allow for specific years in series to be altered (this may be unnecessary given our purpose)
#'
#' @param years Vector of years for which Fvalues are specified
#' @param years_alter Vector of years for which Fvalue will be altered
#' @param fvals Vector of Fvalues to be entered into ss3.par file
#' @param file_in Input SS3 par file.
#' @param file_out Output SS3 par file. 
#'
#' @export

change_f <- function(years, years_alter, fvals, file_in="ss3.par", file_out="ss3.par") {
	
  n.years_alter <- length(years_alter)
  #Check that sufficient Fvalues are supplied
  if(n.years_alter != length(fvals)) { 
    stop(paste('#ERROR: Number of years to alter:', n.years_alter,
        'Does NOT equal length of supplied Fvalues:', length(fvals))) 
  }
  
  #Read in ss3.par file
  ss3.par <- readLines(file_in) #Original 
  ss3.par.new <- ss3.par #New file
  
  for(y in 1:n.years_alter) {
  	temp.year <- which(years == years_alter[y])
    temp.loc <- which(ss3.par == paste('# F_rate[', temp.year, ']:', sep='')) #Temporary location
    ss3.par.new[temp.loc+1] <- fvals[y]
  }#next y
  
  #Write new .par file
  writeLines(ss3.par.new, con=file_out)
  close(file(file_out))
  invisible(ss3.par.new)
}

#change_f(years=1:49, years_alter=2, fvals=9999, file_in = "test_in.txt", file_out = "test_out.txt")

