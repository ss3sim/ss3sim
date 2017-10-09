#' Alter the fishing mortality (\emph{F}) values in an SS3 \code{.par} file.
#'
#' Takes an SS3 \code{.par} file and changes the \emph{F} values for specified years.
#' If used with \code{\link{run_ss3sim}} the case file should be named
#' \code{F}. A suggested (default) case letter is \code{F}.
#'
#' @author Curry James Cunningham
#'
#' @param years *Vector of years for which \emph{F} values are specified,
#' if there is more than one fleet or season the catches must be ordered by
#' season:year:fishey (e.g., season1year1fishery1, season2year1fishery1,
#' season1year2fishery1). The actual vector does not have to correspond to
#' true years but must be the correct length (e.g., instead of
#' \code{2000:2004} you can use \code{1:5}). Use this argument to create
#' an index to old values. \code{years_alter} will use values in this vector.
#' For example, with two seasons and one fishery that operates for 4 years
#' you could use the following: \code{1:8}. \code{years} should be written as a 
#' list if several fishing fleets in the model
#' @param years_alter *Vector of years for the which \emph{F} values will be altered.
#' If there is more than one fishery or season, use the mapping system
#' created in \code{years} because actual year values cannot be recycled.
#' For example, to change the second season of the second year in the
#' example above, use: \code{4}.\code{years_alter} should be written as a 
#' list if several fishing fleets in the model
#' @param fvals *Vector of \emph{F} values to be entered into \code{ss3.par} file,
#' where \code{length(fvals) == length(years_alter)} must be true. 
#' \code{fvals} should be written as a list if several fishing fleets in the model
#' @param fleets Fleets for which to change F if more than one fishing fleet 
#' in the model, default is NULL
#' @template par_file_in
#' @template par_file_out
#' @return A modified SS3 \code{.par} file.
#' @family change functions
#' @template casefile-footnote
#' @examples
#' # Create a temporary folder for the output:
#' temp_path <- file.path(tempdir(), "ss3sim-f-example")
#' dir.create(temp_path, showWarnings = FALSE)
#'
#' # Find the example .par file in the package data:
#' d <- system.file("extdata", package = "ss3sim")
#' par_file <- paste0(d, "/change_f/ss3.par")
#'
#' change_f(years = 1:49, years_alter = 2, fvals = 9999, par_file_in =
#' par_file, par_file_out = paste0(temp_path, "/test.par"))
#' @export

change_f <- function(years, years_alter, fvals, fleets = NULL, par_file_in = "ss3.par", 
  par_file_out = "ss3.par") {

  # legacy code - in ss3sim, single fleet - here we made it multifleet
  if (!is.list(years)) years <- list(years)
  if (!is.list(years_alter)) years_alter <- list(years_alter)
  if (!is.list(fvals)) fvals <- list(fvals)
  
  if (is.null(fleets)) flt = 1:length(years) 
  if (!is.null(fleets)) flt = 1:length(fleets)
  
  move_idx <- 0
  
    # Read in ss3.par file
    ss3.par <- readLines(par_file_in) # Original
    ss3.par.new <- ss3.par # New file
  
  for (idx in flt) {
    
  # renumber years to be altered
  df_new = NULL
  max_yrs <- 0
  
   for (i in 1:length(years[[idx]])) {
     
  df <- cbind.data.frame(years =years[[idx]][i], years_alter=NA, fvals=NA)
  df[df$years %in% years_alter[[idx]], "years_alter"] <- years_alter[[idx]][i]
  df[df$years %in% years_alter[[idx]], "fvals"] <- fvals[[idx]][i]
  df$years <- df$years + max_yrs
  df_new <- rbind(df_new, df)
  }
  
   years_alter[[idx]] <- df_new$years_alter
   years[[idx]] <- df_new$years
   fvals[[idx]] <- df_new$fvals
  # 
  # back to original script
  n.years_alter <- length(years_alter[[idx]])

  # Check that sufficient F values are supplied
  if(n.years_alter != length(fvals[[idx]])) {
    stop(paste('#ERROR: Number of years to alter:', n.years_alter,
        'Does NOT equal length of supplied Fvalues:', length(fvals[[idx]])))
  }

   temp.year <- which(!is.na(years_alter[[idx]])) + move_idx
    temp.loc <- which(ss3.par %in% paste('# F_rate[', temp.year, ']:', sep=''))
    ss3.par.new[temp.loc+1] <- fvals[[idx]][!is.na(fvals[[idx]])]

  move_idx <- move_idx + length(years[[idx]])
 
  }
  
  writeLines(ss3.par.new, con = par_file_out)  
  # Write new .par file
  close(file(par_file_out))
  invisible(ss3.par.new)
}
