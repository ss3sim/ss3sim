#' Change ageing-error matrix in estimation model
#' 
#' This function alters the fleet-specific ageing-error definition in a
#' Stock Synthesis estimation model (EM). Ageing error can only be modified
#' not changed so be sure to add ageing-error definitions to your OM data file
#' prior to running the simulation. Ageing error can have zero bias and exact
#' precision.
#' 
#' @details
#' The arguments `fleets` and `definition` are both vectors, or a list of vectors
#' that must be of equal length. The order in which the fleets are specified in
#' the `fleets` vector should correspond to the intended new ageing-error 
#' definition in the `definition` argument. Currently this function only allows
#' for fleet specific changes to  ageing-error observations, not year specific.
#' 
#' Users need to input all ageing-error matrices being considered into the 
#' operating model (OM) data file, as this function changes an ageing-error 
#' definition in the age-composition section of the `dat` file and does not
#' allow a new ageing-error matrix to be added.
#' 
#' This function can also be called in [run_ss3sim()] by inputting a vector of
#' ageing-error definitions, corresponding to the ageing-error definition in each 
#' scenario, into a column labeled `ca.definition.1` in the `simdf` data frame. 
#' Where the number in `ca.definition.#` corresponds to the fleet. e.g.- 
#' `ca.definition.1 <- c(3, 2)` will change the aging-error definition, in the EM, 
#' of fleet 1 to 3 in the first scenario and 2 in the second scenario.
#' 
#' Ageing-error definitions can also be changed in the OM in [run_ss3sim()] 
#' by inputting a vector of ageing-error definitions that you want to use for
#' fleet-specific sampling of the data. Each value in the provided vector
#' corresponds to the definition you want to use for a scenario. For example,
#' if the column labeled `sa.ageerr.1` in `simdf` data frame equals 3 and 2,
#' then the ageing-error definitions for fleet 1 will be 3 in the first scenario
# and 2 in the second scenario.
#' 
#' @author Derek W. Chamberlin
#' 
#' @template dat_list
#' @template outfile
#' @param fleets A vector of integers specifying the fleets to include. The
#' order of the fleets corresponds to the order of the `definition` argument.
#' An entry of `fleets=NULL` leads to no ageing-error definitions being 
#' changed in the EM.
#' 
#' @param definition A vector of integers, the same length as `fleets`, 
#' specifying the ageing-error definitions to use for each fleet. The order of the 
#' definitions corresponds to the order of the `fleets` argument. The ageing- 
#' error definitions must already be defined in the operating model (OM) data file.
#' 
#' @family change functions
#' @export
#' @examples
#' d <- system.file("extdata", package = "ss3sim")
#' f_in <- file.path(d, "models", "cod-om", "codOM.dat")
#' dat_list <- r4ss::SS_readdat(f_in, verbose = FALSE)
#' 
#' # Adding in second ageing-error definition to the data file
#' dat_list$N_ageerror_definitions <- 2
#' dat_list$ageerror <- rbind(dat_list$ageerror,dat_list$ageerror)
#' 
#' # An example changing the ageing-error observation for multiple fleets
#' new_dat <- change_em_ageerr(
#'   dat_list = dat_list, outfile = NULL, fleets = c(1, 2), definition = c(2, 2)
#' )
#' 
#' # An example changing the ageing-error observations for a single fleet
#' new_dat2 <- change_em_ageerr(
#'   dat_list = dat_list, outfile = NULL, fleets = 2, definition = 2
#' )
#' 
#' rm(d, f_in, dat_list, new_dat, new_dat2)
change_em_ageerr <- function(dat_list, outfile = NULL, fleets,
definition) {
  if (length(fleets) != length(definition)) {
    stop("'fleets' and 'definition' must be the same length.")
  }
  change_err <- !is.null(definition) && !is.null(fleets)
  if (change_err){
      for(i in seq_along(fleets)){
        # Input checks
        if (!definition[i] %in% seq(dat_list$N_ageerror_definitions)) {
          stop("Supplied ageing error definition is not in your dat file.")
        }
        if (!fleets[i] %in% unique(dat_list$agecomp$fleet)) {
          stop("Supplied fleet is not in your dat file. Ageing error 
          definition cannot be changed.")
        }
        # Lookup fleets to be changed and change ageing-
        # error definition
        dat_list$agecomp$ageerr[dat_list$agecomp$fleet
        %in% fleets[i]] <- definition[i]
      }

  }
  # write the dat file so it can be used by the em
    if (!is.null(outfile)) {
    r4ss::SS_writedat(
      datlist = dat_list,
      outfile = outfile,
      overwrite = TRUE,
      verbose = FALSE
    )
  }
  invisible(dat_list)
}
