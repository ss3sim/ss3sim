#' Change aging error matrix in estimation model age composition
#' 
#' `change_em_ageerr()` alters the fleet specific aging error definition in
#' a stock synthesis estimation model (EM). The original aging error definition
#' in the age composition observation section of the data file is changed 
#' according to the user's specification. 
#' 
#' @details
#' The arguments `fleets` and `definition` are both vectors, or a list of vectors
#' that must be of equal length. The order in which the fleets are specified in
#' the `fleets` vector should correspond to the intended new aging error 
#' definition in the `definition` vector. Currently this function only allows
#' for fleet specific changes to  aging error observations, not year specific.
#' 
#' Users need to input all aging error matrices being considered into the 
#' operating model (OM) data file, as this function changes an aging error 
#' definition in the age composition observation section of the dat file
#' and does not allow a new aging error matrix to be added.
#' 
#' This function can also be called in [run_ss3sim()] by inputting a vector of
#' aging error definitions, corresponding to the aging error definition in each 
#' scenario, into a column labeled `ca.definition.1` in the `simdf` data frame. 
#' Where the number in `ca.definition.#` corresponds to the fleet. e.g.- 
#' `ca.definition.1 <- c(3, 2)` will change the aging error definition, in the EM, 
#' of fleet 1 to 3 in the first scenario and 2 in the second scenario.
#' 
#' Aging error definitions can also be changed in the OM in [run_ss3sim()] 
#' by inputting a vector of aging error definitions, corresponding to the 
#' aging error definition in each scenario, into a column labeled 
#' `sa.Ageerr.1` in the `simdf` data frame. Where the number in `sa.Ageerr.#` 
#' corresponds to the fleet. e.g.- `sa.Ageerr.1 <- c(3, 2)` will change the 
#' aging error definition, in the OM, of fleet 1 to 3 in the first scenario 
#' and 2 in the second scenario.
#' 
#' @author Derek W. Chamberlin
#' 
#' @template dat_list
#' @template outfile
#' @param fleets A vector of integers specifying the fleets to include. The
#' order of the fleets corresponds to the order of the `definition` argument.
#' An entry of `fleets=NULL` leads to no ageing error definitions being 
#' changed in the EM.
#' 
#' @param definition A vector of integers, of the same length as `fleets`, 
#' specifying the aging error definitions to include. The order of the 
#' definitions corresponds to the order of the `fleets` argument. The aging 
#' error definitions must already input in the operating model (OM) data file.
#' 
#' @family change functions
#' @export
#' @examples
#' d <- system.file("extdata", package = "ss3sim")
#' f_in <- file.path(d, "models", "cod-om", "codOM.dat")
#' dat_list <- r4ss::SS_readdat(f_in, verbose = FALSE)
#' 
#' # Adding in second aging error definition to the data file
#' dat_list$N_ageerror_definitions <- 2
#' dat_list$ageerror <- rbind(dat_list$ageerror,dat_list$ageerror)
#' 
#' # An example changing the aging error observation for multiple fleets
#' new_dat <- change_em_ageerr(
#'   dat_list = dat_list, outfile = NULL, fleets = c(1, 2), definition = c(2, 2)
#' )
#' 
#' # An example changing the aging error observations for a single fleet
#' new_dat2 <- change_em_ageerr(
#'   dat_list = dat_list, outfile = NULL, fleets = 2, definition = 2
#' )
#' 
#' rm(d, f_in, dat_list, new_dat, new_dat2)
change_em_ageerr <- function(dat_list, outfile = NULL, fleets,
definition) {
  change_err <- ifelse(is.null(definition), FALSE, TRUE)
  if (change_err){
      for(i in 1:length(fleets)){
        # Input checks
        if (!definition[i] %in% seq(dat_list$N_ageerror_definitions)) {
          stop("Supplied ageing error definition is not in your dat file.")
        }
        if (!fleets[i] %in% unique(dat_list$agecomp$FltSvy)) {
          stop("Supplied fleet is not in your dat file. Ageing error 
          definition cannot be changed.")
        }
        # Lookup fleets to be changed and change aging
        # error deifnition
        dat_list$agecomp$Ageerr[dat_list$agecomp$FltSvy 
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
