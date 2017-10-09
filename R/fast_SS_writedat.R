#' Writes files faster than SS_writedat with a small modification from the main 
#' function
#'
#' @author Gwladys Lambert
#' 
#' @note THIS FUNCTION IS NOW REDUNDANT SINCE THE OPTION WAS ADDED TO THE MAIN 
#' FUNCTION IN r4ss !!!! NEED TO CHANGE BACK TO SS_writedat WHEREVER THIS HAS 
#' BEEN USED AND USE THE NEW ARGUMENT: faster = TRUE
#'
#' @param datlist data
#' @param outfile out
#' @param overwrite overwrite
#' @param verbose T or F
#' 
#' @export


fast_SS_writedat <- function (datlist, outfile, overwrite = FALSE, verbose = TRUE) 
{
  
  if (verbose) 
    cat("running SS_writedat\n")
  if (datlist$type != "Stock_Synthesis_data_file") {
    stop("input 'datlist' should be a list with $type=='Stock_Synthesis_data_file'")
  }
  on.exit({
    if (sink.number() > 0) sink()
  })
  if (file.exists(outfile)) {
    if (!overwrite) {
      cat("File exists and input 'overwrite'=FALSE:", outfile, 
          "\n")
      return()
    }
    else {
      file.remove(outfile)
    }
  }
  oldwidth <- options()$width
  oldmax.print <- options()$max.print
  options(width = 5000, max.print = 9999999)
  if (verbose) 
    cat("opening connection to", outfile, "\n")
  zz <- file(outfile, open = "at")
  sink(zz)
  wl <- function(name, comment = NULL) {
    value = datlist[names(datlist) == name]
    if (is.null(comment)) {
      writeLines(paste(value, " #_", name, sep = "", collapse = "_"))
    }
    else {
      writeLines(paste(value, comment))
    }
  }
  printdf <- function(dataframe) {
    names(dataframe)[1] <- paste("#_", names(dataframe)[1], 
                                 sep = "")
    print.data.frame(dataframe, row.names = FALSE, strip.white = TRUE)
  }
  writeLines("#C data file created using the SS_writedat function in the R package r4ss")
  writeLines(paste("#C should work with SS version:", datlist$SSversion))
  writeLines(paste("#C file write time:", Sys.time()))
  writeLines("#")
  wl("styr")
  wl("endyr")
  wl("nseas")
  writeLines(paste(paste(datlist$months_per_seas, collapse = " "), 
                   "#_months_per_seas"))
  wl("spawn_seas")
  wl("Nfleet")
  wl("Nsurveys")
  wl("N_areas")
  writeLines(paste(paste(datlist$fleetnames, collapse = "%"), 
                   "#_fleetnames"))
  writeLines(paste(paste(datlist$fisherytiming, collapse = " "), 
                   paste(datlist$surveytiming, collapse = " "), "#_surveytiming_in_season"))
  writeLines(paste(paste(datlist$areas, collapse = " "), "#_area_assignments_for_each_fishery_and_survey"))
  writeLines(paste(paste(datlist$units_of_catch, collapse = " "), 
                   "#_units of catch:  1=bio; 2=num"))
  writeLines(paste(paste(datlist$se_log_catch, collapse = " "), 
                   "#_se of log(catch) only used for init_eq_catch and for Fmethod 2 and 3"))
  wl("Ngenders")
  wl("Nages")
  writeLines(paste(paste(datlist$init_equil, collapse = " "), 
                   "#_init_equil_catch_for_each_fishery"))
  wl("N_catch", comment = "#_N_lines_of_catch_to_read")
  if (!is.null(datlist$catch)) 
    printdf(datlist$catch)
  wl("N_cpue")
  if (datlist$N_cpue > 0) {
    cat("#_Units:  0=numbers; 1=biomass; 2=F\n")
    cat("#_Errtype:  -1=normal; 0=lognormal; >0=T\n")
    cat("#_Fleet Units Errtype\n")
    printdf(datlist$CPUEinfo)
    printdf(datlist$CPUE)
  }
  wl("N_discard_fleets")
  writeLines("#_discard_units (1=same_as_catchunits(bio/num); 2=fraction; 3=numbers)")
  writeLines("#_discard_errtype:  >0 for DF of T-dist(read CV below); 0 for normal with CV; -1 for normal with se; -2 for lognormal")
  if (!is.null(datlist$discard_fleet_info)) 
    printdf(datlist$discard_fleet_info)
  wl("N_discard")
  if (!is.null(datlist$discard_data)) 
    printdf(datlist$discard_data)
  wl("N_meanbodywt")
  wl("DF_for_meanbodywt", comment = "#_DF_for_meanbodywt_T-distribution_like")
  if (!is.null(datlist$meanbodywt)) 
    printdf(datlist$meanbodywt)
  wl("lbin_method", comment = "# length bin method: 1=use databins; 2=generate from binwidth,min,max below; 3=read vector")
  if (datlist$lbin_method == 2) {
    wl("binwidth", comment = "# binwidth for population size comp")
    wl("minimum_size", comment = "# minimum size in the population (lower edge of first bin and size at age 0.00)")
    wl("maximum_size", comment = "# maximum size in the population (lower edge of last bin)")
  }
  if (datlist$lbin_method == 3) {
    wl("N_lbinspop")
    writeLines("#_lbin_vector_pop")
    writeLines(paste(datlist$lbin_vector_pop, collapse = " "))
  }
  wl("comp_tail_compression")
  wl("add_to_comp")
  wl("max_combined_lbin", comment = "#_combine males into females at or below this bin number")
  wl("N_lbins")
  writeLines("#_lbin_vector")
  writeLines(paste(datlist$lbin_vector, collapse = " "))
  wl("N_lencomp", comment = "#_N_Length_comp_observations")
  if (!is.null(datlist$lencomp)) 
  #  printdf(datlist$lencomp)
  write.table(datlist$lencomp, file=outfile, append=T, col.names=F, row.names=F)
  wl("N_agebins")
  writeLines("#_agebin_vector")
  writeLines(paste(datlist$agebin_vector, collapse = " "))
  wl("N_ageerror_definitions")
  if (!is.null(datlist$ageerror)) 
    printdf(datlist$ageerror)
  wl("N_agecomp")
  wl("Lbin_method", comment = "#_Lbin_method: 1=poplenbins; 2=datalenbins; 3=lengths")
  wl("max_combined_age", comment = "#_combine males into females at or below this bin number")
  if (!is.null(datlist$agecomp)) 
 #   printdf(datlist$agecomp)
  write.table(datlist$agecomp, file=outfile, append=T, col.names=F, row.names=F)
  wl("N_MeanSize_at_Age_obs")
  if (!is.null(datlist$MeanSize_at_Age)) 
    printdf(datlist$MeanSize_at_Age_obs)
  wl("N_environ_variables")
  wl("N_environ_obs")
  if (!is.null(datlist$envdat)) 
    printdf(datlist$envdat)
  if (is.null(datlist$N_sizefreq_methods)) 
    datlist$N_sizefreq_methods <- 0
  wl("N_sizefreq_methods")
  writeLines(paste(paste(datlist$nbins_per_method, collapse = " "), 
                   "#_nbins_per_method"))
  writeLines(paste(paste(datlist$units_per_method, collapse = " "), 
                   "#_units_per_method"))
  writeLines(paste(paste(datlist$scale_per_method, collapse = " "), 
                   "#_scale_per_method"))
  writeLines(paste(paste(datlist$mincomp_per_method, collapse = " "), 
                   "#_mincomp_per_method"))
  writeLines(paste(paste(datlist$Nobs_per_method, collapse = " "), 
                   "#_Nobs_per_method"))
  writeLines("#_Sizefreq bins")
  writeLines("#_sizefreq_bins_list")
  lapply(datlist$sizefreq_bins_list, FUN = function(line) {
    writeLines(paste(line, collapse = " "))
  })
  lapply(datlist$sizefreq_data_list, printdf)
  wl("do_tags")
  if (datlist$do_tags != 0) {
    wl("N_tag_groups")
    wl("N_recap_events")
    wl("mixing_latency_period")
    wl("max_periods")
    if (!is.null(datlist$tag_releases)) 
      printdf(datlist$tag_releases)
    if (!is.null(datlist$tag_recaps)) 
      printdf(datlist$tag_recaps)
  }
  if (is.null(datlist$morphcomp_data)) 
    datlist$morphcomp_data <- 0
  wl("morphcomp_data")
  writeLines("#")
  writeLines("999")
  options(width = oldwidth, max.print = oldmax.print)
  sink()
  close(zz)
  if (verbose) 
    cat("file written to", outfile, "\n")
}
