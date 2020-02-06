#' Sample conditional age-at-length (CAL) data
#'
#' Samples conditional age-at-length (CAL) data from expected values of length
#' proportions and expected values of age proportions (conditional on length)
#' from the operating model (OM) and writes the samples to file for use by the
#' estimation model (EM).
#'
#' @details This function takes a \code{data.SS_new} file from an OM containing
#'  expected values of length proportions and age proportions, conditional on
#'  length. First, sample from true length proportions, using the length comp
#'  sample sizes, to get realistic sample sizes for age bins given their lengths.
#'  Note that the overall total sample size for all CAL bins is specified by
#'  the user for the given fleet and year in \code{Nsamp_ages}. Next,
#'  use these sample sizes and the expected values of age proportions
#'  (conditional on length) to sample for realistic age proportions. If no fish
#'  are sampled, for a row of age proportions in conditional age at length data,
#'  then that row is discarded. A value of NULL for fleets indicates to delete
#'  the conditional age at length data data (but not the marginal age data). If
#'  the EM is used with \code{\link{run_ss3sim}}, the case file should be named
#'   \code{calcomp}. Only the multinomial distribution is currently implemented,
#'  so this function cannot be used with the dirichlet distribution.
#'  Note that this function cannot handle all types of conditional age at length
#'  sampling. This function requires that there be a row of conditional age at
#'  length data for each length data bin (for each year and fleet that sampling
#'  is specified to be performed), where Lbin_lo and Lbin_hi are the
#'  same value.
#'  Note also that this sampling procedure represents simple random sampling for
#'  conditional age at length, where 1) lengths are sampled randomly, 2) fish
#'  are lengthed and placed into bins, and 3) a subset of lengthed fish are
#'  aged, where a constant proportion from each length bin are selected for
#'  aging. This does NOT represent length stratified sampling where a subset of
#'  lengthed fish are aged, and a constant number from each length bin is
#'  selected for aging, although these data could also be put into a Stock
#'  Synthesis model as "Conditional Age at Length."
#' @note This function is only reliable when using multinomial length
#'  compositions for the fleet(s) with conditional age at length sampling. The
#'  real-valued length compositions resulting from the Dirichlet distribution
#'  cause difficulties in the sampling code. See the vignette for more
#'  information.
#' @author Cole Monnahan, Kotaro Ono
#' @template lcomp-agecomp-index
#' @template dat_list
#' @param exp_vals_list This is a data list containing all expected values. It
#'  should not be modified by previous sampling functions to contain sampled
#'  data.
#' @template outfile
#' @param Nsamp_lengths *A numeric list of the same length as fleets. Either
#'  single values or vectors of the same length as the number of years can be
#'  passed through. Single values are repeated for all years. If no fleet
#'  collected samples, specify \code{Nsamp_lengths = NULL}. Specifically, for
#'  \code{sample_calcomp}, \code{Nsamp_lengths} denotes the total number of
#'  length samples for a given year and fleet across all length bins that can be
#'  used to then sample the conditional age at length samples.
#'  \code{Nsamp_lengths} must be greater than or equal to \code{Nsamp_ages}.
#' @param Nsamp_ages *A numeric list of the same length as fleets. Either single
#'  values or vectors of the same length as the number of years can be passed
#'  through. Single values are repeated for all years. If no fleet collected
#'  samples, specify \code{Nsamp_ages = NULL}. Specifically, for
#'  \code{sample_calcomp}, \code{Nsamp_ages} denotes the total number of
#'  conditional age at length samples for a given year and fleet across all
#'  length bins. \code{Nsamp_ages} must be less than \code{Nsamp_lengths}.
#' @param method The method used to sample ages from the lengths. Options are
#'   "simple_random" and "length_stratified". In "simple_random" (the default
#'   option), the fish aged are randomly sampled from the age bins, so the number
#'   sampled in each age bin is not equal. In "length_stratified", an equal
#'   number of fish are aged from each length bin.
#' @param ESS_lengths  The final effective sample size (ESS) associated with the
#'  simulated length data generated for conditional age at length samples. The
#'  ESS is not used to generate the simulated data but can be used as an input
#'  sample size in subsequent models that estimate population parameters or
#'  status. The default, NULL, leads to the true (internally calculated)
#'  effective sample size being used, which is \code{Nsamp_lengths} for the
#'  multinomial case. \code{ESS_lengths} should be a numeric list of the same
#'  length as fleets. Either single values or vectors of the same length as the
#'  number of years can be passed through. Single values are repeated for all
#'  years. Note that the dimensions of ESS_lengths must be compatible with the
#'  dimensions of \code{Nsample_lengths}.
#' @param ESS_ages The final effective sample size (ESS) associated with the
#'  simulated conditional age at length data. The ESS is not used to generate
#'  the simulated data but can be used as an input sample size in subsequent
#'  models that estimate population parameters or status. The default, NULL,
#'  leads to the true (internally calculated) effective sample size being used,
#'  which is Nsamp_ages for the multinomial case. \code{ESS_ages} should be
#'  a numeric list of the same length as fleets. Either single values or vectors
#'  of the same length as the number of years can be passed through. Single
#'  values are repeated for all years. Note that the dimensions of ESS_lengths
#'  must be compatible with the dimensions of \code{Nsample_ages}. The input
#'  value will be apportioned among the conditional age at length bins as the
#'  \code{Nsamp_ages} is and therefore can be a fractional value.
#' @param lcomps_sampled Have marginal length comps already been sampled and are
#'  included in \code{dat_list[["lencomp"]]}? If FALSE, expected values are in
#'  present in \code{datlist[["lencomp"]]}.
#' @template sampling-return
#' @template casefile-footnote
#' @family sampling functions
#' @export

sample_calcomp <- function(dat_list, exp_vals_list, outfile = NULL, fleets,
                           years,
                           Nsamp_lengths, Nsamp_ages,
                           method = "simple_random", ESS_lengths = NULL,
                           ESS_ages = NULL,
                           lcomps_sampled = FALSE) {

  #TODO: add in length_stratified
    method <- match.arg(arg = method, choices = c("simple_random"))
    ## The samples are taken from the expected values, where the
    ## age-at-length data is in the age matrix but has a -1 for Lbin_lo and
    ## Lbin_hi, so subset those out, but don't delete the age values since
    ## those are already sampled from, or might be sampled later so need to
    ## leave them there.
    ## Input checks
    Nfleets <- NROW(fleets)
    if (Nfleets>0){
        for(i in seq_len(Nfleets)) {
            if(length(Nsamp_ages[[i]])>1 & length(Nsamp_ages[[i]]) != length(years[[i]]))
                stop(paste0("Length of Nsamp_ages does not match length of years for",
                  "fleet ",fleets[i]))
        }
    }
    check_data(dat_list)

    # Get necessary values
    ss_version <- get_ss_ver_dl(dat_list) # to use later in SS_writedat calls
    if(lcomps_sampled == TRUE) {
      lencomp_marginal <- dat_list$lencomp
    } else {
      lencomp_marginal <- NULL
    }
    # Divide up age comp into marginal and CAL; we will only be sampling from
    # cal in this function, but want to retain the marginal age comps.
    agecomp.age <- dat_list$agecomp[dat_list$agecomp$Lbin_lo== -1,] # marginal
    agecomp.cal <- dat_list$agecomp[dat_list$agecomp$Lbin_lo != -1,] # CAL
    lbin_vector <- dat_list$lbin_vector # This is the vector of data length bins
    newfile <- dat_list
    ## A value of NULL for fleets indicates not to sample and strip out the
    ## CAL data from the file.
    if(is.null(fleets)){
        newfile$agecomp <- agecomp.age # only leave in maraginal age comps
        if(!is.null(outfile)){
          SS_writedat(datlist = newfile, outfile = outfile, version = ss_version,
                      overwrite = TRUE, verbose=FALSE)
        }
        return(invisible(newfile))
    }
    ## If not, do additional argument checks
    if(nrow(agecomp.cal)==0) { #TODO: maybe turn this into a warning instead?
        stop("No conditional age-at-length expected values found")
    }
    Nfleets <- length(fleets)
    if(any(!fleets %in% unique(agecomp.cal$FltSvy))){
        stop("A fleet specified in fleets was not found in ",
        "the fleets in age comps for dat_list.")
    }
    if(any(!fleets %in% unique(exp_vals_list$lencomp$FltSvy))) {
      stop("A fleet specified in fleets was not found in the fleets in len ",
           "comps for exp_vals_list.")
    }
    if(class(years) != "list" | length(years) != Nfleets)
        stop("years needs to be a list of same length as fleets")
    # check that Lbin_lo and Lbin_hi are the same for all CAL data. This is
    # a requirement of how the function is currently written
    if(!all(agecomp.cal$Lbin_lo == agecomp.cal$Lbin_hi)) {
      stop("In order to use sample_calcomp, for each row of conditional age at",
           " length data, Lbin_lo must equal Lbin_hi. Currently, this is not ",
           "the case for all conditional age at length data within dat_list.")
    }
    # check that bin compression is turned off. Bin compression should never be
    # used with CAAL. If it is not turned off, warn the user.
    if(any(dat_list$age_info$mintailcomp[fleets] >= 0)) {
      stop("Bin compression cannot be used for any fleets with CAL data. ",
           "Please turn off bin compression by making mintailcomp in the ",
           "age_info table negative for any fleet with CAL",
           "data and rerun ss3sim. This should be done in the skeleton OM data ",
           "file or, if using mla, can be done by specifying the tail ",
           "compression in a case file")
    }
    # check that the sample size for lengths are always greater than ages.
   Msamp_check <-  mapply(function(l, a) {
      if(length(l) == 1) {
        l <- rep(l, length(a))
      }
      if(length(a) == 1) {
        a <- rep(a, length(l))
      }
      if(length(l) != length(a)) {
        stop("Nsamp_lengths and Nsamp_ages should either have the same length ",
             "for each fleet or have length of 1.")
      }
      if(any(l < a)) {
        invalid_input <- TRUE
      } else {
        invalid_input <- FALSE
      }
      invalid_input
    }, l = Nsamp_lengths, a = Nsamp_ages, SIMPLIFY = FALSE)

    if(any(unlist(Msamp_check) == TRUE)) {
      stop("More age samples specified than fish collected for calcomps. ",
           "Please adjust the Nsamp_ages specified via sample_calcomp function",
           "input so that it is less than the Nsamp_lengths for the same fleet",
           "and year.")
    }
    # Check that the years specified are valid
    check_yrs <- function(f, y, d) {
     yrs_in_mod <- unique(d[d$FltSvy == f, "Yr"])
     if(any(!y %in% yrs_in_mod)) {
       stop("A year specified in years was not found in the expected value ",
            "age comps or length comps for fleet ", f, ". Years in expected ",
            "values (length or age comps): ",
            paste0(yrs_in_mod, collapse = ", "), "; Years ",
            "specified through years:", paste0(y, collapse = ", "))
     }
   }
   # check years in both age and length comp
   mapply(check_yrs,
          f = fleets, y = years,
          MoreArgs = list(d = exp_vals_list[["lencomp"]]))
    mapply(check_yrs,
           f = fleets, y = years,
           MoreArgs = list(d = agecomp.cal))
    ## End input checks
    # sample the length comp data that will then be used to get conditional age
    # at length samples.
    CAL_lencomp <- sample_comp(exp_vals_list[["lencomp"]],
                               Nsamp = Nsamp_lengths,
                               fleets = fleets,
                               years = years)
    ## The general approach here is to loop through each fl/yr combination
    ## (for which there will be a row for each length bin) and then
    ## recombine them later.
    newcomp.list <- list() # temp storage for the new rows
    k <- 1                 # each k is a new row of data, to be rbind'ed later
    ## Loop through each fleet
    for(i in seq_along(fleets)){
        fl <- fleets[i]
        if(length(Nsamp_ages[[i]]) == 1) {
          Nsamp_ages[[i]] <- rep(Nsamp_ages[[i]], length(years[[i]]))
        }
        if(length(Nsamp_lengths[[i]]) == 1 ) {
          Nsamp_lengths[[i]] <- rep(Nsamp_lengths[[i]], length(years[[i]]))
        }
        agecomp.cal.fl <- agecomp.cal[agecomp.cal$FltSvy == fl &
                                      agecomp.cal$Yr %in% years[[i]], ]
        ## Only loop through the subset of years for this fleet
        for(yr in years[[i]]) {
            newcomp <- agecomp.cal.fl[agecomp.cal.fl$Yr==yr, ]
            #Note that the below check may be redundant...
            if(nrow(newcomp) != length(lbin_vector)) {
                stop("The number of conditional age at length data rows for ",
                "fleet ", fl, " and year ", yr, " is not the same as the number",
                " of length bins. For each fleet and year, please make sure ",
                "there is row where Lbin_lo and Lbin_hi are equal to each of ",
                "the values in lbin_vector: ",
                paste0(lbin_vector, collapse = ", "), ". Note that Lbin_lo and",
                " Lbin_hi if conditional age at length data should always have",
                " the same values in order for sample_calcomp() to work.")
            }
            if(NROW(newcomp) == 0) {
              stop("no age data found for fleet ", fl, "and yr ", yr)
            }
            ## Get the sample sizes of the length and age comps.
            ## Probability distribution for length comps
            prob.len <- as.numeric(
              CAL_lencomp[CAL_lencomp$Yr==yr & CAL_lencomp$FltSvy==fl, -(1:6)])
            ## From observed length distribution, sample which fish to age.
            yr.ind <- which(years[[i]]==yr)
            Nsamp.len <- Nsamp_lengths[[i]][yr.ind]
            if(any(prob.len>1)){
                ## This code creates a vector of empirical samples of
                ## length, such that each length bin is repeated equal to
                ## the number of observed fish in that bin
                prob.len.ints <- unlist(sapply(seq_along(prob.len),
                                               function(i) rep(i, prob.len[i])))
                ## Now resample from it, garuanteeing that the sample size
                ## doesn't exceed
                temp <- sample(x=prob.len.ints, size=Nsamp_ages[[i]][yr.ind],
                               replace=FALSE)
                Nsamp.ages.per.lbin <- sapply(seq_along(prob.len),
                                              function(i) sum(temp==i))
                ## Note: If you're ageing all fish this isn't needed, but holds.
            } else {
                ## (2) case of Dirichlet.
                Nsamp.ages.per.lbin <- rmultinom(n=1,
                                                 size=Nsamp_ages[[i]][yr.ind] ,
                                                 prob=prob.len)
            }
            ## Nsamp.ages.per.lbin is the column of sample sizes in the
            ## CAAL matrix, which gives the sample size to draw CAAL
            ## samples below.
        if(any(is.na(Nsamp.ages.per.lbin))) {
            stop("Invalid age sample size for a length bin in calcomp")
        }
        ## Sample conditional age at length. Loop through each
        ## length bin and sample # fish in each age bin, given expected
        ## conditional age-at-length
        newcomp$Nsamp <- Nsamp.ages.per.lbin
        for(ll in seq_len(nrow(newcomp))) {
            N.temp <- newcomp$Nsamp[ll]
            if(N.temp>0){
                cal.temp <-
                    rmultinom(n=1,
                              size=Nsamp.ages.per.lbin[ll],
                              prob=as.numeric(newcomp[ll,-(1:9)]))
            } else {
                cal.temp <- -1 # placeholders
            }
            newcomp[ll,-(1:9)] <- cal.temp
        }
        newcomp <- newcomp[newcomp$Nsamp>0,] # get rid of placeholders
        newcomp.list[[k]] <- newcomp
        k <- k+1
    }
}

    ## Combine back together into final data frame with the different data
    ## types
    newcomp.final <- do.call(rbind, newcomp.list)

    # Effective sample sizes ----
    # length
    if(!is.null(ESS_lengths)) {
      if(!length(ESS_lengths) %in% c(1,length(fleets))) {
        stop("Dimensions of ESS_lengths is not compatible with other",
             " arguments.")
      }
      if(length(unlist(ESS_lengths)) == 1) {
        CAL_lencomp[, "Nsamp"] <- unlist(ESS_lengths)
      } else {
        #match up with each year and fleet
        for(flt in seq_along(fleets)) {
          tmp_yrs <- years[[flt]]
          tmp_ess <- ESS_lengths[[flt]]
          if(length(tmp_ess) == 1) tmp_ess <- rep(tmp_ess, times = length(tmp_yrs))
          if(length(tmp_ess) != length(tmp_yrs)) {
            stop("Dimensions of ESS_lengths is not compatible with other",
                 " arguments. For fleet ", fleets[flt], " years had length ",
                 length(tmp_yrs), " and there were ", length(tmp_ess), "values ",
                 "in ESS_lengths for the fleet, but it should have 1 or ",
                 length(tmp_yrs),
                 "values.")
          }
          for(yr in seq_along(tmp_yrs)) {
            CAL_lencomp[CAL_lencomp$FltSvy == flt &
                        CAL_lencomp$Yr == tmp_yrs[yr], "Nsamp"] <-
              tmp_ess[yr]
          }
        }
      }
    }
    # ages?
    # maybe it would be better to have 2 parameters for this?
    # ESS_ages_proportion Which will divide it up
    # ESS_ages_all_equal which sets all age bins the same (e.g., 1)
    # TODO: implment adding in effective sample size for ages.Think more about
    # the most user friendly way to do this. will start by just implementing a
    # ESS_ages which divides up the same as Nsamp_ages. Can modify this to be
    # more flexible in the future.
    if(!is.null(ESS_ages)) {
      if(!length(ESS_ages) %in% c(1,length(fleets))) {
        stop("Dimensions of ESS_ages is not compatible with other",
             " arguments.")
      }
      if(length(unlist(ESS_ages)) == 1) {
        dat_combos <- unique(newcomp.final[, c("Yr", "FltSvy")])
        for(dc in seq_len(nrow(dat_combos))) {
          tmp_combo <- dat_combos[dc, ]
          tmp_total_Nsamp <-
            sum(newcomp.final[newcomp.final$FltSvy == tmp_combo$FltSvy&
                              newcomp.final$Yr == tmp_combo$Yr, "Nsamp"])
          tmp_frac <- newcomp.final[
                       newcomp.final$FltSvy == tmp_combo$FltSvy&
                       newcomp.final$Yr == tmp_combo$Yr, "Nsamp"]/tmp_total_Nsamp
          # replace Nsamp
          newcomp.final[
            newcomp.final$FltSvy == tmp_combo$FltSvy&
              newcomp.final$Yr == tmp_combo$Yr, "Nsamp"] <- tmp_frac * unlist(ESS_ages)
        }
      } else {
        #match up with each year and fleet
        for(flt in seq_along(fleets)) {
          tmp_yrs <- years[[flt]]
          tmp_ess <- ESS_ages[[flt]]
          if(length(tmp_ess) == 1) tmp_ess <- rep(tmp_ess, times = length(tmp_yrs))
          if(length(tmp_ess) != length(tmp_yrs)) {
            stop("Dimensions of ESS_ages is not compatible with other",
                 " arguments. For fleet ", fleets[flt], " years had length ",
                 length(tmp_yrs), " and there were ", length(tmp_ess), "values ",
                 "in ESS_ages for the fleet, but it should have 1 or ", length(tmp_yrs),
                 "values.")
          }
          for(yr in seq_along(tmp_yrs)) {
           tmp_total_Nsamp <-  sum(newcomp.final[newcomp.final$FltSvy == flt &
                                 newcomp.final$Yr == tmp_yrs[yr], "Nsamp"])
           tmp_frac <- newcomp.final[
                         newcomp.final$FltSvy == flt &
                         newcomp.final$Yr == tmp_yrs[yr], "Nsamp"] /
                         tmp_total_Nsamp
           # replace Nsamp
           newcomp.final[newcomp.final$FltSvy == flt &
                         newcomp.final$Yr == tmp_yrs[yr], "Nsamp"] <-
             tmp_frac * tmp_ess[yr]
          }
        }
      }
    }
    # add the new length comp
    if(!is.null(lencomp_marginal)) {
      newfile[["lencomp"]] <- rbind(CAL_lencomp, lencomp_marginal)
    } else {
      newfile[["lencomp"]] <- CAL_lencomp
    }

    #age comps
    if(NROW(agecomp.age) > 0 & NROW(newcomp.final) > 0) {
      newfile[["agecomp"]] <- rbind(agecomp.age, newcomp.final)
    }
    if(NROW(agecomp.age) == 0 & NROW(newcomp.final) > 0) {
      newfile[["agecomp"]] <- newcomp.final
    }
    if(NROW(agecomp.age) > 0 & NROW(newcomp.final) == 0) {
      newfile[["agecomp"]] <- agecomp.age
    }
    if(NROW(agecomp.age) == 0 & NROW(newcomp.final) == 0) {
      newfile[["agecomp"]] <- NULL
    }

    ## Write the modified file
    if (!is.null(outfile)){
      r4ss::SS_writedat(datlist = newfile, outfile = outfile,
                        version = ss_version, overwrite = TRUE, verbose=FALSE)
    }
    invisible(newfile)
}
