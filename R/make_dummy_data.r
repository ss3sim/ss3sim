make_dummy_dat_index <- function(fleets, years) {
    ## expand dummy data across all types:
    dummy_dat_list <- lapply(fleets, function(fleet) {
        data.frame("year"   = years, "Seas" = 1, "index"  = fleet,
                   "obs" = 1, "se_log" = .1,
                   stringsAsFactors = FALSE)})
    dummy_dat <- as.data.frame(do.call('rbind', dummy_dat_list))
    ## Add the dummy data for each data cell
    dummy_dat
}


make_dummy_dat_lencomp <- function(fleets, years, len_bins, gender) { # ADD A GENDER ARGUMENT IN THERE # PROB WHERE SEASON WOULD HAVE TO BE CHANGED TOO
    ## expand dummy data across all types:
    dummy_dat_list <- lapply(fleets, function(fleet) {
        data.frame("Yr"   = years, "Seas" = 1, "FltSvy"  = fleet, ################## Changed Flt to FLTSVY
                   "Gender" = gender, "Part"   = 0, "Nsamp" = 10, ################## REPLACE Gender by 3 if dat_list$Ngenders == 2
                   stringsAsFactors = FALSE)})
    dummy_dat <- as.data.frame(do.call('rbind', dummy_dat_list))
    ## Add the dummy data for each data cell
    if (gender==3) len_bins <- c(len_bins,len_bins) ############################# ADDED IN # MULTIPLY BY 2 if GENDER = 3
    dummy_df <- data.frame(matrix(1, nrow=nrow(dummy_dat), ncol=length(len_bins))) 
    names(dummy_df) <- paste0("l", len_bins)
    cbind(dummy_dat, dummy_df)
}

make_dummy_dat_agecomp <- function(fleets, years, age_bins, gender) { ## SEE ABOVE 
    ## expand dummy data across all types:
    dummy_dat_list <- lapply(fleets, function(fleet)
        data.frame("Yr"   = years, "Seas" = 1, "FltSvy"  = fleet, "Gender" = gender, ################## Changed Flt to FLTSVY
                   "Part"   = 0, "Ageerr"=1, "Lbin_lo"=-1, "Lbin_hi"=-1,
                   "Nsamp" = length(age_bins), stringsAsFactors = FALSE))
    dummy_dat <- as.data.frame(do.call('rbind', dummy_dat_list))
    ## Add the dummy data for each data cell
    if (gender==3) age_bins <- c(age_bins,age_bins) ############################# ADDED IN # MULTIPLY BY 2 if GENDER = 3
    dummy_df <- data.frame(matrix(1, nrow=nrow(dummy_dat), ncol=length(age_bins)))
    names(dummy_df) <- paste0("a", age_bins)
    cbind(dummy_dat, dummy_df)
}

make_dummy_dat_calcomp <- function(fleets, years, age_bins, 
                                  len_bins, gender) {# added gender in too ###################################
    Lbin_lo <- len_bins
    Lbin_hi <- c(len_bins[-1], -1)
    dummy_dat_list <- lapply(fleets, function(fleet)
                             lapply(years, function(yr)
        data.frame("Yr"   = yr, "Seas" = 1, "FltSvy"  = fleet, "Gender" = gender, ################## Changed Flt to FLTSVY
                   "Part"   = 0, "Ageerr"=1, "Lbin_lo"=Lbin_lo, "Lbin_hi"=Lbin_hi,
                   "Nsamp" = length(age_bins), stringsAsFactors = FALSE)))
    dummy_dat_list <- unlist(dummy_dat_list, recursive=FALSE)
    dummy_dat <- as.data.frame(do.call('rbind', dummy_dat_list))
    ## Add the dummy data for each data cell
    if (gender==3) age_bins <- c(age_bins,age_bins) ############################# ADDED IN # MULTIPLY BY 2 if GENDER = 3
    dummy_df <- data.frame(matrix(1, nrow=nrow(dummy_dat), ncol=length(age_bins)))
    names(dummy_df) <- paste0("a", age_bins)
    cbind(dummy_dat, dummy_df)
}

make_dummy_dat_mlacomp <- function(fleets, years, age_bins, gender) { # added gender in too ###################################
    ## expand dummy data across all types:
    dummy_dat_list <- lapply(fleets, function(fleet)
        data.frame("Yr"   = years, "Seas" = 1, "FltSvy"  = fleet, "Gender" = gender, ################## Changed Flt to FLTSVY
                   "Part"   = 0, "Ageerr"=1, "Ignore" = 10, stringsAsFactors = FALSE))
    dummy_dat <- as.data.frame(do.call('rbind', dummy_dat_list))
    ## Add the dummy data for each data cell
    if (gender==3) age_bins <- c(age_bins,age_bins) ############################# ADDED IN # MULTIPLY BY 2 if GENDER = 3
    dummy_df <- data.frame(matrix(1, nrow=nrow(dummy_dat), ncol=length(age_bins)*2))
    names(dummy_df) <- c(paste0("a", c(age_bins)), paste0("N", c(age_bins)))
    cbind(dummy_dat, dummy_df)
}

