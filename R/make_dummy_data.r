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


make_dummy_dat_lencomp <- function(fleets, years, len_bins) {
    ## expand dummy data across all types:
    dummy_dat_list <- lapply(fleets, function(fleet) {
        data.frame("Yr"   = years, "Seas" = 1, "Flt"  = fleet,
                   "Gender" = 0, "Part"   = 0, "Nsamp" = 10,
                   stringsAsFactors = FALSE)})
    dummy_dat <- as.data.frame(do.call('rbind', dummy_dat_list))
    ## Add the dummy data for each data cell
    dummy_df <- data.frame(matrix(1, nrow=nrow(dummy_dat), ncol=length(len_bins)))
    names(dummy_df) <- paste0("l", len_bins)
    cbind(dummy_dat, dummy_df)
}

make_dummy_dat_agecomp <- function(fleets, years, age_bins) {
    ## expand dummy data across all types:
    dummy_dat_list <- lapply(fleets, function(fleet)
        data.frame("Yr"   = years, "Seas" = 1, "Flt"  = fleet, "Gender" = 0,
                   "Part"   = 0, "AgeErr"=1, "Lbin_lo"=-1, "Lbin_hi"=-1,
                   "Nsamp" = length(age_bins), stringsAsFactors = FALSE))
    dummy_dat <- as.data.frame(do.call('rbind', dummy_dat_list))
    ## Add the dummy data for each data cell
    dummy_df <- data.frame(matrix(1, nrow=nrow(dummy_dat), ncol=length(age_bins)))
    names(dummy_df) <- paste0("a", age_bins)
    cbind(dummy_dat, dummy_df)
}

## fleets <- c(1,2)
## years <- c(7,9)
## age_bins <- c(1,5,10)
## Lbin_lo <- (1:5)[-5]
## Lbin_hi <- (1:5)[-1]
make_dummy_dat_calcomp <- function(fleets, years, age_bins,
                                  len_bins) {
    Lbin_lo <- len_bins
    Lbin_hi <- c(len_bins[-1], -1)
    dummy_dat_list <- lapply(fleets, function(fleet)
                             lapply(years, function(yr)
        data.frame("Yr"   = yr, "Seas" = 1, "Flt"  = fleet, "Gender" = 0,
                   "Part"   = 0, "AgeErr"=1, "Lbin_lo"=Lbin_lo, "Lbin_hi"=Lbin_hi,
                   "Nsamp" = length(age_bins), stringsAsFactors = FALSE)))
    dummy_dat_list <- unlist(dummy_dat_list, recursive=FALSE)
    dummy_dat <- as.data.frame(do.call('rbind', dummy_dat_list))
    ## Add the dummy data for each data cell
    dummy_df <- data.frame(matrix(1, nrow=nrow(dummy_dat), ncol=length(age_bins)))
    names(dummy_df) <- paste0("a", age_bins)
    cbind(dummy_dat, dummy_df)
}

make_dummy_dat_mlacomp <- function(fleets, years, age_bins) {
    ## expand dummy data across all types:
    dummy_dat_list <- lapply(fleets, function(fleet)
        data.frame("Yr"   = years, "Seas" = 1, "Flt"  = fleet, "Gender" = 0,
                   "Part"   = 0, "AgeErr"=1, "Nsamp" = 10, stringsAsFactors = FALSE))
    dummy_dat <- as.data.frame(do.call('rbind', dummy_dat_list))
    ## Add the dummy data for each data cell
    dummy_df <- data.frame(matrix(1, nrow=nrow(dummy_dat), ncol=length(age_bins)*2))
    names(dummy_df) <- c(paste0("a", c(age_bins)), paste0("N", c(age_bins)))
    cbind(dummy_dat, dummy_df)
}

