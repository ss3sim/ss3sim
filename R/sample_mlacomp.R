#' Sample mean length (size-)-at-age data and write to file for use by the EM.
#'
#' @details Take a \code{data.SS_new} file, read in by \pkg{r4ss} function
#'   \code{\link[r4ss]{SS_readdat}} containing observed values, and
#'   sample from the observed ages to get realistic proportions for the number
#'   of fish in each age bin, then use the mean size-at-age and CV for growth to
#'   generate random samples of size, which are then averaged to get mean
#'   length-at-age values. These values are then written to file for the
#'   EM.
#' @author Cole Monnahan, Kelli Johnson
#'
#' @template lcomp-agecomp-index
#' @template lcomp-agecomp
#' @param datfile A \code{.dat} file read in by \pkg{r4ss} function
#'   \code{\link[r4ss]{SS_readdat}}. This list will be used to determine
#'   how many fish of each age bin are to be sampled and the mean length
#'   for the distribution to sample from.
#' @param ctlfile A path to the control file, outputed from an OM, containing
#'   the OM parameters for growth. These values are used to determine the
#'   uncertainty about size for fish sampled in each age bin.
#' @param mean_outfile A path to write length and age data for external
#' estimation of parametric growth. If NULL no file will be written.
#' This file is used by \code{change_e} to externally estimate growth
#' parameters. Filename must contain "vbgf" to be used by \code{change_e}.
#' Also, if "remove" is included in the filename, the mean length at age data
#' will be removed from the \code{.dat} file and not be available to the EM.
#' @param verbose Logical value whether or not diagnostic information from
#'   \pkg{r4ss} functions should be printed to the screen. Default is FALSE.
#' @template sampling-return
#' @template casefile-footnote
#' @seealso \code{\link{sample_lcomp}, \link{sample_agecomp},
#'   link{sample_calcomp}}
#' @importFrom r4ss SS_writedat SS_parlines
#' @export
#'
#' @examples
#' temp_path <- file.path(tempdir(), "ss3sim-test")
#'   dir.create(temp_path, showWarnings = FALSE)
#'   wd <- getwd()
#'   setwd(temp_path)
#' d <- system.file("extdata/models/cod-om", package = "ss3sim")
#'   dat_in <- file.path(d, "codOM.dat")
#'   datfile <- r4ss::SS_readdat(dat_in, section = 2, verbose = FALSE)
#'   datfile <- change_fltname(datfile)
#'   datfile <- change_data(datfile, outfile = NULL, write_file = FALSE,
#'     fleets = 1, years = 1990:2010, types = c("age", "mla"))
#'   datfile <- change_fltname(datfile)
#'   ctlfile <- file.path(d, "codOM.ctl")
#'
#' out <- sample_mlacomp(datfile, outfile = NULL, ctlfile = ctlfile,
#'                       fleets = 1, Nsamp = 30, years = list(1992),
#'                       verbose = FALSE, mean_outfile = "test.csv", write_file = FALSE)
#'
#' setwd("..")
#' unlink("ss3sim-test", recursive = TRUE)
#' setwd(wd)
#'

sample_mlacomp <- function(datfile, outfile, ctlfile, fleets = 1, Nsamp,
                           years, write_file=TRUE, mean_outfile = NULL,
                           verbose = TRUE){

  ## If fleets==NULL, quit here and delete the data so the EM doesn't use it.
  if (is.null(fleets)) {
    datfile$MeanSize_at_Age_obs <- data.frame("#")
    datfile$N_MeanSize_at_Age_obs <- 0
    if (write_file)
      SS_writedat(datlist = datfile, outfile = outfile, overwrite = TRUE,
                  verbose = verbose)
    return(invisible(datfile))
  }

  is_ssdat_file(datfile)
  # Users can specify either Lbin_lo or Lbin_hi as a negative value to
  # delineate between cal and age comp data
  agecomp <- datfile$agecomp[datfile$agecomp$Lbin_lo == -1 |
                             datfile$agecomp$Lbin_hi == -1, ]
  if (NROW(agecomp) == 0) {
    stop(paste0("No age data exist in the datfile."))
  }
  mlacomp <- datfile$MeanSize_at_Age_obs
  agebin_vector <- datfile$agebin_vector

  ## Read in the control file
  ctl <- SS_parlines(ctlfile)
    CV.growth <- ctl[ctl$Label == "CV_young_Fem_GP_1", "INIT"]
    CV.growth.old <- ctl[ctl$Label == "CV_old_Fem_GP_1", "INIT"]
    if (CV.growth != CV.growth.old) {
      stop(paste0("sample_mlacomp does not support different values for the",
                  "CV's of young and old fish. Please the check ", ctlfile,
                  "and make sure CV_young_Fem_GP_1 (", CV.growth, ") is",
                  " equal to CV_old_Fem_GP_1 (", CV.growth.old, ")."))
    }

  ## Check inputs for errors
  if (!is.null(outfile) & write_file){
    if (substr_r(outfile,4) != ".dat") {
      stop(paste0("outfile ", outfile, " needs to end in .dat"))
    }
  }

  Nfleets <- length(fleets)
  if (length(years) != Nfleets) {
    stop("years needs to be a list of same length as fleets")
  }
  if (class(years) != "list" & length(years) > 1 & Nfleets == 1) {
    stop("years needs to be a list unless it only includes one fleet and one year")
  }
  if (is.null(mlacomp)) {
    stop("mean length-at-age compositions do not exist")
  } else {
      if (NROW(mlacomp[mlacomp$AgeErr > 0, ]) < 1){
        stop("mean length-at-age compositions do not exist")
      }
    }
  # Check for a sample size for every year
  if (length(Nsamp) != length(Nfleets)){
    stop(paste("Nsamp was not included for all fleets in fleet.",
               "User must include a sample size for each fleet that can be",
               "repeated for each year for that fleet or specified as a list of",
               "vectors, with the same dimensions as years."))
  }
  if (any(sapply(Nsamp, length) == 1)) {
    repNsamp <- which(sapply(Nsamp, length) == 1)
    for (i in repNsamp){
      Nsamp[[i]] <- rep_len(Nsamp[[i]], length(years[[i]]))
    }
  }
  if (any(sapply(Nsamp, length) != sapply(years, length))) {
    stop(paste("Number of samples were not specified for every year.\n",
               "length of years and Nsamp did not match for fleet(s)",
               fleets[which(sapply(Nsamp, length) != sapply(years, length))]))
  }
  ## End input checks

  mlacomp.new.list <- list() # temp storage for the new rows
  forexport <- list()
  k <- 1                 # each k is a new row of data, to be rbind'ed later
  # Loop through mla data for this fleet, given the years specified
  for (fl in 1:length(fleets)) {
    fl.temp <- fleets[fl]
    mlacomp.fl <- mlacomp[mlacomp$FltSvy == fleets[fl] &
                          mlacomp$Yr %in% years[[fl]], ]
    if (length(years[[fl]]) != length(unique(mlacomp.fl$Yr))) {
      stop(paste("A year specified in years for fleet", fl.temp, "was not",
                 "found in the input datfile for fleet", fl.temp))
    }
    for (j in 1:NROW(mlacomp.fl)) {
      yr.temp <- mlacomp.fl$Yr[j]
      # Loop through mla data for this fleet / year combo
      mlacomp.new <- mlacomp.fl[j, ]
      if (NROW(mlacomp.new) == 0) {
        stop(paste("No mla comp data found for fleet", fl.temp,
                   "in year", yr.temp))
      }
      # Get the columns that pertain to the actual mla data and not metadata
      # The following subset routine will only work for a single sex model
      if (any(grepl("f", names(mlacomp.new)))) {
        stop(paste("mlacomp data contains two sexes, which is not currently",
                   "supported by ss3sim, please reconfigure your model."))
      }
      mla.means <- as.numeric(mlacomp.new[paste0("a", agebin_vector)])
      ## For each age, given year and fleet, get the expected length
      ## and CV around that length, then sample from it using
      ## lognormal (below)
      sds <- mla.means * CV.growth
      # given an age the length distribution is lognormal with expected value
      # equal to the expected mean length at age and a CV equal to the CV
      # from the control file
      # \mu = log(\frac{M}{sqrt{1 + \frac{V}{m^2})}})
      # log(\frac{m}{(1 + \frac{v}{m^2})^(1/2)}) = log(\frac{m^2}{(m^2 + v)^(1/2)})
      means.log <- log(mla.means^2 / sqrt(sds^2 + mla.means^2))
      # sigma^2 = ln(1 + var[X] / (E[X])^2)
      # where the variance is defined on the real scale b/c the distribution
      # is symmetric, where $\mu$ is defined on the log scale.
      sds.log <- sqrt(log(1 + sds^2 / mla.means^2))
      # Get the true age distributions, probability of being a fish of age x
      agecomp.temp <- agecomp[agecomp$Yr == yr.temp &
                              agecomp$FltSvy == fl.temp, ]
      # remove the 9 columns of metadata
      age.means <- as.numeric(agecomp.temp[-(1:9)])
      # Get user input sample size, theoretically this cannot be bigger than age n
      age.Nsamp <- as.numeric(Nsamp[[fl]][j])
      if (age.Nsamp > sum(agecomp$Nsamp)) {
        stop(paste("Cannot sample more fish for mean length-at-age than what",
                   "exists in the observed ages. ", agecomp$Nsamp, "were",
                   "observed but user specified a sample size of ", age.Nsamp))
      }
      ## Draw samples to get # of fish in each age bin
      age.samples <- rmultinom(n = 1, size = as.integer(age.Nsamp),
                               prob = age.means)

      # Draw samples to get # of fish in each age bin
      if (any(is.na(age.means))) {
        stop("Invalid age comp probabilities in sample_mlacomp.")
      }
      # apply sampling across columns (ages) to get sample of lengths
      lengths.list <-
        lapply(1:length(means.log), function(kk) {
          exp(rnorm(n = age.samples[kk], mean = means.log[kk],
          sd = sds.log[kk]))
          })

      # prepare mla data for export to the von B growth function
      names(lengths.list) <- as.numeric(gsub("a", "",
                                        colnames(agecomp.temp)[-(1:9)]))
      temp <- lapply(seq_along(lengths.list), function(x) {
         cbind(age = names(lengths.list)[x], length = lengths.list[[x]],
               mean = exp(means.log[x]), fleet = fl.temp, year = yr.temp)
        })
      forexport[[k]] <- do.call("rbind", temp[lapply(temp, length) > 4])
      # Take mean length of each age bin mean and place in mla comp data frame
      mlacomp.new.means <- do.call(c, lapply(lengths.list, mean))
      # Sometimes you draw 0 fish from an age class, resulting in NaN
      # For now, replace with filler values
      # TODO: Fix the placeholder values for missing age bins
      mlacomp.new.means[is.nan(mlacomp.new.means)] <- -1
      ## mla data needs the sample sizes, so concatenate those on
      mlacomp.new[-(1:7)] <- c(mlacomp.new.means, age.samples)
      mlacomp.new.list[[k]] <- mlacomp.new
      k <- k + 1
    }
  } # end sampling
  if (!is.null(mean_outfile)) {
    write.csv(do.call("rbind", forexport), mean_outfile, row.names = FALSE)
  }
  ## Combine new rows together into one data.frame
  mlacomp.new <- do.call(rbind, mlacomp.new.list)
  datfile$MeanSize_at_Age_obs <- mlacomp.new
  datfile$N_MeanSize_at_Age_obs <- NROW(mlacomp.new)
  ## Write the modified file
  if(write_file) SS_writedat(datlist = datfile, outfile = outfile,
                             overwrite = TRUE, verbose = verbose)
  return(invisible(datfile))
}
