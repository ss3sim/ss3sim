#' Sample empirical weight-at-age data and write to file for use by the EM
#'
#' Take a \code{data.SS_new} file containing expected values and sample from
#' true ages to get realistic proportions for the number of fish in each age
#' bin, then use the mean size-at-age and CV for growth to generate random
#' samples of size, which are then converted to weight and averaged to get mean
#' weight-at-age values. Missing ages and years are filled according to a
#' specified function. These matrices are then written to file for the EM. By
#' calling this function, \pkg{ss3sim} will turn on the empirical weight-at-age
#' function (set maturity option to 5) automatically. See
#' \code{\link{ss3sim_base}} for more details on how that is implemented.
#' If used with \code{\link{run_ss3sim}} the case file should be named
#' \code{wtatage}.
#'
#' @author Cole Monnahan, Allan Hicks, Peter Kuriyama
#'
#' @param ctl_file_in A path to the control file, output from an OM, containing
#'   the OM parameters for growth and weight/length relationship. These values
#'   are used to determine the uncertainty about weight for fish sampled in each
#'   age bin. Commonly \code{control.ss_new}
#' @param fill_fnc *A function to fill in missing values (ages and years). The
#'   resulting weight-at-age file will have values for all years and ages.One
#'   function is \code{fill_across}.
#' @param cv_wtatage A user specified CV for growth. Default is \code{NULL}.
#' @template distribution
#' @return A modified \code{.wtatage.ss} file if \code{!is.null(outfile)}. A list
#'   object containing the modified \code{.wtatage.ss} file is returned invisibly.
#' @template lcomp-agecomp-index
#' @template dat_list
#' @template outfile
#' @template casefile-footnote
#' @importFrom r4ss SS_parlines
#' @importFrom stats aggregate
#' @seealso \code{\link{fill_across}}
#' @family sampling functions
#' @export
#' @examples
#' d <- system.file("extdata", "models", "cod-om", package = "ss3sim")
#' dat <- r4ss::SS_readdat(dir(d, full.names = TRUE, pattern = "\\.dat"),
#'   verbose = FALSE)
#' dat[["use_MeanSize_at_Age_obs"]] <- 1
#' temp <- t(sapply(rnorm(length(26:100), mean = 10), 
#'   seq, to = 100, length.out = dat$Nages))
#' colnames(temp) <- paste0("a", 1:dat$Nages)
#' dat[["MeanSize_at_Age_obs"]] <- data.frame(
#'   "Yr" = 26:100, "Seas" = 1, "FltSvy" = 1, "Gender" = 0, "Part" = 0,
#'   "AgeErr" = 0, "Ignore" = 10, temp)
#' wt <- sample_wtatage(outfile = NULL, dat_list = dat,
#'  ctl_file_in = dir(d, full.names = TRUE, pattern = "\\.ctl"),
#'  years = list(50:100), fill_fnc = fill_across,
#'   fleets = list(1), cv_wtatage = 0.05)
#'
#' rm(d, dat, temp, wt)
#'
sample_wtatage <- function(outfile, dat_list, ctl_file_in,
  years, fill_fnc = fill_across, fleets,
  cv_wtatage = NULL, distribution = c("rmultinom")){

  distribution <- match.arg(distribution, several.ok = FALSE)

  if(is.null(cv_wtatage)) stop("Specify cv_wtatage to sample weight-at-age data.")
  if(is.null(dat_list$MeanSize_at_Age_obs)) stop("data_list doesn't contain",
    " mean size-at-age observations.")
  if(any(dat_list$agecomp$Gender != 0)) {
    stop("sample_wtatage cannot handle multiple gender models right now.")
  }
  ## fleets = NULL turns off this data type in ss3sim_base
  if(is.null(fleets)) return(NULL)
  if(any(!fleets %in% seq(dat_list$Nfleets))) {
    stop("Not all fleets in fleets, ", paste(fleets, collapse = ", "), ", are available.")
  }
  if(length(fleets) > 1) {
    if(length(years) == 1) {
      years[2:length(fleets)] <- years[1]
    }
    if(length(cv_wtatage) == 1){
      cv_wtatage[2:length(fleets)] <- cv_wtatage[1]
    }
  }

  # compress conditionals to a marginal number of samples per year
  agecomp <- stats::aggregate(. ~ Yr + FltSvy + Gender, FUN = sum,
    data = dat_list$agecomp[, grep("Yr|Flt|samp|Gend|^a[0-9]", colnames(dat_list$agecomp))])

  mlacomp <- split_comp(dat_list$MeanSize_at_Age_obs)
  ## Read in the control file
  ss_version <- get_ss_ver_file(ctl_file_in)
  ctl <-SS_parlines(ctl_file_in, version = ss_version)
  #Define growth parameters
  Wtlen1 <- ctl[grep("Wtlen_1_Fem", ctl$Label), "INIT"]
  Wtlen2 <- ctl[grep("Wtlen_2_Fem", ctl$Label), "INIT"]
  if(any(grepl("Wtlen_1_M|GP_[2-9]", ctl$Label))) {
    stop("sample_wtatage does not currently accommodate",
      " 2-sex models or multiple growth morphs.")
  }

  wtatage <- make_dummy_wtatage(fleets = unlist(fleets),
    years = dat_list$styr:dat_list$endyr,
    age_bins = 0:dat_list$Nages)
  for(fl in fleets) {
    for(yr in years[[fl]]) {
      #Step 1, draw from true age distributions if there were samples
      agecomp.temp <- split_comp(
        agecomp[agecomp$Yr==yr & agecomp$FltSvy == fl, ])
      if(agecomp.temp$n==0) next
      if(NROW(agecomp.temp$data) > 1) stop("More than one row of age samples",
        " when sampling by fleet and year in sample_wtatage.")

      #Step 2, determine # of fish in sample per bin
      #todo: have option to use DM instead of multinomial
      if(distribution == "rmultinom") {
        age.samples <- rmultinom(n = 1,
          size = agecomp.temp$meta[, "Nsamp"],
          prob = agecomp.temp$raw)
      } else{
        stop("No other distributions are coded to sample the number of ages yet.")
      }
      #Step 3, use mean length-at-age to sample with normal/lognormal and user-specified cv
      #first define mean length-at-age
      mla.means <- mlacomp$raw[mlacomp$meta$Yr == yr & mlacomp$meta$FltSvy == fl, ]

      #step 4, sampling from normal distribution
      lengths.list <- mapply(rnorm, n = age.samples, mean = mla.means,
        sd = mla.means * cv_wtatage[fl])
      if(any(unlist(lengths.list) < 0)) {
        stop("CV of weight-at-age (cv_wtatage = ", cv_wtatage[fl], ") is too large",
          "\nfish of negative length are being sampled in the call to rnorm for lengths.")
      }
      
      #step 5, calculate new mean weight at age
      wtatage[wtatage$yr == yr & wtatage$fleet == fl,
        grep("^a[1-9]",colnames(wtatage))] <- sapply(lengths.list,
        function(x) mean(Wtlen1 * x^Wtlen2), simplify = TRUE)
    }
    if(all(is.na(wtatage[wtatage$fleet == fl, grep("a[0-9]", colnames(wtatage))]))) {
      stop("No weight-at-age data were sampled, probably because age data weren't available.")
    }    
    wtatage[wtatage$fleet == fl, ] <- fill_wtatage(wtatage[wtatage$fleet == fl, ])
    availableyears <- max(wtatage[wtatage$fleet == fl, "yr"])
    # Make maximum year a negative to fill in all remaining years for the forecasting
    wtatage[wtatage$fleet == fl & wtatage$yr == availableyears, "yr"] <- -1 * availableyears
  }

  maturity.pars <- ctl[grep("^Mat", ctl$Label), "INIT"]
  maturityatage <- exp(
    maturity.pars[1]*maturity.pars[2] + 0:dat_list$Nages *-1* maturity.pars[2]) /
    (1+exp(maturity.pars[1]*maturity.pars[2] + 0:dat_list$Nages * -1*maturity.pars[2]))
  population <- fill_wtatage(aggregate(. ~ yr + seas + sex + bio_pattern + birthseas,
    data = type.convert(wtatage),
    FUN = function(x) mean(x, na.rm = TRUE), na.action = "na.pass"))
  populationpieces <- fill_header(population)
  fecundity <- cbind(
    populationpieces[["prefix"]],
    as.matrix(populationpieces[["data"]]) %*% diag(maturityatage))
  colnames(fecundity) <- populationpieces[["cols"]]
  fecundity$fleet <- -2

  fleetsneeded <- c(-1:0,seq(dat_list$Nfleets)[!seq(dat_list$Nfleets)%in%fleets])
  wtatagesampled <- rbind(fecundity, wtatage, do.call("rbind",
    mapply(`[<-`, replicate(length(fleetsneeded), population, simplify = FALSE), 'fleet',
      value = fleetsneeded,
      SIMPLIFY = FALSE)))
  colnames(wtatagesampled)[1:6] <- tools::toTitleCase(colnames(wtatagesampled)[1:6])

  if(!is.null(outfile)) {
    r4ss::SS_writewtatage(wtatagesampled,
      file = basename(outfile), dir = dirname(outfile), overwrite=TRUE,
      verbose = FALSE, warn = FALSE)
  }

    return(invisible(wtatagesampled))
}
