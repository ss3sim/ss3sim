#' Sample empirical weight-at-age data
#'
#' Create samples of empirical weight-at-age data from expected values
#' of numbers at age, mean size at age, and measurements of uncertainty.
#' Empirical weight-at-age data provides fleet-specific body weights and
#' removes the need to estimate growth internally in the model because fish
#' weights are assigned values based on the input data.
#' Empirical weight-at-age data are not data because they do not have an
#' associated likelihood.
#'
#' @details
#' The steps for sampling empirical weight-at-age are as follows:
#' * Sample from the expected ages to get realistic proportions for
#'   the number of fish in each age bin.
#' * Use the mean size-at-age and coefficient of variation for growth to
#'   generate random samples of size,
#'   which are converted to weight and averaged to get mean weight-at-age.
#' * Fill in missing ages and years.
#' * Write the information to the appropriate files.
#' * Turn on weight-at-age data in Stock Synthesis by setting
#'   the maturity option to 5.
#'
#' @author Cole Monnahan, Allan Hicks, Peter Kuriyama
#'
#' @param wta_file_in The file to read weight-at-age from, this is typically
#'   `wtatage.ss_new`.
#' @param ctl_file_in A path to the control file, output from an OM, containing
#'   the OM parameters for growth and weight/length relationship. These values
#'   are used to determine the uncertainty about weight for fish sampled in each
#'   age bin. Commonly `control.ss_new`.
#' @param fill_fnc A function to fill in missing values (ages and years). The
#'   resulting weight-at-age file will have values for all years and ages. One
#'   such function is [fill_across()].
#' @param fleets A vector of integers specifying which fleets to sample from.
#'   `fleets = NULL` results in `return(NULL)`.
#' @param years A list of numeric vectors with one vector for each entry in
#'   `fleets`. The order of the entries in the list is assumed to be in the
#'   same order as fleets. That is, the first element of `years` must pertain
#'   to the first fleet listed in `fleets`.
#'
#'   Non-standard behavior will occur if an element of `years` does not
#'   contain at least two integers.
#'   Use of a single integer leads to fleet mirroring.
#'   For example, if you want fleet 1 to use information from fleet 2,
#'   then set `fleets = 2:1` and `years = list(c(1:100), 2)`.
#'   In this situation, you have to re-order fleets to ensure that sampling
#'   for fleet 2 occurs before information is needed for fleet 1. The single
#'   integer in `years[[1]]` signifies to `sample_wtatage` that it should get
#'   sample information for fleet 1 from fleet 2.
#' @template dat_list
#' @template outfile
#' @param cv_wtatage Coefficient of variation (CV) for growth.
#' @return A modified weight-at-age file will be saved if a file path is
#' provided to `outfile`. A `list` containing the weight-at-age data frame
#' is returned invisibly.
#' @seealso
#' * [fill_across()]
#' * [ss3sim_base()]
#' @family sampling functions
#' @export
#' @examples
#' \dontrun{
#' wta_file_in <- "https://raw.githubuser.com/nmfs-stock-synthesis/user-examples//wtatage.ss"
#' ctl_file_in <- "om/control.ss_new"
#' dat_list <- r4ss::SS_readdat(
#'   file = ,
#'   verbose = FALSE
#' )
#' test <- sample_wtatage(
#'   wta_file_in = wta_file_in,
#'   outfile = "wtatage.ss",
#'   dat_list = dat_list,
#'   ctl_file_in = ctl_file_in,
#'   years = list(seq(2, 100, 1), seq(2, 100, 1)),
#'   fill_fnc = fill_across,
#'   fleets = 1:2,
#'   cv_wtatage = 0.5
#' )
#' }
sample_wtatage <- function(
  wta_file_in,
  outfile,
  dat_list,
  ctl_file_in,
  years,
  fill_fnc = fill_across,
  fleets,
  cv_wtatage) {

  # Checks ----
  # fleets = NULL signifies to turn this data off in the EM
  # So, quit early and in `ss3sim_base()` do NOT turn wtatage on
  if (is.null(fleets)) {
    return(NULL)
  }

  # Setup ----
  agecomp <- dat_list$agecomp[dat_list$agecomp$Lbin_lo == -1, ]
  agebin_vector <- dat_list$agebin_vector
  mlacomp <- dat_list$MeanSize_at_Age_obs
  if (is.null(mlacomp)) stop("No mean length-at-age data in dat_list")

  # TODO: Change to providing a control list rather than file
  ctl <- r4ss::SS_parlines(ctl_file_in)
  # Read in the file and grab the expected values
  wtatage <- r4ss::SS_readwtatage(wta_file_in)
  wtatage$Yr <- abs(wtatage$Yr)
  # TODO: fix this bug regarding hard-coding for fleet
  if (2 %in% unique(wtatage$Fleet) == FALSE) {
    ones <- wtatage[wtatage$Fleet == 1, ]
    twos <- ones
    twos$Fleet <- 2
    wtatage <- rbind(wtatage, twos)
  }

  age0 <- wtatage[!duplicated(wtatage$Fleet), c("Fleet", "0")]
  wtatage.new.list <- list(fleets) # temp storage for the new rows

  # Sample ----
  # TODO: document that expected values are used for much
  #       population values, make an issue, as for different ways
  # Pull wtatage for fleets -2, -1, and 0 from OM for now.
  #-2 is age-specific fecundity * maturity
  #-1 is population wt-at-age in middle of season
  # 0 is population wt-at-age in beginning of season2
  unsampled.wtatage <- list(1, 2, 3)
  unsampled.wtatage[[1]] <- wtatage[which(wtatage$Fleet == -2), ]
  unsampled.wtatage[[2]] <- wtatage[which(wtatage$Fleet == -1), ]
  unsampled.wtatage[[3]] <- wtatage[which(wtatage$Fleet == 0), ]

  # Change all years to negatives so it will work with SS3
  unsampled.wtatage <- lapply(
    unsampled.wtatage,
    function(x) {
      x$Yr <- -x$Yr
      return(x)
    }
  )

  # Start Sampling other fleets
  # TODO: Change indexing to allow fleets to be out of order in fleets
  for (fl in fleets) {
    # set up wtatage matrix of sampled years
    wtatage.new.list[[fl]] <- as.data.frame(
      matrix(NA, nrow = length(years[[fl]]), ncol = ncol(wtatage))
    )
    names(wtatage.new.list[[fl]]) <- names(wtatage)
    row.names(wtatage.new.list[[fl]]) <- as.character(years[[fl]])

    # Loop over years sampled
    if (length(years[[fl]]) == 1) { # copy wtatage matrix from designated fleet
      if (fl <= years[[fl]]) {
        stop("You must designate an earlier fleet to copy from.\n")
      }
      wtatage.new.list[[fl]] <- wtatage.new.list[[years[[fl]]]]
      wtatage.new.list[[fl]]$Fleet <- fl
    } else {
      for (yr in years[[fl]]) {
        # Step 1, draw from true age distributions
        agecomp.temp <- agecomp[agecomp$Yr == yr & agecomp$FltSvy == fl, ]
        # Why enter a year for wtatage when you do not have data?
        if (nrow(agecomp.temp) == 0) {
          stop("No age comp observations for year", yr, "and fleet", fl, "\n")
        }

        # Get the true age distributions
        age.means <- as.numeric(agecomp.temp[-(1:9)])
        age.Nsamp <- as.numeric(agecomp.temp$Nsamp)

        # Step 2, determine # of fish in sample per bin
        # Use a multinomial here but
        # may need to have dirichlet or option to toggle between the two
        age.samples <- stats::rmultinom(n = 1, size = age.Nsamp, prob = age.means)

        # Step 3, use mean length-at-age to sample with
        # normal/lognormal and user-specified cv
        # first define mean length-at-age

        # Change fleets name to FltSvy to keep everything consistent
        names(mlacomp)[3] <- "FltSvy"
        mla.means <- as.numeric(mlacomp[
          mlacomp$Yr == yr & mlacomp$FltSvy == fl,
          paste0("a", agebin_vector)
        ])

        CV.growth <- as.numeric(cv_wtatage) # User-Specified

        # Define growth parameters
        Wtlen1 <- ctl[ctl$Label == "Wtlen_1_Fem", "INIT"]
        Wtlen2 <- ctl[ctl$Label == "Wtlen_2_Fem", "INIT"]
        sds <- mla.means * CV.growth

        # create empty list to store lengths and weights
        # todo: could be a bug here in seq(seq_len())
        lengths.list <- as.list(seq(seq_len(nrow(age.samples))))
        weights.list <- lengths.list

        # fill in list by sampling from normal distribution
        for (ii in seq_len(nrow(age.samples)))
        {
          lengths.list[[ii]] <- suppressWarnings(
            stats::rnorm(
              n = age.samples[ii],
              mean = mla.means[ii],
              sd = sds[ii]
            )
          )

          # Step 4, convert lengths to weights with no error
          weights.list[[ii]] <- Wtlen1 * lengths.list[[ii]]^Wtlen2
        }

        # step 5, calculate new mean weight at age
        samp.wtatage <- sapply(weights.list, mean)

        # concatenate everything
         # I used fleet=1 because wtatage_new only outputs fleet 1
        prefix <- wtatage[wtatage$Yr == yr & wtatage$Fleet == 1, 1:5]
        tmp.fl <- fl
        wtatage.new.means <- c(
          unlist(prefix), tmp.fl, age0[age0$fleet == 1, "age0"],
          samp.wtatage
        )
        # store to wtatage.new.list
        wtatage.new.list[[fl]][as.character(yr), ] <- wtatage.new.means
      }
    }
  }


  wtatage.complete <- lapply(
    wtatage.new.list,
    fill_fnc,
    minYear = dat_list$styr,
    maxYear = dat_list$endyr
  )
  Nlines <- sum(unlist(lapply(unsampled.wtatage, nrow)))
  Nlines <- Nlines + sum(unlist(lapply(wtatage.complete, nrow)))

  ## write wtatage.ss file
  if (!is.null(outfile)) {
    cat(dat_list$Nages, "# Maximum Age\n", file = outfile, append = FALSE)
  }

  # loop through the various matrices and build up wtatage.final while doing it
  wtatage.final <- list()

  wtatage.final[[1]] <- unsampled.wtatage[[1]]
  if (!is.null(outfile)) {
    utils::write.table(
      unsampled.wtatage[[1]],
      file = outfile,
      append = TRUE,
      row.names = FALSE,
      col.names = FALSE
    )
  }

  wtatage.final[[2]] <- unsampled.wtatage[[2]]
  if (!is.null(outfile)) {
    utils::write.table(
      unsampled.wtatage[[2]],
      file = outfile,
      append = TRUE,
      row.names = FALSE,
      col.names = FALSE
    )
  }

  wtatage.final[[3]] <- unsampled.wtatage[[3]]
  if (!is.null(outfile)) {
    utils::write.table(
      unsampled.wtatage[[3]],
      file = outfile,
      append = TRUE,
      row.names = FALSE,
      col.names = FALSE
    )
  }

  # loop through fleets
  for (i in fleets) {
    wtatage.final[[i + 3]] <- wtatage.complete[[i]]
    wtatage.final[[i + 3]]$Yr <- -1 * wtatage.final[[i + 3]]$Yr
    if (!is.null(outfile)) {
      utils::write.table(
        wtatage.final[[i + 3]],
        file = outfile,
        append = TRUE,
        row.names = FALSE,
        col.names = FALSE
      )
    }
  }
  endline <- data.frame(t(c(-9999, 1, 1, 1, 1, rep(0, dat_list$Nages))))
  if (!is.null(outfile)) {
    utils::write.table(
      endline,
      file = outfile,
      append = TRUE,
      row.names = FALSE,
      col.names = FALSE
    )
  }

  return(invisible(wtatage.final))
}
