#' Sample empirical weight-at-age data
#'
#' In Stock Synthesis, empirical weight-at-age data can be used to
#' read empirical body weight for the population from each fleet.
#' This data removes the use of growth parameters from the EM because
#' weights are assigned to each age internally rather than from the growth parameters,
#' from which spawning biomass/fecundity can be determined.
#' These values are not data in the sense they have a likelihood but are generated from samples.
#' Sampling empirical weight-at-age data from the expected values takes many steps.
#'
#' @details
#' The steps for sampling empirical weight-at-age are as follows:
#' * Sample from the expected ages to get realistic proportions for
#'   the number of fish in each age bin.
#' * Use the mean size-at-age and coefficient of variation for growth to
#'   generate random samples of size,
#'   which are then converted to weight and averaged to get mean weight-at-age values.
#' * Fill in missing ages and years.
#' * Write the information to the appropriate files.
#' * Turn on weight-at-age data in Stock Synthesis by setting the maturity option to 5.
#'
#' @author Cole Monnahan, Allan Hicks, Peter Kuriyama
#'
#' @param wta_file_in The file to read weight-at-age from. Specifically to get the
#'   age-0 weight-at-age. This is typically `wtatage.ss_new`.
#' @param ctl_file_in A path to the control file, output from an OM, containing
#'   the OM parameters for growth and weight/length relationship. These values
#'   are used to determine the uncertainty about weight for fish sampled in each
#'   age bin. Commonly `control.ss_new`
#' @param fill_fnc A function to fill in missing values (ages and years). The
#'   resulting weight-at-age file will have values for all years and ages.One
#'   function is [fill_across()].
#' @param cv_wtatage A user specified coefficient of variation (CV) for growth.
#'   Default is `NULL`.
#' @return A modified `.wtatage.ss` file if `!is.null(outfile)`. A list
#'   object containing the modified `.wtatage.ss` file is returned invisibly.
#' @template lcomp-agecomp-index
#' @template dat_list
#' @template outfile
#' @seealso
#' * [fill_across()]
#' * [ss3sim_base()]
#' @family sampling functions
#' @export
#
# # #For Debugging
# setwd('/Users/peterkuriyama/School/Research/capam_growth/sample_wtatage_test/')
# # source('fill_across.r')
# #
# wta_file_in <- "om/wtatage.ss_new"
# outfile <- "em/wtatage.ss"
# datfile <- "em/ss3.dat"
# ctl_file_in <- "om/control.ss_new"
# years <- list(seq(2, 100, 1), seq(2, 100, 1))
# fill_fnc <- fill_across
# fleets <- list(1, 2)
# cv_wtatage <- .5
#
# dat_list <- r4ss::SS_readdat(file = datfile, verbose = FALSE)
# test <- sample_wtatage(wta_file_in = wta_file_in, outfile = outfile, dat_list = dat_list,
#     ctl_file_in = ctl_file_in, years = years, fill_fnc = fill_across,
#     fleets = fleets, cv_wtatage = cv_wtatage)
#

sample_wtatage <- function(wta_file_in, outfile, dat_list, ctl_file_in,
                           years, fill_fnc = fill_across, fleets,
                           cv_wtatage = NULL) {
  ## fill_type: specify type of fill, fill zeroes with first row? annual interpolation?
  ## Age Interpolation?
  ## A value of NULL for fleets signifies to turn this data off in the
  ## EM. So quit early and in ss3sim_base do NOT turn wtatage on using
  ## the maturity function
  if (is.null(cv_wtatage)) stop("specify cv_wtatage")

  cat("cv_wtatage is", cv_wtatage, "\n")
  if (is.null(fleets)) {
    return(NULL)
  }

  ### ACH: Because you always have to have year 100, you may want to check for duplicates
  # years <- years[!duplicated(years)]
  #----------------------------------------------------------------------------

  agecomp <- dat_list$agecomp[dat_list$agecomp$Lbin_lo == -1, ]

  cat("sample size is ", unique(agecomp$Nsamp), "\n")
  agebin_vector <- dat_list$agebin_vector

  mlacomp <- dat_list$MeanSize_at_Age_obs
  if (is.null(mlacomp)) stop("No mean length-at-age data found in dat_list")
  ## Read in the control file
  ctl <- r4ss::SS_parlines(ctl_file_in)
  ## Read in the file and grab the expected values
  wta_file_in <- readLines(wta_file_in)

  ## Remove double spaces, which Stock Synthesis writes in the 7th column
  wta_file_in <- gsub("  ", replacement = " ", x = wta_file_in)
  xx <- grep(x = wta_file_in, "#yr seas gender growpattern birthseas fleet")
  if (length(xx) != 1) stop("Failed to read in wtatage file")
  header <- unlist(strsplit(wta_file_in[xx], " "))
  header[-(1:6)] <- paste("age", header[-(1:6)], sep = "")
  ## It appears the first three lines need to be there for some
  ## reason. ****TODO Peter****: fix this if need be??

  wtatage <- wta_file_in[(xx + 1):length(wta_file_in)]
  wtatage <- as.data.frame(matrix(as.numeric(unlist(strsplit(wtatage, split = " "))),
    nrow = length(wtatage), byrow = TRUE
  ))
  names(wtatage) <- gsub("#", replacement = "", x = header)
  wtatage$yr <- abs(wtatage$yr)
  if (2 %in% unique(wtatage$fleet) == FALSE) {
    ones <- wtatage[wtatage$fleet == 1, ]
    twos <- ones
    twos$fleet <- 2
    wtatage <- rbind(wtatage, twos)
  }

  age0 <- wtatage[!duplicated(wtatage$fleet), c("fleet", "age0")]
  wtatage.new.list <- list(1, 2) # temp storage for the new rows

  #----------------------------------------------------------------------------
  # start sampling
  # Pull wtatage for fleets -2, -1, and 0 from OM for now.
  #-2 is age-specific fecundity * maturity
  #-1 is population wt-at-age in middle of season
  # 0 is population wt-at-age in beginning of season2
  unsampled.wtatage <- list(1, 2, 3)
  unsampled.wtatage[[1]] <- wtatage[which(wtatage$fleet == -2), ]
  unsampled.wtatage[[2]] <- wtatage[which(wtatage$fleet == -1), ]
  unsampled.wtatage[[3]] <- wtatage[which(wtatage$fleet == 0), ]

  # Change all years to negatives so it will work with ss
  unsampled.wtatage <- lapply(
    unsampled.wtatage,
    function(x) {
      x$yr <- -x$yr
      return(x)
    }
  )

  # Start Sampling other fleets
  # fl <- 1
  # yr <- 1
  for (fl in fleets) { # fleets must be 1:Nfleets
    # set up wtatage matrix of sampled years
    wtatage.new.list[[fl]] <- as.data.frame(matrix(NA, nrow = length(years[[fl]]), ncol = ncol(wtatage)))
    names(wtatage.new.list[[fl]]) <- names(wtatage)
    row.names(wtatage.new.list[[fl]]) <- as.character(years[[fl]])

    # ===============Loop over Years Sampled
    if (length(years[[fl]]) == 1) { # copy wtatage matrix from designated fleet
      if (fl <= years[[fl]]) stop("You must designate an earlier fleet to copy from.\n")
      wtatage.new.list[[fl]] <- wtatage.new.list[[years[[fl]]]]
      wtatage.new.list[[fl]]$fleet <- fl
    } else {
      for (yr in years[[fl]]) {
        #----------------------------------------------------------------------------------------------------
        # Step 1, draw from true age distributions
        agecomp.temp <- agecomp[agecomp$year == yr & agecomp$fleet == fl, ]
        # ACH: I'm going with the motto of think about it. Why enter a year for wtatage when you do not have data?
        if (nrow(agecomp.temp) == 0) {
          stop("No age comp observations for year", yr, "and fleet", fl, "\n")
        }

        ## Get the true age distributions
        age.means <- as.numeric(agecomp.temp[-(1:9)])
        age.Nsamp <- as.numeric(agecomp.temp$Nsamp)

        #----------------------------------------------------------------------------------------------------
        # Step 2, determine # of fish in sample per bin
        # Use a multinomial here but may need to have dirichlet or option to toggle between the two
        age.samples <- stats::rmultinom(n = 1, size = age.Nsamp, prob = age.means)

        #----------------------------------------------------------------------------------------------------
        # Step 3, use mean length-at-age to sample with normal/lognormal and user-specified cv
        # first define mean length-at-age

        # Change fleets name to fleet to keep everything consistent
        names(mlacomp)[3] <- "fleet"
        # [which(names(mlacomp) == 'fleet')]
        mla.means <- as.numeric(mlacomp[
          mlacomp$year == yr & mlacomp$fleet == fl,
          paste0("a", agebin_vector)
        ])

        # CV.growth <- ctl[ctl$Label=="CV_young_Fem_GP_1", "INIT"]
        CV.growth <- cv_wtatage # User-Specified

        # Define growth parameters
        Wtlen1 <- ctl[ctl$Label == "Wtlen_1_Fem", "INIT"]
        Wtlen2 <- ctl[ctl$Label == "Wtlen_2_Fem", "INIT"]
        sds <- mla.means * CV.growth

        # Sample: I used a for loop to keep things understandable for me. could use apply also

        # create empty list to store lengths and weights
        # todo: could be a bug here in seq(seq_len())
        lengths.list <- as.list(seq(seq_len(nrow(age.samples))))
        weights.list <- lengths.list

        # fill in list by sampling from normal distribution
        for (ii in seq_len(nrow(age.samples)))
        {
          lengths.list[[ii]] <- suppressWarnings(stats::rnorm(n = age.samples[ii], mean = mla.means[ii], sd = sds[ii]))

          # Step 4, convert lengths to weights with no error
          weights.list[[ii]] <- Wtlen1 * lengths.list[[ii]]^Wtlen2
        }

        # step 5, calculate new mean weight at age
        samp.wtatage <- sapply(weights.list, mean)

        # concatenate everything
        prefix <- wtatage[wtatage$yr == yr & wtatage$fleet == 1, 1:5] # I used fleet=1 because wtatage_new only outputs fleet 1
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

  # fill in missing values
  ## ## Do we need to check whether there was only one fleet? Or force this?
  ## if(!is.data.frame(wtatage.new.list[[1]])){
  ##     wtatage.new.list[[1]] <- wtatage.new.list[[2]]
  ##     wtatage.new.list[[1]]$fleet <- 1
  ## }
  ## if(!is.data.frame(wtatage.new.list[[2]])){
  ##     wtatage.new.list[[2]] <- wtatage.new.list[[1]]
  ##     wtatage.new.list[[2]]$fleet <- 2
  ## }

  wtatage.complete <- lapply(wtatage.new.list, fill_fnc, minYear = dat_list$styr, maxYear = dat_list$endyr)

  # wtatage.new.list[[1]]
  # temp.wtatage <- wtatage.new.list[[1]]

  # filled.wtatage <- fill_fnc(mat = temp.wtatage, minYear = dat_list$styr, maxYear = dat_list$endyr)

  # Manually check
  # utils::write.csv(temp.wtatage, file = 'wtatage1_gaps.csv')
  # utils::write.csv(filled.wtatage, file = 'wtatage1_filled.csv')
  # save(wtatage.complete, file = '/Users/peterkuriyama/School/Research/capam_growth/Empirical/runs/wtatage.complete.Rdata')

  # wtatage.complete[[dat_list$Nfleet+1]] <- wtatage.complete[[dat_list$Nfleet]]
  # print(dat_list$Nfleet+1)

  # fltNeg1 <- fltZero <- wtatage.complete[[dat_list$Nfleet+1]]  #first survey
  # fltNeg1 <- fltZero <- wtatage.complete[[dat_list$Nfleet+1]]  #first survey
  # fltNeg1$fleet <- -1
  # fltZero$fleet <- 0



  #----------------------------------------------------------------------------------------------------
  # Comment this out because I just copied from the original file
  # mat.fn <- function(x,age) {
  #     omega3 <- x[1]
  #     omega4 <- x[2]
  #     den <- 1+exp(omega3*(age-omega4))
  #     return(1/den)
  # }

  # # ctl[ctl$Label=="Mat_slope_Fem","INIT"]
  # # ctl[ctl$Label=="Mat50%_Fem","INIT"]
  # # # print(c(ctl[ctl$Label=="Mat_slope_Fem","INIT"],ctl[ctl$Label=="Mat50%_Fem","INIT"]))

  # matAtAge <- mat.fn(c(ctl[ctl$Label=="Mat_slope_Fem","INIT"],ctl[ctl$Label=="Mat50%_Fem","INIT"]),agebin_vector)
  # matAtAge <- c(0, matAtAge)

  # # fecund <- matAtAge * wtatage.complete[[dat_list$Nfleet+1]][,-(1:6)]
  # #Adjusted Matrix Multiplication
  # fecund <- t(apply(wtatage.complete[[dat_list$Nfleet+1]][,-(1:6)], 1, FUN = function(x) matAtAge * x))

  # fecund <- cbind(wtatage.complete[[dat_list$Nfleet+1]][,1:6],fecund)
  # fecund$fleet <- -2

  # Nlines <- nrow(fecund)+nrow(fltNeg1)+nrow(fltZero)
  Nlines <- sum(unlist(lapply(unsampled.wtatage, nrow)))
  Nlines <- Nlines + sum(unlist(lapply(wtatage.complete, nrow)))

  # Nlines <- Nlines + sum(unlist(lapply(wtatage.complete,nrow)))

  ## write wtatage.ss file
  if (is.null(outfile)) cat(dat_list$Nages, "# Maximum Age\n", file = outfile, append = TRUE)

  # loop through the various matrices and build up wtatage.final while doing it
  wtatage.final <- list()

  if (is.null(outfile)) cat("#fleet -2, fecundity\n", file = outfile, append = TRUE)
  wtatage.final[[1]] <- unsampled.wtatage[[1]]

  # wtatage.final[[1]] <- fecund
  # wtatage.final[[1]]$yr <- -1 * wtatage.final[[1]]$yr
  if (!is.null(outfile)) utils::write.table(unsampled.wtatage[[1]], file = outfile, append = TRUE, row.names = FALSE, col.names = FALSE)

  if (!is.null(outfile)) cat("\n#fleet -1\n", file = outfile, append = TRUE)
  # wtatage.final[[2]] <- fltNeg1
  wtatage.final[[2]] <- unsampled.wtatage[[2]]
  # wtatage.final[[2]]$yr <- -1 * wtatage.final[[2]]$yr
  if (!is.null(outfile)) utils::write.table(unsampled.wtatage[[2]], file = outfile, append = TRUE, row.names = FALSE, col.names = FALSE)

  if (!is.null(outfile)) cat("\n#fleet 0\n", file = outfile, append = TRUE)
  # wtatage.final[[3]] <- fltZero
  wtatage.final[[3]] <- unsampled.wtatage[[3]]
  # wtatage.final[[3]]$yr <- -1 * wtatage.final[[3]]$yr
  if (!is.null(outfile)) utils::write.table(unsampled.wtatage[[3]], file = outfile, append = TRUE, row.names = FALSE, col.names = FALSE)

  # loop through fleets
  for (i in fleets) {
    if (!is.null(outfile)) cat("\n#fleet", i, "\n", file = outfile, append = TRUE)
    wtatage.final[[i + 3]] <- wtatage.complete[[i]]
    wtatage.final[[i + 3]]$yr <- -1 * wtatage.final[[i + 3]]$yr
    if (!is.null(outfile)) utils::write.table(wtatage.final[[i + 3]], file = outfile, append = TRUE, row.names = FALSE, col.names = FALSE)
  }
  endline <- data.frame(t(c(-9999, 1, 1, 1, 1, rep(0, dat_list$Nages))))
  if (!is.null(outfile)) {
    utils::write.table(endline,
      file = outfile, append = TRUE,
      row.names = FALSE, col.names = FALSE
    )
  }

  return(invisible(wtatage.final))
}
