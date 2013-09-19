#' Change length comps
#'
#' Take a data.SS_new file, resample the length compositions from the
#' expected values, and return a new file with the new length comp
#' samples. Samples can have dimensions, bins, sample sizes, and
#' distributions which are different than those coming from SS.
#'
#' @author Felipe Hurtado-Ferro (modified by Kotaro Ono)
#' @param infile SS data object from SS_readdat() in the r4ss package.
#' Make sure you select option "section=2"
#' @param outfile Name of the new file to be created. Path may be
#' global or local. Make sure to give extension .dat to the file name.
#' @param distribution Distribution to be used to sample the length
#' compositions. Options are "multinomial" and "dirichlet". it could
#' also be written in the form of a vector if the fishery and survey
#' composition data are not sampled using the same distribution
#' @param Nsamp Number of samples drawn from a multinomial
#' distribution, or precision for the Dirichlet distribution. This
#' could either be a single value if the sample size is the same for
#' all years but could also be a vector with its length equal to the
#' number of years with fishery and survey samples. For the latter,
#' fishery sample size precedes the survey sample size. \code{Nsamp}
#' only accepts numeric values and requires to be specified for both
#' the fishery and survey EVEN IF it is not used in the end. Default
#' to 100.
#' @param years vector of years for the fleet length comps.
#' @param svyears vector of years for the survey length comps.
#' @param lbin_vector Vector of length bins for the observations.
#' @param fish_lcomp, \code{TRUE} or \code{FALSE}. This indicates
#' whether you want to keep the fishery length comp data at all.
#' Defaults to \code{TRUE}.
#' @param sv_lcomp, \code{TRUE} or \code{FALSE}. This indicates
#' whether you want to keep the survey lcomp data at all. Defaults to
#' \code{TRUE}
#' @param cpar \code{c} parameter scaling the variance of the
#' Dirichlet and Multinomial distributions.
#'
#' @export
#' @examples
#' \dontrun{
#' d <- system.file("extdata", package = "ss3sim")
#' f_in <- paste0(d, "/example-om/data.ss_new")
#' infile <- r4ss::SS_readdat(f_in, section = 2, verbose = FALSE)
#'
#' # Generate a .dat file with the same dimensions as the original
#' # 'infile'
#' change_lcomp(infile, outfile = "newdat.dat")
#'
#' # Generate a .dat file with a smaller sample size
#' change_lcomp(infile, outfile = "newdat.dat", Nsamp = 20)
#'
#' # Generate a .dat file with a shorter time series
#' change_lcomp(infile, outfile = "newdat.dat", Nsamp = 100,
#'   years = 1980:2012)
#'
#' # Generate a .dat file using Dirichlet distributed samples
#' change_lcomp(infile, outfile = "newdat.dat", Nsamp = 100,
#'   distribution = "dirichlet")
#'
#' # Generate a .dat file using Dirichlet distributed samples for the
#' # fishery and Multinomial for the survey
#' change_lcomp(infile, outfile = "newdat.dat", Nsamp = 100,
#'   distribution = c("dirichlet", "multinomial"))
#'
#' # Generate a .dat file using Dirichlet distributed samples for the
#' # fishery with 50 samples and Multinomial for the survey with 100
#' # samples
#' change_lcomp(infile, outfile = "newdat.dat", distribution = c("dirichlet",
#'   "multinomial"), years = seq(1994, 2012, by = 2), svyears = seq(2003,
#'   2012), Nsamp = c(rep(50, 10), rep(100, 10)))
#'
#' # cleanup:
#' unlink("newdat.dat")
#' }

change_lcomp <- function(infile, outfile, distribution =
  "multinomial", Nsamp = 100,  years = NA, svyears = NA, lbin_vector =
  NA, fish_lcomp = TRUE, sv_lcomp = TRUE, cpar = 2, lencomp = NA){

  # Read the input file
  dat.file <- infile

  # Explicit inputs
  if(class(Nsamp)!="numeric"){
    stop("Nsamp must have a numeric input")
  }
  if (TRUE %in% is.na(years)) {
    years <- infile$lencomp$Yr[infile$lencomp$FltSvy == 1]
  }
  if (TRUE %in% is.na(svyears)) {
    svyears <- infile$lencomp$Yr[infile$lencomp$FltSvy == 2]
  }

  # Save the expected lencomps in another object to be modified (if
  # necessary)
  init.lcomp <- dat.file$lencomp

  # Check the length of the lbin_vector and adjust if necessary
  if (is.na(sum(lbin_vector)) == FALSE) {
    if (class(lbin_vector) != "numeric") {
      stop("lbin_vector must have a numeric input")
    }
    if (length(dat.file$lbin_vector) != length(lbin_vector)) {
      minobsl <- match(min(lbin_vector), dat.file$lbin_vector)
      maxobsl <- match(max(lbin_vector), dat.file$lbin_vector)
      init.lcomp <- cbind(init.lcomp[, 1:6], apply(init.lcomp[,
          7:(6 + minobsl)], 1, sum), init.lcomp[, (7 +
          minobsl):(maxobsl + 5)], apply(init.lcomp[, (maxobsl +
            5):length(init.lcomp[1, ])], 1, sum))
    }
    dat.file$lbin_vector <- lbin_vector
  }

  # Write the length comps
  DF.width <- length(init.lcomp[1, ])
  NDF.width <- length(dat.file$lbin_vector) + 6

  # Create a matrix that holds the length composition data (both
  # fishery and survey)
  new.lencomp <- array(0, dim = c(length(c(years, svyears)),
      NDF.width))
  new.lencomp[, 1] <- c(years, svyears)
  new.lencomp[, 2] <- 1
  new.lencomp[1:length(years), 3] <- 1
  new.lencomp[(length(years) + 1):length(c(years, svyears)),
    3] <- 2
  new.lencomp[, 4] <- 0
  new.lencomp[, 5] <- 0


  # Write the fishery length composition data in the matrix
  if(length(Nsamp)==1) {
    new.lencomp[,6] <- Nsamp
  }
  fllcomp <- subset(init.lcomp, init.lcomp[, 3] == 1)

  if (is.na(sum(years)) == FALSE) {
    for (it in 1:length(years)) {
      if (length(Nsamp) > 1) {
        new.lencomp[it, 6] <- Nsamp[it]
      }
      probs <- fllcomp[fllcomp[, 1] == years[it], 7:DF.width]
      probs <- as.numeric(probs)/sum(as.numeric(probs))
      if (length(distribution) == 1) {
        if (distribution == "multinomial") {
          new.lencomp[it, 7:NDF.width] <- rmultinom(1,
            new.lencomp[it, 6], probs)
        }
        if (distribution == "dirichlet") {
          lambda <- new.lencomp[it, 6]/cpar^2 - 1
          new.lencomp[it, 7:NDF.width] <- gtools::rdirichlet(1,
            as.numeric(probs) * lambda)
        }
      }
      if (length(distribution) > 1) {
        if (distribution[1] == "multinomial") {
          new.lencomp[it, 7:NDF.width] <- rmultinom(1,
            new.lencomp[it, 6], probs)
        }
        if (distribution[1] == "dirichlet") {
          lambda <- new.lencomp[it, 6]/cpar^2 - 1
          new.lencomp[it, 7:NDF.width] <- gtools::rdirichlet(1,
            as.numeric(probs) * lambda)
        }
      }
    }
  }

  # Write the survey length composition data in the matrix
  svlcomp <- subset(init.lcomp, init.lcomp[, 3] == 2)
  if (is.na(sum(svyears)) == FALSE) {
    for (it in (length(years) + 1):length(c(years, svyears))) {
      if (length(Nsamp) > 1) {
        new.lencomp[it, 6] <- Nsamp[it]
      }
      probs <- svlcomp[svlcomp[, 1] == c(years, svyears)[it],
        7:DF.width]
      probs <- as.numeric(probs)/sum(as.numeric(probs))
      if (length(distribution) == 1) {
        if (distribution == "multinomial") {
          new.lencomp[it, 7:NDF.width] <- rmultinom(1,
            new.lencomp[it, 6], probs)
        }
        if (distribution == "dirichlet") {
          lambda <- new.lencomp[it, 6]/cpar^2 - 1
          new.lencomp[it, 7:NDF.width] <- gtools::rdirichlet(1,
            as.numeric(probs) * lambda)
        }
      }
      if (length(distribution) > 1) {
        if (distribution[2] == "multinomial") {
          new.lencomp[it, 7:NDF.width] <- rmultinom(1,
            new.lencomp[it, 6], probs)
        }
        if (distribution[2] == "dirichlet") {
          lambda <- new.lencomp[it, 6]/cpar^2 - 1
          new.lencomp[it, 7:NDF.width] <- gtools::rdirichlet(1,
            as.numeric(probs) * lambda)
        }
      }
    }
  }

  # To convert the matrix of data into data.frame
  new.lencomp <- as.data.frame(new.lencomp)
  names(new.lencomp) <- c(names(dat.file$lencomp)[1:6], paste("l",
      dat.file$lbin_vector, sep = ""))

  # To keep or not to keep the length comp from fishery and survey
  if (fish_lcomp == FALSE) {
    new.lencomp <- new.lencomp[new.lencomp$FltSvy != 1, ]
  }
  if (sv_lcomp == FALSE) {
    new.lencomp <- new.lencomp[new.lencomp$FltSvy != 2, ]
  }

  dat.file$lencomp <- new.lencomp

  if (dim(new.lencomp)[1] == 0) {
    dat.file$lencomp <- data.frame("#")
  }

  # To calculate the final sum of years
  dat.file$N_lencomp <- length(new.lencomp[, 1])

  # Write the modified file
  r4ss::SS_writedat(datlist = dat.file, outfile = outfile,
    overwrite = TRUE)
}
