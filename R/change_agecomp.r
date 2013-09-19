#' Resample age composition data
#'
#' Take a \code{data.SS_new} file, resample the age compositions from
#' the expected values, and return a new file with the new age
#' composition samples. Samples can have dimensions, bins, sample
#' sizes, and distributions which are different than those coming from
#' SS.
#'
#' @author Roberto Licandeo, Felipe Hurtado-Ferro
#' @param infile SS data object from \code{SS_readdat()} in the r4ss
#' package. Make sure you select option \code{section = 2}
#' @param outfile Name of the new file to be created. Make sure to
#' give the extension \code{.dat} to the file name.
#' @param distribution Distribution to be used to sample the length
#' compositions. Options are \code{"multinomial"} and
#' \code{"dirichlet"}
#' @param Nsamp Number of samples drawn from a multinomial
#' distribution, or precision for the Dirichlet distribution. This
#' could either be a single value if the sample size is the same for
#' all years but could also be a vector with its length equal to the
#' number of years with fishery and survey samples. For the latter,
#' fishery sample size precedes the survey sample size. \code{Nsamp}
#' only accepts numeric values and requires to be specified for both
#' the fishery and survey EVEN IF it is not used in the end. Default
#' to 100.
#' @param N_agebins Number of age bins.
#' @param years Vector of years for the fleet age comps.
#' @param svyears Vector of years for the survey age comps.
#' @param agecomp Matrix of age comps.
#' @param fish_agecomp, \code{TRUE} or \code{FALSE}; default to
#' \code{TRUE}. Sets whether to use or not use the fishery age
#' composition data.
#' @param sv_agecomp, \code{TRUE} or \code{FALSE}; default to
#' \code{TRUE.} Sets whether to use or not use the survey age
#' composition data.
#' @param cpar \code{c} parameter scaling the variance of the
#' Dirichlet and Multinomial distributions.
#' @examples
#' \dontrun{
#' d <- system.file("extdata", package = "ss3sim")
#' f_in <- paste0(d, "/example-om/data.ss_new")
#' infile <- r4ss::SS_readdat(f_in, section = 2, verbose = FALSE)
#'
#' # Generate a .dat file with the same dimensions as the 'infile'
#' change_agecomp(infile, outfile = "newdat.dat")
#'
#' # Generate a .dat file with a smaller sample size
#' change_agecomp(infile, outfile = "newdat.dat", Nsamp=20)
#'
#' # Generate a .dat file with a shorter time series
#' change_agecomp(infile, outfile = "newdat.dat", Nsamp = 100,
#'   years = 1980:2012)
#'
#' # Generate a .dat file using Dirichlet distributed samples
#' change_agecomp(infile, outfile = "newdat.dat", Nsamp = 100,
#'   distribution = "dirichlet")
#'
#' # Generate a .dat file using Dirichlet distributed samples for the
#' # fishery and Multinomial for the survey
#' change_agecomp(infile, outfile = "newdat.dat", Nsamp = 100,
#'   distribution = c("dirichlet", "multinomial"))
#'
#' # Generate a .dat file using Dirichlet distributed samples for the
#' # fishery with 50 samples and Multinomial for the survey with 100
#' # samples
#' change_agecomp(infile, outfile = "newdat.dat", distribution =
#'   c("dirichlet", "multinomial"), years = seq(1994, 2012, by = 2),
#'   svyears = seq(2003, 2012), Nsamp = c(rep(50, 10), rep(100, 10)))
#'
#' # cleanup:
#' unlink("newdat.dat")
#' }
#' @export

change_agecomp <- function(infile, outfile, distribution =
  "multinomial", Nsamp = NA, years = NA, svyears = NA, fish_agecomp =
  TRUE, sv_agecomp = TRUE, N_agebins = NA, agecomp
  = NA, cpar = 2){

  dat.file <- infile
  if (is.na(N_agebins) == FALSE) {
    if (class(N_agebins) != "numeric")
      stop("N_agebins must have a numeric input")
    dat.file$N_agebins <- N_agebins
  }
  if (is.na(agecomp) == FALSE) {
    if (class(agecomp) != "numeric")
      stop("agecomp must have a numeric input")
    new.agecomp <- agecomp
  }
  if (is.na(sum(Nsamp)) == TRUE) {
    Nsamp <- dat.file$agecomp[, "Nsamp"]
  }

  # Save the expected lencomps in another object to be modified (if
  # necessary)
  init.agecomp <- dat.file$agecomp

  # Write the age comps
  DF.width <- length(init.agecomp[1,])     #Width for the comp matrix
  NDF.width <- length(dat.file$agebin_vector)+9

  if(is.na(agecomp)==TRUE) {
    new.agecomp <- array(0,dim=c(length(c(years,svyears)),NDF.width))  ## create age matrix
    new.agecomp[,1] <- c(years,svyears)                                ## fill Yr
    new.agecomp[,2] <- 1                                               ## fill Seas
    new.agecomp[1:length(years),3] <- 1                                ## fill FltSvy  =1
    new.agecomp[(length(years)+1):length(c(years,svyears)),3] <- 2     ## fill FltSvy  =2
    new.agecomp[,4] <- 0                                               ## Gender (default = 0)
    new.agecomp[,5] <- 0                                               ## Part (default = 0)
    new.agecomp[,6] <- 1                                               ## Ageerr (default = 0)
    new.agecomp[,7] <- -1                                             ## Lbin_lo (default = -1)
    new.agecomp[,8] <- -1                                             ## Lbin_hi (default = -1)
    if (length(Nsamp) ==1) new.agecomp[,9] <- Nsamp
    flacomp <- subset(init.agecomp,init.agecomp[,3]==1)
    if (is.na(sum(years)) == FALSE) {
      for (it in 1:length(years)) {
        if (length(Nsamp) > 1) {
          new.agecomp[it, 9] <- Nsamp[it]
        }
        probs <- flacomp[flacomp[, 1] == years[it], 10:DF.width]
        probs <- as.numeric(probs)/sum(as.numeric(probs))
        if (length(distribution) == 1) {
          if (distribution == "multinomial") {
            new.agecomp[it, 10:NDF.width] <- rmultinom(1,
              new.agecomp[it, 9], probs)
          }
          if (distribution == "dirichlet") {
            lambda <- new.agecomp[it, 9]/cpar^2 - 1
            new.agecomp[it, 10:NDF.width] <- gtools::rdirichlet(1,
              as.numeric(probs) * lambda)
          }
        }
        if (length(distribution) > 1) {
          if (distribution[1] == "multinomial") {
            new.agecomp[it, 10:NDF.width] <- rmultinom(1,
              new.agecomp[it, 9], probs)
          }
          if (distribution[1] == "dirichlet") {
            lambda <- new.agecomp[it, 9]/cpar^2 - 1
            new.agecomp[it, 10:NDF.width] <- gtools::rdirichlet(1,
              as.numeric(probs) * lambda)
          }
        }
      }
    }

    svagecomp <- subset(init.agecomp, init.agecomp[, 3] == 2)
    if (is.na(sum(svyears)) == FALSE) {
      for (it in (length(years) + 1):length(c(years, svyears))) {
        if (length(Nsamp) > 1) {
          new.agecomp[it, 9] <- Nsamp[it]
        }
        probs <- svagecomp[svagecomp[, 1] == c(years, svyears)[it],
          10:DF.width]
        probs <- as.numeric(probs)/sum(as.numeric(probs))
        if (length(distribution) == 1) {
          if (distribution == "multinomial") {
            new.agecomp[it, 10:NDF.width] <- rmultinom(1,
              new.agecomp[it, 9], probs)
          }
          if (distribution == "dirichlet") {
            lambda <- new.agecomp[it, 9]/cpar^2 - 1
            new.agecomp[it, 10:NDF.width] <- gtools::rdirichlet(1,
              as.numeric(probs) * lambda)
          }
        }
        if (length(distribution) > 1) {
          if (distribution[2] == "multinomial") {
            new.agecomp[it, 10:NDF.width] <- rmultinom(1,
              new.agecomp[it, 9], probs)
          }
          if (distribution[2] == "dirichlet") {
            lambda <- new.agecomp[it, 9]/cpar^2 - 1
            new.agecomp[it, 10:NDF.width] <- gtools::rdirichlet(1,
              as.numeric(probs) * lambda)
          }
        }
      }
    }
  }

  new.agecomp <- as.data.frame(new.agecomp)
  names(new.agecomp) <- c(names(dat.file$agecomp)[1:9], paste("a",
      dat.file$agebin_vector, sep = ""))

  #To keep or not to keep the length comp from fishery and survey
  if(fish_agecomp==FALSE) {
    new.agecomp <- new.agecomp[new.lencomp$FltSvy != 1, ]
  }
  if(sv_agecomp==FALSE) {
    new.agecomp <- new.agecomp[new.lencomp$FltSvy != 2, ]
  }

  dat.file$agecomp <- new.agecomp

  if(dim(new.agecomp)[1]==0) {
    dat.file$agecomp = data.frame("#")
  }

  #To calculate the final sum of years
  dat.file$N_agecomp <- length(new.agecomp[,1])

  #Write the modified file
  r4ss::SS_writedat(datlist = dat.file, outfile = outfile,
    overwrite = TRUE)
}
