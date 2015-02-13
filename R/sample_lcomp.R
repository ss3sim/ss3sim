#' Sample length compositions from expected values
#'
#' Take a \code{data.SS_new} file containing expected values and sample to
#' create observed length compositions which are then written to file for use by
#' the estimation model.
#' If used with \code{\link{run_ss3sim}} the case file should be named
#' \code{lcomp}. A suggested (default) case letter is \code{D} for data.
#'
#' @author Cole Monnahan and Kotaro Ono; modified from a version by Roberto
#'   Licandeo and Felipe Hurtado-Ferro
#'
#' @template lcomp-agecomp-index
#' @template lcomp-agecomp
#' @template casefile-footnote
#' @template sampling-return
#' @importFrom r4ss SS_writedat
#'
#' @examples
#' d <- system.file("extdata", package = "ss3sim")
#' f_in <- paste0(d, "/example-om/data.ss_new")
#' datfile <- r4ss::SS_readdat(f_in, section = 2, verbose = FALSE)
#' datfile <- change_fltname(datfile)
#'
#' ## Generate with constant sample size across years
#' ex1 <- sample_lcomp(datfile=datfile, outfile="test1.dat", fleets=c(1,2),
#'                     Nsamp=list(100,50), years=list(seq(1994, 2012, by=2),
#'                                             2003:2012), write_file = FALSE)
#'
#' ## Generate with varying Nsamp by year for first fleet
#' ex2 <- sample_lcomp(datfile=datfile, outfile="test2.dat", fleets=c(1,2),
#'                     Nsamp=list(c(rep(50, 5), rep(100, 5)), 50),
#'                     years=list(seq(1994, 2012, by=2),
#'                         2003:2012), write_file = FALSE)
#'
#' \dontrun{
#' ## Plot distributions for a particular year to compare multinomial
#' ## vs. overdispersed Dirichlet
#' temp.list <- temp.list2 <- list()
#' for(i in 1:40){
#'     temp.list[[i]] <-
#'       sample_lcomp(datfile=datfile, outfile="test1.dat", fleets=c(2), cpar=c(3),
#'                      Nsamp=list(100), years=list(1995),
#'                      write_file=FALSE)
#'     temp.list2[[i]] <-
#'         sample_lcomp(datfile=datfile, outfile="test1.dat", fleets=c(2),
#'                      cpar=c(NA), Nsamp=list(100), years=list(1995),
#'                      write_file=FALSE)
#' }
#' ## Organize the data for plotting
#' x1 <- reshape2::melt(do.call(rbind, temp.list)[,-(1:6)[-3]], id.vars="FltSvy")
#' x2 <- reshape2::melt(do.call(rbind, temp.list2)[,-(1:6)[-3]], id.vars="FltSvy")
#' op <- par(mfrow=c(2,1))
#' with(x1, boxplot(value~variable, las=2, ylim=c(0,.6), ylab="Proportion",
#'                  main="Overdispersed (cpar=3)",  xlab="length bin"))
#' temp <- as.numeric(subset(datfile$lencomp, Yr==1995 & FltSvy == 2)[-(1:6)])
#' points(temp/sum(temp), pch="-", col="red")
#' with(x2, boxplot(value~variable, las=2, ylim=c(0,.6), ylab="Proportion",
#'                  main="Multinomial", xlab="length bin"))
#' temp <- as.numeric(subset(datfile$lencomp, Yr==1995 & FltSvy == 2)[-(1:6)])
#' points(temp/sum(temp), pch="-", col="red")
#' par(op)
#' }
#'
#' @export
#' @family sampling functions

sample_lcomp <- function(datfile, outfile, fleets = c(1,2), Nsamp,
  years, cpar = 1, write_file = TRUE){

  is_ssdat_file(datfile)
  ## The new lcomp is mostly based on the old one so start with that
  lcomp <- datfile$lencomp
  ## Check inputs for errors
  Nfleets <- ifelse(is.null(fleets), 0, length(fleets))
  if(FALSE %in% (fleets %in% unique(lcomp$FltSvy)))
    stop(paste0("The specified fleet number does not match input file"))
  if(Nfleets!= 0 & class(Nsamp) != "list" | length(Nsamp) != Nfleets)
    stop("Nsamp needs to be a list of same length as fleets")
  if(Nfleets!= 0 & class(years) != "list" | length(years) != Nfleets)
    stop("years needs to be a list of same length as fleets")
  if (Nfleets>0){
    for(i in 1:Nfleets){
      if(length(Nsamp[[i]])>1 & length(Nsamp[[i]]) != length(years[[i]]))
        stop(paste0("Length of Nsamp does not match length of years for fleet ",
          fleets[i]))
    }
    if(length(cpar) == 1){
      ## If only 1 value provided, use it for all fleets
      cpar <- rep(cpar, times=Nfleets)
    } else if(length(cpar) != Nfleets){
      stop(paste0("Length of cpar (", length(cpar),
        ") needs to be length of fleets (", Nfleets,
        ") or 1"))
    }
  }
  ## End input checks

  ## Resample from the length data
  ## The general approach here is to loop through each row to keep
  ## (depends on years input) and resample depending on Nsamp and
  ## cvar. All these rows are then combined back together to form
  ## the final lcomp.
  newcomp.list <- list() # temp storlength for the new rows
  k <- 1
  ## Loop through each fleet
  if (Nfleets>0){
    for(i in 1:length(fleets)){
      fl <- fleets[[i]]
      if(!is.na(fl)){
        lcomp.fl <- lcomp[lcomp$FltSvy == fl & lcomp$Yr %in% years[[i]], ]
        if(length(years[[i]]) != nrow(lcomp.fl))
          stop(paste("A year specified in years was not found in the input",
                     "file for fleet", fl))
        lcomp.fl$Nsamp <- Nsamp[[i]]
        ## Now loop through each year and resample that row
        for(yr in years[[i]]) {
          newcomp <- lcomp.fl[lcomp.fl$Yr==yr, ]
          ## Replace expected values with sampled values
          ## First 1-9 cols aren't length bins so skip them
          probs <- as.numeric(newcomp[-(1:6)]/sum(newcomp[-(1:6)]))
          ## If cpar is NA this signifies to use the multinomial
          if(is.na(cpar[i])){
            newcomp[-(1:6)] <-
              rmultinom(1, size=newcomp$Nsamp, prob=probs)#/newcomp$Nsamp
          } else { # use Dirichlet
            lambda <- newcomp$Nsamp/cpar[i]^2 - 1
            if(lambda<0)
              stop(paste("Invalid Dirichlet parameter: Lambda=", lambda))
            newcomp[-(1:6)] <- gtools::rdirichlet(1,probs * lambda)
            ## Use the effective sample size when using Dirichlet
            effectiveN <- newcomp$Nsamp/cpar[i]^2
            newcomp$Nsamp <- effectiveN
          }
          newcomp.list[[k]] <- newcomp
          k <- k+1
        }
      }
    }
  }
  ## Combine new rows together into one data.frame
  if(Nfleets>0) newcomp.final <- do.call(rbind, newcomp.list)
  if(Nfleets==0) newcomp.final = data.frame("#")

  ## Build the new dat file
  newfile <- datfile
  newfile$lencomp <- newcomp.final
  if(Nfleets>0) newfile$N_lencomp <- nrow(newcomp.final)
  if(Nfleets==0) newfile$N_lencomp <- 0

  ## Write the modified file
  if(write_file)
    SS_writedat(datlist = newfile, outfile = outfile, overwrite = TRUE,
                verbose = FALSE)
  invisible(newfile)
}

#' (Depreciated) Sample length compositions from expected values
#'
#' \code{change_lcomp} is a depreciated function. Please use
#' \code{\link{sample_lcomp}} instead. \code{change_lcomp} will be removed
#' in the next major version.
#'
#' @param ... Arguments that get passed to \code{\link{sample_lcomp}}.
#'
#' @export

change_lcomp <- function(...) {
  warning(paste("change_lcomp is a depreciated function.",
    "Please use sample_lcomp instead. change_lcomp will",
    "be removed in the next major version."))
  sample_lcomp(...)
}
