#' Sample conditional age-at-length compositions from expected values
#'
#' Take a \code{data.SS_new} file containing expected values and sample to
#' create observed conditional length-at-age compositions 
#' which are then written to file for use by the estimation model.
#'
#' @author Kelli Faye Johnson
#'
#' @template lcomp-agecomp-index
#' @param yearxbin A list of lists indicating the length bins
#'   with samples for each fleet. Desired bins for each year of 
#'   included data should be available for each fleet. If bins
#'   do not change between years a single value for each fleet
#'   can be specified in the argument.
#' @param keep_agecomp A logical if age compositions data
#'   should be kept or removed entirely from the \code{.dat} file.
#'   \code{sample_ccomp} only works on the conditional age-at-length data
#'   and not on the age composition data. To sample the 
#'   composition data set \code{keep_agecomp} to \code{TRUE}
#'   and use \link{\code{sample_agecomp}}.
#'
#' @template sampling-return
#'
#' @examples
#' d <- system.file("extdata", package = "ss3sim")
#' f_in <- paste0(d, "/example-om/data.ss_new")
#' infile <- r4ss::SS_readdat(f_in, section = 2, verbose = FALSE)
#'
#' ## Turn off age comps by specifying fleets=NULL
#' sample_agecomp(infile=infile, outfile="test1.dat",
#'                fleets=NULL, cpar=c(5,NA), Nsamp=list(100,100),
#'                years=list(1995, 1995), write_file=FALSE)
#'
#' ## Generate with a smaller number of fleet taking samples
#' ex1 <- sample_agecomp(infile=infile, outfile="test1.dat", fleets=c(2),
#'                       Nsamp=list(c(10,50)), years=list(c(1999,2000)),
#'                       write_file=FALSE)
#'
#' ## Generate with varying Nsamp by year for first fleet
#' ex2 <- sample_agecomp(infile=infile, outfile="test2.dat", fleets=c(1,2),
#'                       Nsamp=list(c(rep(50, 5), rep(100, 5)), 50),
#'                       years=list(seq(1994, 2012, by=2),
#'                           2003:2012), write_file=FALSE)
#'
#' ## Run three  cases showing Multinomial, Dirichlet(1) and over-dispersed
#' ## Dirichlet for different levels of sample sizes
#' op <- par(mfrow = c(1,3))
#' for(samplesize in c(30, 100, 1000)){
#'     ex4 <- sample_agecomp(infile=infile, outfile="test4.dat", fleets=c(1,2),
#'                           Nsamp=list(samplesize, samplesize),
#'                           write_file = FALSE,
#'                           years=list(2000,2000), cpar=c(NA, 1))
#'     ex5 <- sample_agecomp(infile=infile, outfile="test5.dat", fleets=c(1,2),
#'                           Nsamp=list(samplesize, samplesize),
#'                           write_file = FALSE,
#'                           years=list(2000,2000), cpar=c(1, 1))
#'     ex6 <- sample_agecomp(infile=infile, outfile="test6.dat", fleets=c(1,2),
#'                           Nsamp=list(samplesize, samplesize),
#'                           write_file = FALSE,
#'                           years=list(2000,2000), cpar=c(5, 1))
#'     true <- subset(infile$agecomp, FltSvy==1 & Yr == 2000)[-(1:9)]
#'     true <- true/sum(true)
#'     plot(0:15, subset(ex4, FltSvy==1)[1,-(1:9)], type="b", ylim=c(0,1),
#'          col=1, xlab="Age", ylab="Proportion", main=paste("Sample size=",
#'          samplesize))
#'     legend("topright", legend=c("Multinomial", "Dirichlet(1)",
#'                                 "Dirichlet(5)", "Truth"),
#'            lty=1, col=1:4)
#'     lines((0:15), subset(ex5, FltSvy==1)[1,-(1:9)], type="b", col=2)
#'     lines((0:15), subset(ex6, FltSvy==1)[1,-(1:9)], type="b", col=3)
#'     lines((0:15), true, col=4, lwd=2)
#' }
#' par(op)
#' @seealso \code{\link{sample_lcomp}} \code{\link{sample_agecomp}}
#' @export
sample_ccomp <- function(infile, outfile, fleets = c(1, 2), 
                         years, yearxbin, cpar = 1, write_file = TRUE,
                         keep_agecomp = TRUE){

    agecomp <- infile$agecomp
    #Split comps and conditionals
    data <- subset(agecomp, Lbin_lo >= 0)

    ## Check inputs for errors
    if(substr_r(outfile, 4) != ".dat" & write_file)
        stop(paste0("outfile ", outfile, " needs to end in .dat"))
    Nfleets <- ifelse(is.null(fleets), 0, length(fleets))
    if(Nfleets > 0 & FALSE %in% (fleets %in% unique(data$FltSvy)))
        stop(paste0("The specified fleet number does not match input file"))
    if(Nfleets != 0 & (class(Nsamp) != "list" | length(Nsamp) != Nfleets))
        stop("Nsamp needs to be a list of same length as fleets")
    if(Nfleets != 0 & (class(years) != "list" | length(years) != Nfleets))
        stop("years needs to be a list of same length as fleets")

    ## If no fleets are specified then skip these
    if (Nfleets>0){
        for(i in 1:Nfleets){
            if(length(yearxbin[[i]]) > 1 & length(yearxbin[[i]]) != length(years[[i]]))
                stop(paste0("Length of Nsamp does not match length of years for",
                  "fleet ", fleets[i]))
        }
        if(length(cpar) == 1){
            ## If only 1 value provided, use it for all fleets
            cpar <- rep(cpar, times = Nfleets)
        } else if(length(cpar) != Nfleets){
            stop(paste0("Length of cpar (", length(cpar),
                        ") needs to be length of fleets (", Nfleets,
                        ") or 1"))
        }
    }
    ## End input checks

    ## Steps
    #1. Determine the number of samples per length bin from the TRUE data
    Nsamp <- #vector of samples for each bin
    #2. Subset for fleet x years
    newcomp.list <- list() # temp storage for the new rows
    k <- 1
    for(f in seq_along(fleets)) {
        temp <- subset(data, FltSvy == fleets[f] & Yr %in% years[[f]])
        for(y in seq_along(years[[f]])){
          use <- subset(temp, Lbin_lo %in% yearxbin[[f]][[y]])
          #resample data
          for(r in seq(nrow(use))) {
              newcomp <- use[r, ]
              newcomp$Nsamp <- Nsamp[match(newcomp$Lbin_lo, names(Nsamp))]
              ## Replace expected values with sampled values
              ## First 1-9 cols aren't age bins so skip them
              probs <- as.numeric(newcomp[-(1:9)]/sum(newcomp[-(1:9)]))
              #If cpar is NA use the multinomial
              if(is.na(cpar[f])) {
                  newcomp[-(1:9)] <- rmultinom(1, size = newcomp$Nsamp,
                    prob = probs) / newcomp$Nsamp
              } else {
                  lambda <- newcomp$Nsamp/cpar[f]^2 - 1
                  if(lambda < 0){
                    stop(paste("Invalid Dirichlet parameter: Lambda=",
                      lambda))
                  }
                  newcomp[-(1:9)] <- gtools::rdirichlet(1, probs * lambda)
                  ## use the effective sample size when using Dirichlet
                  newcomp$Nsamp <- newcomp$Nsamp/cpar[i]^2
                }
                newcomp.list[[k]] <- newcomp
                k <- k+1
          }
        }
    }
    ## Combine new rows together into one data.frame
    if(Nfleets > 0) newcomp.final <- do.call(rbind, newcomp.list)
    if(Nfleets == 0) newcomp.final = data.frame("#")

    ## Build the new dat file
    newfile <- infile
    if(keep_agecomp) {
        newcomp.final <- rbind(subset(agecomp, Lbin_lo < 0),
                               newcomp.final)
        newfile$agecomp <- newcomp.final
    } else {
        newfile$agecomp <- newcomp.final
    }
    if(Nfleets > 0) newfile$N_agecomp <- nrow(newcomp.final)
    if(Nfleets == 0 & keep_agecomp == FALSE) newfile$N_agecomp <- 0
    if(Nfleets == 0 & keep_agecomp == TRUE) newfile$N_agecomp <- nrow(newcomp.final)

    ## Write the modified file
    if(write_file)
        r4ss::SS_writedat(datlist = newfile, outfile = outfile, overwrite = T)
    return(invisible(newcomp.final))
}
