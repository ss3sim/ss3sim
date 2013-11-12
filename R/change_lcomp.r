#' Sample length compositions from expected values
#'
#' Take a \code{data.SS_new} file containing expected values and sample to
#' create observed length compositions which are then written to file for use by
#' the EM.
#'
#' @author Cole Monnahan and Kotaro Ono; modified from a version by Roberto
#' Licandeo and Felipe Hurtado-Ferro
#'
#' @template lcomp-agecomp-index
#' @template lcomp-agecomp
#' @param lengthbin_vector A numeric vector giving the new length bins
#' to use. \code{lengthbin_vector} must be within the [min;max] of
#' population bin. This feature allows dynamic binning by the user,
#' but is not fully tested. Users should consult the vignette and
#' carefully check the function bins the data as desired before
#' proceeding with simulations.
#'
#' @template sampling-return
#'
#' @examples
#' d <- system.file("extdata", package = "ss3sim")
#' f_in <- paste0(d, "/example-om/data.ss_new")
#' infile <- r4ss::SS_readdat(f_in, section = 2, verbose = FALSE)
#'
#' ## Generate with constant sample size across years
#' ex1 <- change_lcomp(infile=infile, outfile="test1.dat", fleets=c(1,2),
#'                     Nsamp=list(100,50), years=list(seq(1994, 2012, by=2),
#'                                             2003:2012))
#'
#' ## Generate with varying Nsamp by year for first fleet
#' ex2 <- change_lcomp(infile=infile, outfile="test2.dat", fleets=c(1,2),
#'                     Nsamp=list(c(rep(50, 5), rep(100, 5)), 50),
#'                     years=list(seq(1994, 2012, by=2),
#'                         2003:2012))
#'
#' ## Generate with constant sample size across years AND with different length
#' ## bins (same as ex1 except bins)
#' ex3 <- change_lcomp(infile=infile, outfile="test3.dat", fleets=c(1,2),
#'                     Nsamp=list(100,50), years=list(seq(1994, 2012, by=2),
#'                                             2003:2012), lengthbin_vector = seq(9,30,by=2))
#'
#' plot(seq(8,30,by=2), as.numeric(ex3[1, -(1:6)]), type="b", col=2,
#'      xlab="Length Bin", ylab="Proportion of length",
#'      main="Comparison of different length bin structures via lengthbin_vector")
#' lines(seq(8, 30, by=.5), as.numeric(ex1[1, -(1:6)]), type="b", col=1)
#' legend("topright", legend=c("ex1", "ex3"), col=c(1,2), pch=1)
#'
#' unlink(x=c("test1.dat", "test2.dat", "test3.dat")) # clean up
#'
#' ## Plot distributions for a particular year to compare multinomial
#' ## vs. overdispersed Dirichlet
#' temp.list <- temp.list2 <- list()
#' for(i in 1:80){
#'     temp.list[[i]] <-
#'         change_lcomp(infile=infile, outfile="test1.dat", fleets=c(2), cpar=c(3),
#'                      Nsamp=list(100), years=list(1995),
#'                      write_file=FALSE)
#'     temp.list2[[i]] <-
#'         change_lcomp(infile=infile, outfile="test1.dat", fleets=c(2),
#'                      cpar=c(NA), Nsamp=list(100), years=list(1995),
#'                      write_file=FALSE)
#' }
#' ## Organize the data for plotting
#' x1 <- reshape2::melt(do.call(rbind, temp.list)[,-(1:6)[-3]], id.vars="FltSvy")
#' x2 <- reshape2::melt(do.call(rbind, temp.list2)[,-(1:6)[-3]], id.vars="FltSvy")
#' par(mfrow=c(2,1))
#' with(x1, boxplot(value~variable, las=2, ylim=c(0,.6), ylab="Proportion",
#'                  main="Overdispersed (cpar=3)",  xlab="length bin"))
#' temp <- as.numeric(subset(infile$lencomp, Yr==1995 & FltSvy == 2)[-(1:6)])
#' points(temp/sum(temp), pch="-", col="red")
#' with(x2, boxplot(value~variable, las=2, ylim=c(0,.6), ylab="Proportion",
#'                  main="Multinomial", xlab="length bin"))
#' temp <- as.numeric(subset(infile$lencomp, Yr==1995 & FltSvy == 2)[-(1:6)])
#' points(temp/sum(temp), pch="-", col="red")
#'
#' #'
#' @export
#' @seealso \code{\link{change_agecomp}}

change_lcomp <- function(infile, outfile, fleets = c(1,2), Nsamp,
                         years, cpar=1, lengthbin_vector=NULL, write_file=TRUE){
    ## The new lcomp is mostly based on the old one so start with
    ## that
    lcomp <- infile$lencomp
    newbins <- lengthbin_vector
    oldbins <- infile$lbin_vector
    if(!is.null(newbins)){
        if (min(newbins) > min(oldbins)) newbins <- c(min(oldbins), newbins)
        Nbins <- length(newbins)
    }
    ## Check inputs for errors
    if(substr_r(outfile,4) != ".dat" & write_file)
        stop(paste0("outfile ", outfile, " needs to end in .dat"))
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
                stop(paste0("Length of Nsamp does not match length of years for fleet ",fleets[i]))
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

    ## Recalculate length bins depending on user input
    if (!is.null(newbins) & Nfleets>0) {
        if (class(newbins) != "numeric") {
            stop("lengthbin_vector must have a numeric input")
        }
        if (min(newbins) < min(oldbins) |
            max(newbins) > max(oldbins)) {
            stop("lengthbin_vector must be within the [min;max] of population bin")
        }
        ## Adjust the "old" length bins to the "new" one
        if (length(oldbins) != Nbins) {
            newbin.index <- findInterval(oldbins, newbins)
            ## Calculate new bins by consolidating old bins, and then and sum
            ## across appropriate columns to create the new columns
            temp.index <- 1:length(unique(newbin.index))
            temp <- sapply(temp.index, function(x)
                           apply(as.matrix(lcomp[, 6+which(newbin.index==x)]),1, sum))
            new.lcomp <- cbind(lcomp[,1:6], temp)
            names(new.lcomp)[-(1:6)] <-
                c(paste0("l", newbins[-Nbins], "-",
                         newbins[-1]), paste0("l>",newbins[Nbins]))

            ## Change the corresponding locations in the .dat file
            infile$N_lbins <- Nbins
            infile$lbin_vector <- newbins
            lcomp <- new.lcomp
        }
    }

    ## Resample from the length data
    ## The general approach here is to loop through each row to keep
    ## (depends on years input) and resample depending on Nsamp and
    ## cvar. All these rows are then combined back together to form
    ## the final lcomp.
    newcomp.list <- list()                  # temp storlength for the new rows
    k <- 1
    ## Loop through each fleet
    if (Nfleets>0){
        for(i in 1:length(fleets)){
            fl <- fleets[[i]]
            if(!is.na(fl)){
                lcomp.fl <- lcomp[lcomp$FltSvy == fl & lcomp$Yr %in% years[[i]], ]
                if(length(years[[i]]) != nrow(lcomp.fl))
                    stop(paste("A year specified in years was not found in the input file for fleet", fl))
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
                            rmultinom(1, size=newcomp$Nsamp, prob=probs)/newcomp$Nsamp
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
    newfile <- infile
    newfile$lencomp <- newcomp.final
    if(Nfleets>0) newfile$N_lencomp <- nrow(newcomp.final)
    if(Nfleets==0) newfile$N_lencomp <- 0

    ## Write the modified file
    if(write_file)
        r4ss::SS_writedat(datlist = newfile, outfile = outfile, overwrite = T)
    return(invisible(newcomp.final))
}

