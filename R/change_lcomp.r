#' Sample length compositions from expected values
#'
#' Take a \code{data.SS_new} file containing expected values and sample to
#' create observed length compositions which are then written to file for use by
#' the EM.
#'
#' @author Cole Monnahan and Kotaro Ono; modified from a version by Roberto
#' Licandeo and Felipe Hurtado-Ferro
#' @param infile SS data object as read in from \code{SS_readdat} in the r4ss
#' packlength. Make sure you select option \code{section=2}.
#' @param outfile Character string of the name for the new file to be
#' created. Must end in \code{.dat}.
#' @param fleets Numeric vector giving the fleets to be used. This order also
#' pertains to other arguments. A value of \code{NA} can be passed to exclude
#' that fleet from outfile (i.e. turn it off).
#' @param Nsamp A numeric list of the same length as \code{fleets}. Either
#' single values or vectors the same length as the number of years can be passed
#' through. Single values are repeated for all years.
#' @param years A numeric list of the same length as fleets. Each element
#' specifies the years to sample from each fleet. Years left out are excluded in
#' \code{outfile}, allowing the user to reduce (but not increase) the sample
#' scheme as given in \code{infile}
#' @param lengthbin_vector A numeric vector giving the new length bins to use.
#' \code{agebin_vector} must be within the [min;max] of population bin. This
#' feature allows dynamic binning by the user, but is not fully tested. Users
#' should consult the vignette and carefully check the function bins the data as
#' desired before proceeding with simulations.
#' @param cpar A numeric value or vector the same length as \code{fleets}
#' controlling the variance of the Dirichlet distribution used for sampling. A
#' value of \code{1} indicates the same standard deviation as a multinomial of
#' the given \code{Nsamp}, \code{2} indicates twice, etc. Values greater than
#' one indicate overdispersion, and less underdispersion.
#' @param write_file A switch for whether to write \code{outfile} to disk. Can
#' be turned off to speed up testing or exploration of the function (the new
#' lcomp is returned invisibly)

#' @examples
#' \dontrun{
# setwd("C:\\Users\\kotaro\\Desktop\\essai\\ss3\\example-om")
# d <- getwd()
# # f_in <- paste0(d, "/data.ss_new")
# f_in <- paste0(getwd(), "/codOM.dat")
# infile <- r4ss::SS_readdat(f_in, section = 2, verbose = FALSE)

## Generate with constant sample size across years
# ex1 <- change_lcomp(infile=infile, outfile="test1.dat", fleets=c(1,2),
               # Nsamp=list(100,50), years=list(seq(1994, 2012, by=2),
              # 2003:2012))

# ## Generate with varying Nsamp by year for first fleet
# ex2 <- change_lcomp(infile=infile, outfile="test2.dat", fleets=c(1,2),
               # Nsamp=list(c(rep(50, 5), rep(100, 5)), 50),
               # years=list(seq(1994, 2012, by=2),
               # 2003:2012))

# ## Generate with varying Nsamp by year for first fleet AND with different length bins
# ex3 <- change_lcomp(infile=infile, outfile="test3.dat", fleets=c(1,2),
               # Nsamp=list(c(rep(50, 5), rep(100, 5)), 50),
               # years=list(seq(1994, 2012, by=2), 2003:2012),
			   # lengthbin_vector = seq(9,30,by=2))

# plot(seq(20,150, by=5), as.numeric(ex3[1, -(1:6)]), type="b", col=2,
     # xlab="length Bin", ylab="Proportion of length",
     # main="Comparison of different length bin structures via lengthbin_vector")
# lines(0:15, as.numeric(ex1[1, -(1:9)]), type="b", col=1)
# legend("topright", legend=c("ex1", "ex3"), col=c(1,2), pch=1)

# unlink(x=c("test1.dat", "test2.dat", "test3.dat")) # clean up

# ## Plot distributions for a particular year for a cpar of 5 and 1 to
# ## demonstrate the impact of cpar
# temp.list <- list()
# for(i in 1:500){
    # temp.list[[i]] <-
        # sample_lcomp(infile=infile, outfile="test1.dat",
                       # fleets=c(1,2), cpar=c(5,1), Nsamp=list(100,100),
                       # years=list(1995, 1995), write_file=F)
# }
# xx <- do.call(rbind, temp.list)[,-(1:9)[-3]]
# xx <- reshape2::melt(xx, id.vars="FltSvy")
# par(mfrow=c(2,1))
# with(subset(xx, FltSvy==1), boxplot(value~variable, las=2,
                # main="Overdispersed",  xlab="length bin"))
# temp <- as.numeric(subset(infile$lcomp, Yr==1995 & FltSvy == 1)[-(1:9)])
# points(temp/sum(temp), pch="-", col="red")
# with(subset(xx, FltSvy==2), boxplot(value~variable, las=2,
                # main="Multinomial", xlab="length bin"))
# temp <- as.numeric(subset(infile$lcomp, Yr==1995 & FltSvy == 2)[-(1:9)])
# points(temp/sum(temp), pch="-", col="red")

#' }
#' @export

change_lcomp <- function(infile, outfile, fleets = c(1,2), Nsamp,
                         years, cpar=1, lengthbin_vector=NULL, write_file=TRUE){
    ## The new lcomp is mostly based on the old one so start with
    ## that
    lcomp <- infile$lencomp
    newbins <- lengthbin_vector
    oldbins <- infile$lbin_vector
    if (min(newbins) > min(oldbins)) newbins <- c(min(oldbins), newbins)
    Nbins <- length(newbins)

    ## Check inputs for errors
    if(substr_r(outfile,4) != ".dat" & write_file)
        stop(paste0("outfile ", outfile, " needs to end in .dat"))
    Nfleets <- length(fleets)
    if(length(unique(lcomp$FltSvy)) != Nfleets)
        stop(paste0("Number of fleets specified (",Nfleets,
                    ") does not match input file (",
                    length(unique(lcomp$FltSvy)), ")"))
    if(FALSE %in% (fleets %in% unique(lcomp$FltSvy)))
        stop(paste0("The specified fleet number does not match input file"))
    if(class(Nsamp) != "list" | length(Nsamp) != Nfleets)
        stop("Nsamp needs to be a list of same length as fleets")
    if(class(years) != "list" | length(years) != Nfleets)
        stop("years needs to be a list of same length as fleets")
    for(i in fleets){
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
    ## End input checks

    ## Recalculate length bins depending on user input
    if (!is.null(newbins)) {
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
    for(i in 1:length(fleets)){
        if(!is.na(fl)){
            lcomp.fl <- subset(lcomp, FltSvy==fl & Yr %in% years[[i]])
            if(length(years[[i]]) != nrow(lcomp.fl))
                stop(paste("A year specified in years was not found in the input file for fleet", fl))
            lcomp.fl$Nsamp <- Nsamp[[i]]
            ## Now loop through each year and resample that row
            for(yr in years[[i]]) {
                newcomp <- subset(lcomp.fl, Yr==yr)
                ## First 1-9 cols aren't length bins so skip them
                probs <- as.numeric(newcomp[-(1:6)]/sum(newcomp[-(1:6)]))
                lambda <- newcomp$Nsamp/cpar[i]^2 - 1
                ## Replace expected values with sampled values
                newcomp[-(1:6)] <-
                    gtools::rdirichlet(1, as.numeric(probs) * lambda)
                newcomp.list[[k]] <- newcomp
                k <- k+1
            }
        }
    }
    ## Combine new rows together into one data.frame
    newcomp.final <- do.call(rbind, newcomp.list)

    ## Build the new dat file
    newfile <- infile
    newfile$lencomp <- newcomp.final
    newfile$N_lencomp <- nrow(newcomp.final)

    ## Write the modified file
    if(write_file)
        r4ss::SS_writedat(datlist = newfile, outfile = outfile, overwrite = T)
    return(invisible(newcomp.final))
}
