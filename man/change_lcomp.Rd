\name{change_lcomp}
\alias{change_lcomp}
\title{Sample length compositions from expected values}
\usage{
  change_lcomp(infile, outfile, fleets = c(1, 2), Nsamp,
    years, cpar = 1, lengthbin_vector = NULL,
    write_file = TRUE)
}
\arguments{
  \item{infile}{An SS data object as read in from
  \code{\link[r4ss]{SS_readdat}} in the \pkg{r4ss} package.
  Make sure you select option \code{section=2}.}

  \item{outfile}{A character string of the new \code{.dat}
  file name to be created. Must end in \code{.dat}.}

  \item{fleets}{A numeric vector giving the fleets to be
  used. This order also pertains to other arguments. A
  missing value excludes that fleet from \code{outfile}
  (i.e. it turns it off so no samples are written). If none
  of the fleet collected samples, keep the value to
  \code{fleets=NULL}.}

  \item{years}{A list the same length as \code{fleets}
  giving the years as numeric vectors. If no fleet
  collected samples, keep the value to \code{years=NULL}.}

  \item{write_file}{A switch for whether to write
  \code{outfile} to disk. Can be turned off to speed up
  testing or exploration of the function. The new data are
  returned invisibly, as in the examples below.}

  \item{Nsamp}{A numeric list of the same length as
  \code{fleets}. Either single values or vectors of the
  same length as the number of years can be passed through.
  Single values are repeated for all years. If no fleet
  collected samples, keep the value to \code{Nsamp=NULL}.}

  \item{cpar}{A numeric value or vector the same length as
  \code{fleets} controlling the variance of the Dirichlet
  distribution used for sampling. A value of \code{1}
  indicates the same standard deviation as a multinomial of
  the given \code{Nsamp}, \code{2} indicates twice, etc.
  Values greater than one indicate overdispersion, and less
  underdispersion.}

  \item{lengthbin_vector}{A numeric vector giving the new
  length bins to use. \code{lengthbin_vector} must be
  within the [min;max] of population bin. This feature
  allows dynamic binning by the user, but is not fully
  tested. Users should consult the vignette and carefully
  check the function bins the data as desired before
  proceeding with simulations.}
}
\value{
  A modified \code{.dat} file if \code{write_file=TRUE}. A
  list object containing the modified \code{.dat} file is
  returned invisibly.
}
\description{
  Take a \code{data.SS_new} file containing expected values
  and sample to create observed length compositions which
  are then written to file for use by the EM.
}
\examples{
d <- system.file("extdata", package = "ss3sim")
f_in <- paste0(d, "/example-om/data.ss_new")
infile <- r4ss::SS_readdat(f_in, section = 2, verbose = FALSE)

## Generate with constant sample size across years
ex1 <- change_lcomp(infile=infile, outfile="test1.dat", fleets=c(1,2),
                    Nsamp=list(100,50), years=list(seq(1994, 2012, by=2),
                                            2003:2012))

## Generate with varying Nsamp by year for first fleet
ex2 <- change_lcomp(infile=infile, outfile="test2.dat", fleets=c(1,2),
                    Nsamp=list(c(rep(50, 5), rep(100, 5)), 50),
                    years=list(seq(1994, 2012, by=2),
                        2003:2012))

## Generate with constant sample size across years AND with different length
## bins (same as ex1 except bins)
ex3 <- change_lcomp(infile=infile, outfile="test3.dat", fleets=c(1,2),
                    Nsamp=list(100,50), years=list(seq(1994, 2012, by=2),
                                            2003:2012), lengthbin_vector = seq(9,30,by=2))

plot(seq(8,30,by=2), as.numeric(ex3[1, -(1:6)]), type="b", col=2,
     xlab="Length Bin", ylab="Proportion of length",
     main="Comparison of different length bin structures via lengthbin_vector")
lines(seq(8, 30, by=.5), as.numeric(ex1[1, -(1:6)]), type="b", col=1)
legend("topright", legend=c("ex1", "ex3"), col=c(1,2), pch=1)

unlink(x=c("test1.dat", "test2.dat", "test3.dat")) # clean up

## Plot distributions for a particular year to compare multinomial
## vs. overdispersed Dirichlet
temp.list <- temp.list2 <- list()
for(i in 1:100){
    temp.list[[i]] <-
        change_lcomp(infile=infile, outfile="test1.dat", fleets=c(2), cpar=c(3),
                     Nsamp=list(100), years=list(1995),
                     write_file=FALSE)
    temp.list2[[i]] <-
        change_lcomp(infile=infile, outfile="test1.dat", fleets=c(2),
                     cpar=c(NA), Nsamp=list(100), years=list(1995),
                     write_file=FALSE)
}
## Organize the data for plotting
x1 <- reshape2::melt(do.call(rbind, temp.list)[,-(1:6)[-3]], id.vars="FltSvy")
x2 <- reshape2::melt(do.call(rbind, temp.list2)[,-(1:6)[-3]], id.vars="FltSvy")
par(mfrow=c(2,1))
with(x1, boxplot(value~variable, las=2, ylim=c(0,.6), ylab="Proportion",
                 main="Overdispersed (cpar=3)",  xlab="length bin"))
temp <- as.numeric(subset(infile$lencomp, Yr==1995 & FltSvy == 2)[-(1:6)])
points(temp/sum(temp), pch="-", col="red")
with(x2, boxplot(value~variable, las=2, ylim=c(0,.6), ylab="Proportion",
                 main="Multinomial", xlab="length bin"))
temp <- as.numeric(subset(infile$lencomp, Yr==1995 & FltSvy == 2)[-(1:6)])
points(temp/sum(temp), pch="-", col="red")

#'
}
\author{
  Cole Monnahan and Kotaro Ono; modified from a version by
  Roberto Licandeo and Felipe Hurtado-Ferro
}
\seealso{
  \code{\link{change_agecomp}}
}

