\name{change_agecomp}
\alias{change_agecomp}
\title{Sample age compositions from expected values}
\usage{
  change_agecomp(infile, outfile, fleets = c(1, 2), Nsamp,
    years, cpar = 1, agebin_vector = NULL,
    write_file = TRUE)
}
\arguments{
  \item{infile}{SS data object as read in from
  \code{SS_readdat} in the r4ss package. Make sure you
  select option \code{section=2}.}

  \item{outfile}{Character string of the name for the new
  file to be created. Must end in \code{.dat}.}

  \item{fleets}{Numeric vector giving the fleets to be
  used. This order also pertains to other arguments. A
  missing value excludes that fleet from outfile (i.e. turn
  it off so no samples are written). If none of the fleet
  collected samples, keep the value to \code{fleets=NULL}.}

  \item{Nsamp}{A numeric list of the same length as
  \code{fleets}. Either single values or vectors of the
  same length as the number of years can be passed through.
  Single values are repeated for all years. If no fleet
  collected samples, keep the value to \code{Nsamp=NULL}.}

  \item{years}{A numeric list of the same length as
  \code{fleets}. Each element specifies the years to sample
  from each fleet. Years left out are excluded in
  \code{outfile}, allowing the user to reduce (but not
  increase) the sample scheme as given in \code{infile}. If
  no fleet collected samples, keep the value to
  \code{years=NULL}.}

  \item{agebin_vector}{A numeric vector giving the new age
  bins to use. \code{agebin_vector} must be within the
  [min;max] of population bin. This feature alows dynamic
  binning by the user, but is not fully tested. Users
  should consult the vignette and carefully check the
  function bins the data as desired before proceeding with
  simulations.}

  \item{cpar}{A numeric value or vector the same length as
  \code{fleets} controlling the variance of the Dirichlet
  distribution used for sampling. A value of \code{1}
  indicates the same standard deviation as a multinomial of
  the given \code{Nsamp}, \code{2} indicates twice, etc.
  Values greater than one indicate overdispersion, and less
  underdispersion.}

  \item{write_file}{A switch for whether to write
  \code{outfile} to disk. Can be turned off to speed up
  testing or exploration of the function (the new agecomp
  is returned invisibly, as in the examples below)}
}
\description{
  Take a \code{data.SS_new} file containing expected values
  and sample to create observed age compositions which are
  then written to file for use by the EM.
}
\examples{
\dontrun{
d <- system.file("extdata", package = "ss3sim")
f_in <- paste0(d, "/example-om/data.ss_new")
infile <- r4ss::SS_readdat(f_in, section = 2, verbose = FALSE)

## Generate with a smaller number of fleet taking samples
ex1 <- change_agecomp(infile=infile, outfile="test1.dat", fleets=c(2),
               Nsamp=list(c(10,50)), years=list(c(1999,2000)))

## Generate with varying Nsamp by year for first fleet
ex2 <- change_agecomp(infile=infile, outfile="test2.dat", fleets=c(1,2),
               Nsamp=list(c(rep(50, 5), rep(100, 5)), 50),
               years=list(seq(1994, 2012, by=2),
               2003:2012))

## Generate with varying Nsamp by year for first fleet AND with different age bins
ex3 <- change_agecomp(infile=infile, outfile="codEM.dat", fleets=c(1,2),
               Nsamp=list(c(rep(50, 5), rep(100, 5)), 50),
               years=list(seq(1994, 2012, by=2),
               2003:2012),	agebin_vector = seq(1,15,by=3))

plot(seq(0,15, by=3), as.numeric(ex3[1, -(1:9)]), type="b", col=2,
     xlab="Age Bin", ylab="Proportion of Age",
     main="Comparison of different age bin structures via agebin_vector")
lines(0:15, as.numeric(ex1[1, -(1:9)]), type="b", col=1)
legend("topright", legend=c("ex1", "ex3"), col=c(1,2), pch=1)

unlink(x=c("test1.dat", "test2.dat", "test3.dat")) # clean up

## Plot distributions for a particular year for a cpar of 5 and 1 to
## demonstrate the impact of cpar
temp.list <- list()
for(i in 1:500){
    temp.list[[i]] <-
        change_agecomp(infile=infile, outfile="test1.dat",
                       fleets=c(1,2), cpar=c(5,1), Nsamp=list(100,100),
                       years=list(1995, 1995), write_file=F)
}
xx <- do.call(rbind, temp.list)[,-(1:9)[-3]]
xx <- reshape2::melt(xx, id.vars="FltSvy")
par(mfrow=c(2,1))
with(subset(xx, FltSvy==1), boxplot(value~variable, las=2,
                main="Overdispersed",  xlab="Age bin"))
temp <- as.numeric(subset(infile$agecomp, Yr==1995 & FltSvy == 1)[-(1:9)])
points(temp/sum(temp), pch="-", col="red")
with(subset(xx, FltSvy==2), boxplot(value~variable, las=2,
                main="Multinomial", xlab="Age bin"))
temp <- as.numeric(subset(infile$agecomp, Yr==1995 & FltSvy == 2)[-(1:9)])
points(temp/sum(temp), pch="-", col="red")
#' }
}
\author{
  Cole Monnahan and Kotaro Ono; modified from a version by
  Roberto Licandeo and Felipe Hurtado-Ferro
}

