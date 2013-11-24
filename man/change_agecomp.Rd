\name{change_agecomp}
\alias{change_agecomp}
\title{Sample age compositions from expected values}
\usage{
  change_agecomp(infile, outfile, fleets = c(1, 2), Nsamp,
    years, cpar = 1, agebin_vector = NULL,
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

  \item{agebin_vector}{A numeric vector giving the new age
  bins to use. \code{agebin_vector} must be within the
  [min;max] of population bin. This feature alows dynamic
  binning by the user, but is not fully tested. Users
  should consult the vignette and carefully check the
  function bins the data as desired before proceeding with
  simulations.}
}
\value{
  A modified \code{.dat} file if \code{write_file=TRUE}. A
  list object containing the modified \code{.dat} file is
  returned invisibly.
}
\description{
  Take a \code{data.SS_new} file containing expected values
  and sample to create observed age compositions which are
  then written to file for use by the EM.
}
\examples{
d <- system.file("extdata", package = "ss3sim")
f_in <- paste0(d, "/example-om/data.ss_new")
infile <- r4ss::SS_readdat(f_in, section = 2, verbose = FALSE)
## Turn off age comps by specifying fleets=NULL
change_agecomp(infile=infile, outfile="test1.dat",
               fleets=NULL, cpar=c(5,NA), Nsamp=list(100,100),
               years=list(1995, 1995), write_file=FALSE)
## Generate with a smaller number of fleet taking samples
ex1 <- change_agecomp(infile=infile, outfile="test1.dat", fleets=c(2),
                      Nsamp=list(c(10,50)), years=list(c(1999,2000)),
                      write_file=FALSE)
## Generate with varying Nsamp by year for first fleet
ex2 <- change_agecomp(infile=infile, outfile="test2.dat", fleets=c(1,2),
                      Nsamp=list(c(rep(50, 5), rep(100, 5)), 50),
                      years=list(seq(1994, 2012, by=2),
                          2003:2012), write_file=FALSE)
## Generate with varying Nsamp by year for first fleet AND with different age bins
ex3 <- change_agecomp(infile=infile, outfile="test3.dat", fleets=c(1,2),
                      Nsamp=list(c(rep(50, 5), rep(100, 5)), 50),
                      years=list(seq(1994, 2012, by=2),
                          2003:2012), agebin_vector = seq(1,15,by=3),
                          write_file=FALSE)
plot(seq(0,15, by=3), as.numeric(ex3[1, -(1:9)]), type="b", col=2,
     xlab="Age Bin", ylab="Proportion of Age",
     main="Comparison of different age bin structures via agebin_vector")
lines(0:15, as.numeric(ex2[1, -(1:9)]), type="b", col=1)
legend("topright", legend=c("ex2", "ex3"), col=c(1,2), pch=1)

## Run three  cases showing Multinomial, Dirichlet(1) and over-dispersed
## Dirichlet for different levels of sample sizes
par(mfrow = c(1,3))
for(samplesize in c(30, 100, 1000)){
    ex4 <- change_agecomp(infile=infile, outfile="test4.dat", fleets=c(1,2),
                          Nsamp=list(samplesize, samplesize),
                          write_file = FALSE,
                          years=list(2000,2000), cpar=c(NA, 1))
    ex5 <- change_agecomp(infile=infile, outfile="test5.dat", fleets=c(1,2),
                          Nsamp=list(samplesize, samplesize),
                          write_file = FALSE,
                          years=list(2000,2000), cpar=c(1, 1))
    ex6 <- change_agecomp(infile=infile, outfile="test6.dat", fleets=c(1,2),
                          Nsamp=list(samplesize, samplesize),
                          write_file = FALSE,
                          years=list(2000,2000), cpar=c(5, 1))
    true <- subset(infile$agecomp, FltSvy==1 & Yr == 2000)[-(1:9)]
    true <- true/sum(true)
    plot(0:15, subset(ex4, FltSvy==1)[1,-(1:9)], type="b", ylim=c(0,1),
         col=1, xlab="Age", ylab="Proportion", main=paste("Sample size=",samplesize))
    legend("topright", legend=c("Multinomial", "Dirichlet(1)", "Dirichlet(5)", "Truth"),
           lty=1, col=1:4)
    lines((0:15), subset(ex5, FltSvy==1)[1,-(1:9)], type="b", col=2)
    lines((0:15), subset(ex6, FltSvy==1)[1,-(1:9)], type="b", col=3)
    lines((0:15), true, col=4, lwd=2)
}
}
\author{
  Cole Monnahan and Kotaro Ono; modified from a version by
  Roberto Licandeo and Felipe Hurtado-Ferro
}
\seealso{
  \code{\link{change_lcomp}}
}

