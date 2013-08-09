\name{get_results_all}
\alias{get_results_all}
\title{Extract SS3 simulation output}
\usage{
  get_results_all(directory = getwd(),
    files.overwrite = FALSE, user.scenarios = NULL)
}
\arguments{
  \item{directory}{The directory which contains scenario
  folders with results.}

  \item{files.overwrite}{A switch to determine if existing
  files should be overwritten, useful for testing purposes
  or if new replicates are run.}

  \item{user.scenarios}{A character vector of scenarios
  that should be read in. Default is NULL, which indicates
  find all scenario folders in \code{directory}}
}
\description{
  This high level function extracts results from SS3 model
  runs. Give it a directory which contains directories for
  different "scenario" runs, within which are replicates
  and potentially bias adjustment runs. It writes two
  data.frames to file: one for single scalar values (e.g.
  MSY) and a second that contains output for each year of
  the same model (timeseries, e.g. biomass(year)). These
  can always be joined later.
}
\examples{
\dontrun{
## Put this R script in a folder which contains the Scenario folders, then run
## the code below.

## Exploring ss3 model results
library(ggplot2)
library(ss3sim)

## This function reads in results for all runs in a particular directory
get_results_all(files.overwrite=F)

## Rread in the final results produced by above function
scalars <- read.csv("final_results_scalar.csv")
ts <- read.csv("final_results_ts.csv")

## NOTE: For my case I had run different F cases (F0,F1, F2) for the base
## case. Thus below I've grouped by F. You might want to group by something
## else, such as M, D, E, etc. depending on what you've run.

## Check convergence
g <- ggplot(scalars)
round(with(scalars, tapply(max_grad, species, mean)),3)
round(with(scalars, tapply(max_grad, species, median)),3)
## Plot w/ free ylim to see differences
g+geom_boxplot(aes(F,max_grad))+facet_grid(species~., scales="free")

## Calculate and plot a metric, e.g. relative error of SSB_MSY
scalars <- transform(scalars,
                       SSB_MSY=(SSB_MSY_em-SSB_MSY_om)/SSB_MSY_om)
g <- ggplot(scalars)
g+geom_boxplot(aes(x=F,y=SSB_MSY))+facet_grid(species~.)

## steepness
scalars <- transform(scalars,
                       SR_BH_steep=(SR_BH_steep_om-SR_BH_steep_em)/SR_BH_steep_om)
g <- ggplot(scalars)
g+geom_boxplot(aes(x=F,y=SR_BH_steep))+facet_grid(species~.)

## SSB unfished
scalars <- transform(scalars,
                       SSB_Unfished=(SSB_Unfished_om-SSB_Unfished_em)/SSB_Unfished_om)
g <- ggplot(scalars)
g+geom_boxplot(aes(x=F,y=SSB_Unfished))+facet_grid(species~.)

##  log(R0)
scalars <- transform(scalars,
                       SR_LN_R0=(SR_LN_R0_om-SR_LN_R0_em)/SR_LN_R0_om)
g <- ggplot(scalars)
g+geom_boxplot(aes(F,SR_LN_R0))+facet_grid(species~.)

## Make some timeseries plots
##  Plot error in relative biomass by year
ts <- transform(ts, SpawnBio=(SpawnBio_em-SpawnBio_om)/SpawnBio_om)
g <- ggplot(ts, aes(x=year))+ ylab("Relative bias in biomass") + xlab("Year")
g+geom_jitter(aes(y=SpawnBio, group=replicate), size=.1, alpha=.3)+
    geom_smooth(method="loess",aes(y=SpawnBio), color="red") +
    facet_grid(species~., )+
    geom_hline(yintercept = 0, lty = 2)

## Look at recruitment
ts <- transform(ts, Recruit_0=(Recruit_0_em-Recruit_0_om)/Recruit_0_om)
g <- ggplot(ts, aes(x=year))+ ylab("Relative bias in recruitment") + xlab("Year")
g+geom_jitter(aes(y=Recruit_0), size=.1, alpha=.3)+
    geom_smooth(method="loess",aes(y=Recruit_0), color="red") +
    facet_grid(species~., )+
    geom_hline(yintercept = 0, lty = 2)
}
}
\author{
  Cole Monnahan
}
\seealso{
  Other get.results: \code{\link{get_results_scalar}},
  \code{\link{get_results_scenario}},
  \code{\link{get_results_timeseries}}
}

