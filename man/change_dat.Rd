\name{change_dat}
\alias{change_dat}
\title{Take as SS data object, manipulate the values, and return a
modified version}
\usage{
  change_dat(SS_dat, Yr_start = 1913, Yr_end = 2012,
    Nfleet = 1, Nsurveys = 1, N_areas = 1,
    fleetnames = "Fishery\%Survey",
    surveytiming = c(-1, 0.5), Nareas = 1, Ncatch = 100,
    Ncpue = 200, CPUE_index = rep(1, 100),
    survey_index = rep(1, 100),
    se_log_CPUE = rep(0.1, 100),
    se_log_survey = rep(0.1, 100), N_lencomp = 1,
    lengthcompdata = 1, N_agecomp = 1, agecompdata = 1)
}
\arguments{
  \item{Yr_start}{Starting year}

  \item{Yr_end}{Ending year}

  \item{Nfleet}{Number of fleets}

  \item{Nsurveys}{Number of surveys}

  \item{N_areas}{Number of areas}

  \item{fleetnames}{Fleet names}

  \item{surveytiming}{Sample timing}

  \item{Nareas}{Number of areas}

  \item{Ncatch}{Number of catch records}

  \item{Ncpue}{Number of CPUE records}

  \item{CPUE_index}{CPUE index values}

  \item{survey_index}{Survey index values}

  \item{se_log_CPUE}{Standard error of log(CPUE)}

  \item{se_log_survey}{Standard error of log(survey index)}

  \item{N_lencomp}{Number of length-composition
  observations}

  \item{lengthcompdata}{Length-composition data}

  \item{N_agecomp}{Number of age-composition observations}

  \item{agecompdata}{Age composition data}
}
\description{
  Take as SS data object, manipulate the values, and return
  a modified version
}

