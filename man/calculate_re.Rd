% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_re.R
\name{calculate_re}
\alias{calculate_re}
\title{Calculate relative error}
\usage{
calculate_re(dat, add = TRUE, EM = "em")
}
\arguments{
\item{dat}{An input data frame. Should be either a scalar or time series
data frame as returned from \code{\link[=get_results_all]{get_results_all()}} or a related
\verb{get_results_*()}. Specifically, the data frame needs to have columns
with \verb{_em} and \verb{_om} as names. If the data is provided in long
rather than wide format, then \code{\link[=convert_to_wide]{convert_to_wide()}} will be used
internally before calculating RE and a wide data frame will be returned.}

\item{add}{Logical: should the relative error columns be added to \code{dat}
or should the original EM and OM columns be dropped? If \code{FALSE}, then
the returned data frame will have only the identifying columns and the new
relative error columns. You could then merge selected columns back into
\code{dat} if you wished. The default is to return all columns.}

\item{EM}{A character value specifying the name of the EM to calculate the
RE of when the results are provided in long format and there is the potential
for multiple EMs. See the column \code{model_run} for options.}
}
\value{
The default is to return a data frame structured the same as the
input data frame, i.e., \code{dat}, but with additional columns, where
\code{'_re'} is appended to the base string of the column name.
All \code{NAN} and \code{Inf} values are returned as \code{NA} values,
typically because you cannot divide by zero. Irrelevant columns, i.e.,
columns of entirely zero of \code{NA} are removed prior to returning the
data frame.
}
\description{
Calculate the relative error (RE; \eqn{[EM - OM]/OM}) of
parameters and derived quantities stored in a scalar or time series
data frame generated by \code{\link[=get_results_all]{get_results_all()}}.
}
\examples{
# Example with built in package data:
data("ts_dat", package = "ss3sim")
data("scalar_dat", package = "ss3sim")
head(calculate_re(ts_dat))
head(calculate_re(ts_dat, add = FALSE))
head(calculate_re(scalar_dat, add = FALSE))
rm("ts_dat", "scalar_dat")
}
\seealso{
\code{\link[=get_results_all]{get_results_all()}}, \code{\link[=get_results_scenario]{get_results_scenario()}}
}
\author{
Sean Anderson and Cole Monnahan
}
