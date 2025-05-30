% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-results.r
\name{get_compfit}
\alias{get_compfit}
\title{Get summaries of fits to composition data from report file list}
\usage{
get_compfit(report.file, name)
}
\arguments{
\item{report.file}{A list returned from \code{\link[r4ss:SS_output]{r4ss::SS_output()}} on operating
model or estimation model files.}

\item{name}{A character string that matches the element of
\code{report.file} that you wish to extract, e.g.,
\code{"Length_Comp_Fit_Summary"}.}
}
\description{
Extract the summary of fits to composition data, where the sections
are structured similarly for each type of data in the report file.
}
