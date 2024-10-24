% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_DK.R
\name{calc_DK}
\alias{calc_DK}
\title{Calculate discard to kept ratio}
\usage{
calc_DK(comdiscData, areaDescription, fleetDescription)
}
\arguments{
\item{na.keep}{Boolean. Logical value to indicate whether original strata names
should be retained.}
}
\value{
Returns a \code{comdiscData} data.table with one additional column labeled
 with the value of \code{areaDescription}

\item{areaDescription}{The name of the region (found in \code{areaPolygon})
 that a record in \code{surveyData} is assigned to}
}
\description{
Use observer data to calculate the ratio of discards to kept by species.
}
\seealso{
Other comdisc: 
\code{\link{assign_area}()}
}
\concept{comdisc}
