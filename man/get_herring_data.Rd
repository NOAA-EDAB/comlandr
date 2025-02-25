% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_herring_data.R
\name{get_herring_data}
\alias{get_herring_data}
\title{Pull Herring data}
\usage{
get_herring_data(channel, comland, filterByYear, filterByArea, useForeign)
}
\arguments{
\item{channel}{an Object inherited from \code{ROracle::Oracle}. This object is used to connect
to communicate with the database engine. (see \code{dbutils::connect_to_database})}

\item{comland}{Data frame. master data frame containing species landings}

\item{filterByYear}{numeric vector. Years to be retrieved (Default = NA, pull all years)}

\item{filterByArea}{numeric vector. Statistical Areas to be retrieved (Default = NA, pull all areas)}

\item{useForeign}{boolean. Pull foreign data from NAFO. (Default = T)}
}
\value{
Processed Herring data added to comland
}
\description{
Herring Data comes from the state of Maine and replaces the herring data from StockEff (since
it is incomplete). Pulled from NEFSC_GARFO.maine_herring_catch
}
