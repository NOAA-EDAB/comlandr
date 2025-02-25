% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_species_itis.R
\name{get_species_itis}
\alias{get_species_itis}
\title{Extract SPECIES information from CFDBS (SPECIES_ITIS_NE, table)}
\usage{
get_species_itis(channel, species = "all", nameType = "common_name")
}
\arguments{
\item{channel}{an Object inherited from \code{ROracle::Oracle}. This object is used to connect
to communicate with the database engine. (see \code{dbutils::connect_to_database})}

\item{species}{A specific species code or set of codes. Either numeric or character vector. Defaults to "all" species.
Numeric codes (SPECIES_ITIS, NESPP4) are converted to VARCHAR2 (6 and 4 characters respectively) when creating the sql statement.}

\item{nameType}{Character string. Upper or lower case. Either "common_name" (default), "scientific_name" or "nespp4".
Determines which type of name to search under.}
}
\value{
A list is returned:

\item{data}{containing the result of the executed \code{$sql} statement}

\item{sql}{containing the sql call}

\item{colNames}{a vector of the table's column names}

The default sql statement "\code{select * from cfdbs.SPECIES_ITIS_NE}" is used
}
\description{
Extract a list of species names, code, market category, etc from the NEFSC_GARFO CFDBS_SPECIES_ITIS_NE table
}
\section{Reference}{

Use the data dictionary for field name explanations.
}

\examples{
\dontrun{
# extracts complete species table based on custom sql statement
channel <- connect_to_database(server="name_of_server",uid="individuals_username")
get_species_itis(channel)

# extracts info for cod (164712)
get_species_itis(channel,species=164712)

# extracts info for cod ("COD")
get_species_itis(channel,"cod") #o r
get_species_itis(channel,"co") # or (note also return cockles, calico scallop etc.)
get_species_itis(channel,"COD")

# extracts info for cod ("gadus")
get_species_itis(channel,"gadus",nameType="scientific_name") #o r
get_species_itis(channel,"morh",nameType="scientific_name") #o r
get_species_itis(channel,"GADUS",nameType="scientific_name") #o r

#' # extracts info for cod ("0814") market category 4
get_species_itis(channel,"0814",nameType="NESPP4") #o r
get_species_itis(channel,814,nameType="NESPP4")


# extracts info for cod (164712)  and bluefish (168559)
sqlStatement <- "select * from cfdbs.species_itis_ne"
get_species_itis(channel,species= c("164712","168559"))
}

}
\seealso{
\code{\link[dbutils]{connect_to_database}}

Other get functions: 
\code{\link{get_areas}()},
\code{\link{get_gears}()},
\code{\link{get_locations}()},
\code{\link{get_ports}()},
\code{\link{get_species}()},
\code{\link{get_vessels}()}
}
\concept{get functions}
