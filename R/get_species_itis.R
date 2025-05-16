#' Extract SPECIES information from CFDBS (SPECIES_ITIS_NE, table)
#'
#'Extract a list of species names, code, market category, etc from the NEFSC_GARFO CFDBS_SPECIES_ITIS_NE table
#'
#'
#' @inheritParams get_comland_data
#' @param species A specific species code or set of codes. Either numeric or character vector. Defaults to "all" species.
#' Numeric codes (SPECIES_ITIS, NESPP4) are converted to VARCHAR2 (6 and 4 characters respectively) when creating the sql statement.
#' @param nameType Character string. Upper or lower case. Either "common_name" (default), "scientific_name" or "nespp4".
#'  Determines which type of name to search under.
#'
#' @return A list is returned:
#'
#'   \item{data}{containing the result of the executed \code{$sql} statement}
#'
#'   \item{sql}{containing the sql call}
#'
#'   \item{colNames}{a vector of the table's column names}
#'
#'The default sql statement "\code{select * from NEFSC_GARFO.cfdbs_SPECIES_ITIS_NE}" is used
#'
#'@section Reference:
#'Use the data dictionary for field name explanations.
#'
#'@family get functions
#'
#' @seealso \code{\link[dbutils]{connect_to_database}}
#'
#' @examples
#' \dontrun{
#' # extracts complete species table based on custom sql statement
#' channel <- connect_to_database(server="name_of_server",uid="individuals_username")
#' get_species_itis(channel)
#'
#' # extracts info for cod (164712)
#' get_species_itis(channel,species=164712)
#'
#' # extracts info for cod ("COD")
#' get_species_itis(channel,"cod") #o r
#' get_species_itis(channel,"co") # or (note also return cockles, calico scallop etc.)
#' get_species_itis(channel,"COD")
#'
#' # extracts info for cod ("gadus")
#' get_species_itis(channel,"gadus",nameType="scientific_name") #o r
#' get_species_itis(channel,"morh",nameType="scientific_name") #o r
#' get_species_itis(channel,"GADUS",nameType="scientific_name") #o r
#'
#' #' # extracts info for cod ("0814") market category 4
#' get_species_itis(channel,"0814",nameType="NESPP4") #o r
#' get_species_itis(channel,814,nameType="NESPP4")
#'
#'
#' # extracts info for cod (164712)  and bluefish (168559)
#' sqlStatement <- "select * from cfdbs.species_itis_ne"
#' get_species_itis(channel,species= c("164712","168559"))
#'}
#'
#' @export
#'
#

get_species_itis <- function(channel,species="all",nameType="common_name"){

  # nameType = common_name or scientific_name, NESPP4

  # creates the sql based on user input
  if (toupper(nameType) == "NESPP4"){
    sqlStatement <- dbutils::create_sql(species,fieldName="NESPP4",fieldName2=nameType,dataType="%04d",defaultSqlStatement="select * from NEFSC_GARFO.cfdbs_species_itis_ne")
  } else {
    sqlStatement <- dbutils::create_sql(species,fieldName="species_itis",fieldName2=nameType,dataType="%06d",defaultSqlStatement="select * from NEFSC_GARFO.cfdbs_species_itis_ne")
  }

  # strip ; and add additional content
  sqlStatement <- sub(";","",sqlStatement)
  if (length(species) == 1) {
    if (tolower(species) == "all") {
      sqlStatement <- paste0(sqlStatement," where NESPP4_FLAG = 1")
    } else {
      sqlStatement <- paste0(sqlStatement," and NESPP4_FLAG = 1")
    }
  } else {
    sqlStatement <- paste0(sqlStatement," and NESPP4_FLAG = 1")
  }

  query <- DBI::dbGetQuery(channel,sqlStatement)

  # get column names
  sqlcolName <- "select COLUMN_NAME from ALL_TAB_COLUMNS where TABLE_NAME = 'CFDBS_SPECIES_ITIS_NE' and owner='NEFSC_GARFO'"
  colNames <- t(DBI::dbGetQuery(channel,sqlcolName))

  return (list(data=dplyr::as_tibble(query),sql=sqlStatement, colNames=colNames))

}



