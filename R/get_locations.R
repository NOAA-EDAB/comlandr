#' Extract LOCATION information from CFDBS
#'
#'Extract a list of lat, long, ten minute square, etc from the NEFSC "loc" supporting table
#'
#'
#' @inheritParams get_comland_data
#'
#' @return A list is returned:
#'
#'    \item{data}{containing the result of the executed \code{sqlStatement} }
#'
#'    \item{sql}{containing the \code{sqlStatement} itself}
#'
#'    \item{colNames}{a vector of the table's column names}
#'
#'
#'@section Reference:
#'Use the data dictionary for field name explanations
#'
#'@family get functions
#'
#' @seealso \code{\link[dbutils]{connect_to_database}}
#'
#' @examples
#' \dontrun{
#' # extracts complete locations table based on default sql statement
#' channel <- connect_to_database(server="name_of_server",uid="individuals_username")
#' get_locations(channel)
#'}
#'
#' @export
#'
#
get_locations <- function(channel){

  sqlStatement <- "select * from NEFSC_GARFO.cfdbs_loc"

  query <- DBI::dbGetQuery(channel,sqlStatement)

  # get column names
  sqlcolName <- "select COLUMN_NAME from ALL_TAB_COLUMNS where TABLE_NAME = 'LOC' and owner='CFDBS'"
  colNames <- DBI::dbGetQuery(channel,sqlcolName)

  return (list(data=dplyr::as_tibble(query),sql=sqlStatement, colNames=colNames))

}



