#' Extract VESSEL information from CFDBS
#'
#'Extract a list of vessell ID's, tonnage, crew size, home port, etc from the NEFSC "Mstrvess" supporting table
#'
#'
#' @inheritParams get_comland_data
#' @param sqlStatement an sql statement (optional)
#' @param where text string appending where clause to sql
#'
#' @return A list is returned:
#'
#'   \item{data}{containing the result of the executed \code{sqlStatement} }
#'
#'   \item{sql}{containing the \code{sqlStatement} itself}
#'
#'  \item{colNames}{a vector of the table's column names}
#'
#'If no \code{sqlStatement} is provided the default sql statement "\code{select * from NEFSC_GARFO.cfdbs_mstrvess}" is used
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
#' # extracts complete vessel table based on custom sql statement
#' channel <- connect_to_database(server="name_of_server",uid="individuals_username")
#' get_vessels(channel)
#'
#'}
#'
#' @export
#'
#
get_vessels <- function(channel){

  sqlStatement <- "select * from NEFSC_GARFO.cfdbs_mstrvess"

  query <- DBI::dbGetQuery(channel,sqlStatement)

  # get column names
  sqlcolName <- "select COLUMN_NAME from ALL_TAB_COLUMNS where TABLE_NAME = 'CFDBS_MSTRVESS' and owner='NEFSC_GARFO'"
  colNames <- DBI::dbGetQuery(channel,sqlcolName)

  return (list(data=dplyr::as_tibble(query),sql=sqlStatement, colNames=colNames))

}



