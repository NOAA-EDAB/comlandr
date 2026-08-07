#' Extract VESSEL information
#'
#'Extract a list of vessel ID's, tonnage, crew size, home port, etc from the NEFSC_GARFO "PERMIT_VPS_VESSEL" supporting table
#'
#'
#' @inheritParams get_comland_data
#'
#' @return A list is returned:
#'
#'   \item{data}{containing the result of the executed \code{sqlStatement} }
#'
#'   \item{sql}{containing the \code{sqlStatement} itself}
#'
#'  \item{colNames}{a vector of the table's column names}
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
get_vessels <- function(channel) {
  #sqlStatement <- "select * from NEFSC_GARFO.PERMIT_VPS_VESSEL"
  message("Pulling the vessel data ...")
  # Select set of fields
  sqlStatement <- "select VES_NAME, HPORT, HPST, PPORT, PPST, ZIP1, LEN, CREW, GTONS, VHP, BLT, DATE_ISSUED, DATE_CANCELED from NEFSC_GARFO.PERMIT_VPS_VESSEL"

  query <- DBI::dbGetQuery(channel, sqlStatement)
  # get column names
  sqlcolName <- "select COLUMN_NAME from ALL_TAB_COLUMNS where TABLE_NAME = 'PERMIT_VPS_VESSEL' and owner='NEFSC_GARFO'"
  colNames <- t(DBI::dbGetQuery(channel, sqlcolName))

  return(list(
    data = dplyr::as_tibble(query),
    sql = sqlStatement,
    colNames = colNames
  ))
}
