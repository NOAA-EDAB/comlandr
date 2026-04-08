#' Extract Dealers location details
#'
#'Extract a list of dealer names and locations from supporting tables
#'
#' @inheritParams get_comland_data
#' @param state Character vector. State abbreviations. Default = NA (All states)
#' @param year Numeric vector. Years in which to pull data. Defaul = NA (All years)
#'
#' @return A list is returned:
#'
#'   \item{data}{containing the result of the executed \code{sqlStatement}}
#'
#'    \item{sql}{containing the sql call}
#'
#'   \item{colNames}{a vector of the table's column names}
#'
#'If no \code{sqlStatement} is provided the default sql statement "\code{select * from NEFSC_GARFO.PERMIT_DEALER}" is used
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
#' # extracts dealer location table
#' channel <- connect_to_database(server="name_of_server",uid="individuals_username")
#' get_dealers(channel)
#'
#' # extracts dealer details for states, Massachusetts and Maine
#' get_dealers(channel, state = c("MA","ME"))
#'
#' # extracts dealer details for years 2010 to 2020
#' get_dealers(channel, year = 2010:2020)
#'
#' # extracts dealer details for years 2010 to 2020 for MA & ME
#' get_dealers(channel, state = c("MA","ME"), year = 2010:2020)
#'}
#'
#' @export
#'
#
get_dealers <- function(channel, state = NA, year = NA) {
  # creates the sql based on user input
  defaultSqlStatement = "select * from NEFSC_GARFO.PERMIT_DEALER"
  if (all(any(is.na(state)) & any(is.na(year)))) {
    # both are NA
    sqlStatement <- defaultSqlStatement
  } else if (any(is.na(state))) {
    # states are NA
    years <- paste0("'", year, "'", collapse = ",")
    sqlStatement <- paste(
      defaultSqlStatement,
      "where (YEAR in (",
      years,
      "))"
    )
  } else if (any(is.na(year))) {
    # years are NA
    states <- paste0("'", state, "'", collapse = ",")
    sqlStatement <- paste(
      defaultSqlStatement,
      "where (ST in (",
      states,
      "))"
    )
  } else {
    # neither are NA
    # format query
    states <- paste0("'", state, "'", collapse = ",")
    years <- paste0("'", year, "'", collapse = ",")

    sqlStatement <- paste(
      defaultSqlStatement,
      "where (ST in (",
      states,
      ")) and (YEAR in (",
      years,
      "))"
    )
  }

  query <- DBI::dbGetQuery(channel, sqlStatement)

  # get column names
  sqlcolName <- "select COLUMN_NAME from ALL_TAB_COLUMNS where TABLE_NAME = 'PERMIT_DEALER' and owner='NEFSC_GARFO'"
  colNames <- t(DBI::dbGetQuery(channel, sqlcolName))

  return(list(
    data = dplyr::as_tibble(query),
    sql = sqlStatement,
    colNames = colNames
  ))
}
