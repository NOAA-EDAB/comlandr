#' Pull Herring data
#'
#' Herring Data comes from the state of Maine. The underlying oracle table is
#' updated periodically, often when an assessment is required. The herring data
#' from CAMS/STOCKEFF is incomplete. This is used in its place
#' Pulled from NEFSC_GARFO.maine_herring_catch
#'
#' @inheritParams get_comland_data
#'
#'@return Herring data as a data frame
#'
#'@importFrom data.table ":=" "key"
#'
#' @export

get_herring_data <- function(
  channel,
  filterByYear = NA,
  filterByArea = NA
) {
  #Pulling data
  message("Pulling Atlantic herring data from maine_herring_catch ...")

  # Determine the years and the spatial extent of the pull
  if (any(is.na(filterByYear))) {
    years <- ">= 1963"
  } else {
    years <- paste0("in (", survdat:::sqltext(filterByYear), ")")
  }

  herr_qry <- paste0(
    "select year, month, category, stock_area, negear, gearname,
                     keptmt, discmt
                     from NEFSC_GARFO.maine_herring_catch
                     where year ",
    years
  )
  if (!any(is.na(filterByArea))) {
    herr_qry <- paste0(
      herr_qry,
      " and stock_area in (",
      survdat:::sqltext(filterByArea),
      ")
                               order by stock_area"
    )
  }

  sql <- herr_qry

  # pull the data
  herr_catch <- DBI::dbGetQuery(channel, herr_qry) |>
    dplyr::as_tibble()

  # Convert number fields from characterr to numeric
  herr_catch <- herr_catch |>
    dplyr::mutate(
      YEAR = as.numeric(YEAR),
      MONTH = as.numeric(MONTH),
      STOCK_AREA = as.numeric(STOCK_AREA),
      NEGEAR = as.numeric(NEGEAR)
    )

  # aggregate landings and discards
  herring_data <- herr_catch |>
    dplyr::group_by(YEAR, MONTH, CATEGORY, STOCK_AREA, NEGEAR) |>
    dplyr::summarise(
      SPPLIVMT = sum(KEPTMT, na.rm = TRUE),
      DISCMT = sum(DISCMT, na.rm = TRUE),
      .groups = "drop"
    )

  return(list(data = herring_data, sql = sql))
}
