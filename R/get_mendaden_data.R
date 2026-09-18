#' Pull Menhaden data from FOSS API
#'
#' @description
#' Pulls Menhaden data from the FOSS API
#'
#' @inheritParams get_comland_data
#'
#' @section FOSS API:
#'
#' You can use the Fisheries One Stop Shop ([FOSS](https://www.fisheries.noaa.gov/foss/f?p=215:200:1011421373172:Mail::::)) database through
#' a website or an API. You select a time period, a geographic region and a species to get the data you need
#'
#' @return Menhaden data as a data frame
#'
#' @export

get_menhaden_data <- function(filterByYear = NA, filterByArea = NA) {
  # Need to Map State to filterByArea (Statistical Areas).
  # These will then be used to map to EPUs if required
  # Need to incorporate filterByYear when the correct API syntax for vectors established

  ############### THIS IS WHAT WE WANT #############################
  # Define parameters used in API
  query_list <- list(
    state_name = list(
      "CONNECTICUT",
      "DELAWARE",
      "MAINE",
      "MARYLAND",
      "MASSACHUSETTS",
      "NEW HAMPSHIRE",
      "NEW JERSEY",
      "NEW YORK",
      "PENNSYLVANIA",
      "RHODE ISLAND",
      "VIRGINIA"
    ),
    year = list(`$between` = c("1950", "2026")), # Multiple years using $between
    name = list("MENHADEN, ATLANTIC", "MENHADENS"),
    collection = "Commercial"
  )
  ##################################################################

  # Test
  query_list <- list(
    state_name = "DELAWARE",
    year = list(`$between` = c("1950", "2026")), # Multiple years using $between
    ts_afs_name = "MENHADEN, ATLANTIC",
    collection = "Commercial"
  )

  # Convert the R list to a JSON string
  json_query_string <- jsonlite::toJSON(
    query_list,
    auto_unbox = TRUE,
    complex = "string"
  )
  json_query_string

  # Send the request to NOAAs API endpoint
  url <- "https://apps-st.fisheries.noaa.gov/ods/foss/landings/"

  response <- httr2::request(url) |>
    httr2::req_url_query(
      q = json_query_string,
      limit = 1000, # Return up to 500 records
      offset = 0
    )
  # display what will be sent to server
  response |> httr2::req_dry_run()
  response <- response |>
    httr2::req_perform()

  response

  # Parse the JSON response into data frame
  # If request was successful
  if (response$status_code == 200) {
    parsed_text <- response |> httr2::resp_body_string()
    parsed_data <- jsonlite::fromJSON(parsed_text, flatten = TRUE)

    # The data is nested in $items
    landings_df <- as.data.frame(parsed_data$items) |>
      dplyr::as_tibble()

    # reorder similar to GUI output for easy comparison
    landings <- landings_df |>
      dplyr::relocate(
        year,
        state_name,
        ts_afs_name,
        pounds,
        dollars,
        collection,
        ts_scientific_name,
        tsn,
        source
      ) |>
      dplyr::arrange(desc(year))
  } else {
    print(paste("Error:", response$status_code))
  }

  return(landings)
}
