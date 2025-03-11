#' Pulls FRED data from federal reserve bank
#'
#' From Jan 1981. Data is quarterly

get_fred <- functon() {
  FRED_key <-  Sys.getenv("API_KEY")
  fredr::fredr_set_key(FRED_key)
  deflators <- fredr::fredr(
    series_id = "GDPDEF",
    observation_start = as.Date("1981-01-01"),
    frequency = "q")

  formatted <- deflators |>
    dplyr::mutate(YEAR = lubridate::year(date),
                  MONTH = lubridate::month(date)) |>
    dplyr::select(YEAR,MONTH,value)

  return(formatted)

}

