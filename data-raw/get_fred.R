#' Pulls FRED data from federal reserve bank
#'
#' From Jan 1981. Data is Monthly
#' Producer Price Index by Commodity: Processed Foods and Feeds: Unprocessed and Prepared Seafood (WPU0223)
#' # https://fred.stlouisfed.org/series/WPU0223

get_fred <- function() {
  FRED_key <- Sys.getenv("API_KEY")
  fredr::fredr_set_key(FRED_key)
  deflators <- fredr::fredr(
    #    series_id = "GDPDEF",
    series_id = "WPU0223",
    observation_start = as.Date("1981-01-01"),
    frequency = "m"
  )

  formatted <- deflators |>
    dplyr::mutate(
      YEAR = lubridate::year(date),
      MONTH = lubridate::month(date)
    ) |>
    dplyr::select(YEAR, MONTH, value)

  fn <- "datapull.txt"
  file.create(here::here("data-raw", fn))
  dateCreated <- Sys.time()
  cat(paste0(dateCreated, "\n"), file = here::here("data-raw", fn))

  return(formatted)
}
