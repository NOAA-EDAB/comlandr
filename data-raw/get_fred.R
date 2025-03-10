#' need to incoprporate this into the package to pull
#' directly from th federal reserve
FRED_key <-  Sys.getenv("API_KEY")
fredr::fredr_set_key(FRED_key)
deflators <- fredr::fredr(
  series_id = "GDPDEF",
  observation_start = as.Date("1981-01-01"),
  frequency = "q")
print(deflators)

