#' Adjust species value for inflation
#'
#' Uses Economic data from the \href{Federal Reserve Bank of St Louis}{https://fred.stlouisfed.org/series/GDPDEF} to
#' adjusts species value to a reference year/month. The economic data is reported quarterly (Jan, Apr, Jul, Oct).
#' The value is held constant for months following the reported month
#'
#'
#'@param comland Data frame. master data frame containing species landings
#'@param refMonth Integer. Reference month
#'@param refYear Integer. Reference year
#'
#'@return comland data frame adjusted for inflation
#'
#'@noRd

adjust_inflation <- function(comland, refYear, refMonth) {
  #Pulling data
  message("Adjusting for inflation ...")

  #pull out comland data
  sql <- comland$sql
  comland <- comland$comland

  # pull in economic data. This is quarterly data
  # This gets updated by a cron job using github action getFred.yaml
  deflateData <- readRDS(system.file(
    "extdata/fred/fred.rds",
    package = "comlandr"
  ))

  # Pad missing months values. Use previous quarters value
  # A convoluted way of doing this ...
  fullgrid <- expand.grid(YEAR = unique(deflateData$YEAR), MONTH = 1:12) |>
    dplyr::left_join(deflateData, by = c("YEAR", "MONTH")) |>
    dplyr::arrange(YEAR, MONTH)

  # conform with existing format
  deflate <- fullgrid |>
    data.table::as.data.table()

  #deflate <- comlandr::deflate
  deflate.base <- deflate[YEAR == refYear & MONTH == refMonth, value]

  comland <- merge(comland, deflate, by = c('YEAR', 'MONTH'), all.x = T)
  comland[, SPPVALUE := round((SPPVALUE * deflate.base) / value)]

  #Remove extra column
  comland[, value := NULL]

  return(list(
    comland = comland[],
    sql = sql,
    pullDate = date(),
    functionCall = call
  ))
}
