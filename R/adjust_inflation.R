#' Adjust species value for inflation
#'
#'Reads in data from Bureau of Labor statistics website and adjusts species value
#'What data is fetched?
#'
#'
#'@param comland Data frame. master data frame containing species landings
#'@param refMonth Integer. Reference month
#'@param refYear Integer. Reference year
#'
#'@return comland data frame adjusted for inflation
#'
#'@noRd




adjust_inflation <- function(comland, refYear, refMonth){

  call <- c(comland$call, dbutils::capture_function_call())

  #Pulling data
  message("Adjusting for inflation ...")

  #pull out comland data
  sql <- comland$sql
  comland <- comland$comland

  #This isn't working right now - using downloaded file
  # temp <- tempfile()
  # download.file("http://download.bls.gov/pub/time.series/wp/wp.data.3.ProcessedFoods", temp)
  #inflate <- data.table::as.data.table(read.delim(comlandr::wp.data.3.ProcessedFoods))
  #unlink(temp)

  # inflate[, series_id := gsub(" ", "", inflate[, series_id])]
  # deflate <- inflate[series_id == "WPU0223", ]
  # deflate[, MONTH := as.numeric(substr(period, 2, 3))]
  # data.table::setnames(deflate, c('year', 'value'), c('YEAR', 'PPI'))
  # deflate <- deflate[, list(YEAR, MONTH, PPI)]

  #Set yearly deflator to 0 instead of 13 to match unknown month designation
  deflate <- comlandr::deflate
  deflate[MONTH == 13, MONTH := 0]
  deflate.base <- deflate[YEAR == refYear & MONTH == refMonth, PPI]

  comland <- merge(comland, deflate, by = c('YEAR', 'MONTH'), all.x = T)
  comland[, SPPVALUE := round((SPPVALUE * deflate.base) / PPI)]

  #Remove extra column
  comland[, PPI := NULL]

  return(list(comland      = comland[],
              sql          = sql,
              pullDate     = date(),
              functionCall = call))
}
