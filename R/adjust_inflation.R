#' Adjust species value for inflation
#'
#'Reads in data from ??? website and adjusts species value
#'
#'@param comland Data frame. master data frame containing species landings
#'@param refmonth Integer. Reference month
#'@param refyear Integer. Reference year
#'
#'@return comland data frame adjusted for inflation
#'
#'


adjust_inflation <- function(comland,refyear,refmonth){
  temp <- tempfile()
  download.file("http://download.bls.gov/pub/time.series/wp/wp.data.3.ProcessedFoods", temp)
  inflate <- data.table::as.data.table(read.delim(temp))
  unlink(temp)

  inflate[, series_id := gsub(" ", "", inflate[, series_id])]
  deflate <- inflate[series_id == "WPU0223", ]
  deflate[, MONTH := as.numeric(substr(period, 2, 3))]
  data.table::setnames(deflate, c('year', 'value'), c('YEAR', 'PPI'))
  deflate <- deflate[, list(YEAR, MONTH, PPI)]

  #Set yearly deflator to 0 instead of 13 to match unknown month designation
  deflate[MONTH == 13, MONTH := 0]
  deflate.base <- deflate[YEAR == refyear & MONTH == refmonth, PPI]

  comland <- merge(comland, deflate, by = c('YEAR', 'MONTH'), all.x = T)
  comland[, SPPVALUE := round((SPPVALUE * deflate.base) / PPI)]

  #Remove extra column
  comland[, PPI := NULL]
  return(comland)
}
