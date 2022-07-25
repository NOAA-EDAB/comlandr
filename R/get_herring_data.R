#' Processes herring data
#'
#'Herring Data comes from the state of Maine.
#'
#'@param channel DBI object. connection object for database access
#'@param comland Data frame. master data frame containing species landings
#'
#'@return Processed Herring data added to comland
#'
#'@importFrom data.table ":=" "key"
#'
#' @noRd
#' @export

get_herring_data <- function(channel, comland, filterByYear, filterByArea) {
  
  #Pulling data
  message("Pulling Atlantic herring data from maine_herring_catch ...")
  
  if(is.na(filterByYear[1])){
    years <- ">= 1963"
  }else{
    years <- paste0("in (", survdat:::sqltext(filterByYear), ")")
  }
  
  herr.qry <- paste0("select year, month, category, stock_area, negear, gearname, 
                     keptmt, discmt
                     from maine_herring_catch
                     where year ", years)
  if(!is.na(filterByArea[1])){
    herr.qry <- paste0(herr.qry, " and stock_area in (", survdat:::sqltext(filterByArea), ")
                               order by stock_area")
  }
  
  sql <- c(comland$sql, herr.qry)
  
  #pull out comland data
  comland <- comland$comland

  herr.catch <- data.table::as.data.table(DBI::dbGetQuery(channel, herr.qry))
  
  #Convert number fields from chr to num
  numberCols <- c('YEAR', 'MONTH', 'STOCK_AREA', 'NEGEAR', 'GEARNAME')
  herr.catch[, (numberCols):= lapply(.SD, as.numeric), .SDcols = numberCols]

  #Aggregate data
  data.table::setkey(herr.catch, YEAR, MONTH, CATEGORY, STOCK_AREA, NEGEAR)

  herring <- herr.catch[, list(sum(KEPTMT, na.rm = T), sum(DISCMT, na.rm = T)), 
                        by = key(herr.catch)]

  data.table::setnames(herring, c('STOCK_AREA', 'V1', 'V2'),
                       c('AREA', 'SPPLIVMT', 'DISCMT'))

  #Using averages from comland to fill in categories
  herring[, MKTCAT := 5]

  herring[, TONCL1 := 3]

  herring[, UTILCD := 0]
  
  herring[, MESHCAT := 'LG']

  #compute price/utilization from CF tables
  herring.comland <- comland[NESPP3 == 168, ]

  #Price from comland
  herring.price <- herring.comland[, (sum(SPPVALUE, na.rm = T) / sum(SPPLIVMT, na.rm = T)), 
                                   by = c('YEAR', 'MONTH')]

  data.table::setnames(herring.price, 'V1', 'price')

  herring <- merge(herring, herring.price, by = c('YEAR', 'MONTH'), all.x = T)

  #Use 1964 prices for < 1964
  herring[YEAR < 1964, price := mean(herring[YEAR == 1964, price])]
  #Calculate SPPVALUE from price
  herring[, SPPVALUE := round(price * SPPLIVMT)]

  #Utilization from comland
  herring.util <- herring.comland[, sum(SPPLIVMT), by = c('YEAR', 'MONTH', 'UTILCD')]
  data.table::setnames(herring.util, 'V1', 'SPPLIVMT')

  herring.util[, SPPLIVMT.ALL := sum(SPPLIVMT), by = c('YEAR', 'MONTH')]

  herring.util[, Prop := SPPLIVMT/SPPLIVMT.ALL]

  data.table::setorder(herring.util, YEAR, MONTH, Prop)

  herring.util[, cum.prop := cumsum(Prop), by = c('YEAR', 'MONTH')]

  #Apply proportions to Maine data set
  #Not pulled all the time - current through 2017
  herring[, Total := sum(SPPLIVMT), by = c('YEAR', 'MONTH')]

  herring[, Prop := SPPLIVMT / Total]

  data.table::setorder(herring, YEAR, MONTH, Prop)
  herring[, cum.prop := cumsum(Prop), by = c('YEAR', 'MONTH')]

  for(iyear in unique(herring.util[, YEAR])){
    for(imonth in unique(herring.util[YEAR == iyear, MONTH])){
      cum.prop.low <- 0
      for(iutil in herring.util[YEAR == iyear & MONTH == imonth, UTILCD]){
        cum.prop.high <- herring.util[YEAR == iyear & MONTH == imonth &
                                        UTILCD == iutil, cum.prop]
        herring[YEAR == iyear & MONTH == imonth & cum.prop <= cum.prop.high &
                  cum.prop > cum.prop.low, UTILCD := iutil]
        cum.prop.low <- cum.prop.high
      }
    }
  }

  #fix column headings
  herring[, c('Total', 'Prop', 'cum.prop', 'price', 'DISCMT') := NULL]
  herring[, NESPP3 := 168]

  #Add Nationality Flag
  herring[, US := T]
  herring[CATEGORY == 'NAFO', US := F]
  herring[, CATEGORY := NULL]
  
  data.table::setcolorder(herring, names(comland))

  #remove herring from data pull and add in Maine numbers
  comland <- data.table::rbindlist(list(comland[NESPP3 != 168, ], herring))


  return(list(comland = comland[], 
              sql     = sql))

}
