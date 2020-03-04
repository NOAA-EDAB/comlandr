#' Processes herring data
#'
#'Herring Data comes from the state of Maine.
#'
#'@param channel DBI object. connection object for database access
#'@param comland Data frame. master data frame containing species landings
#'
#'@return Processed Herring data
#'
#'

comland_herring <- function(channel,comland) {

  herr.qry <- "select year, month, stock_area, negear, gearname, keptmt, discmt
             from maine_herring_catch"

  herr.catch <- as.data.table(DBI::dbGetQuery(channel, herr.qry))
  data.table::setkey(herr.catch, YEAR, MONTH, STOCK_AREA, NEGEAR)

  herring <- herr.catch[, list(sum(KEPTMT), sum(DISCMT)), by = key(herr.catch)]
  data.table::setnames(herring, c('STOCK_AREA', 'V1', 'V2'),
                       c('AREA', 'SPPLIVMT', 'DISCMT'))
  herring$YEAR <- as.integer(herring$YEAR)
  herring$MONTH <- as.double(herring$MONTH)

  #Using averages from comland to fill in categories
  herring[, MKTCAT := 5]
  herring[, TONCL1 := 2]
  herring[, UTILCD := 0]

  #compute price/utilization from CF tables
  herring.comland <- comland[NESPP3 == 168, ]
  #Price from comland
  herring.price <- herring.comland[, (sum(SPPVALUE) / sum(SPPLIVMT)), by = c('YEAR', 'MONTH')]
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

  return(herring)

}
