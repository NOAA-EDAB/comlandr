#' Process Herring data
#'
#' @description
#' Combines Herring pull data from State of Maine with comlandr data pull
#' Assigns UTIL CODEs and other stuff
#'
#'
#'
#' @noRd

process_herring_data <- function(herring_data, comland_data, useForeign) {
  comland <- comland_data$comland
  herring <- herring_data$data |>
    dplyr::mutate(MARKET_CODE = "UN", TONCL2 = 30, UTILCD = 0, MESHCAT = "LG")

  browser()
  #Using averages from comland to fill in categories
  # herring[, MARKET_CODE := 'UN']
  #
  # herring[, TONCL2 := 30]
  #
  # herring[, UTILCD := 0]
  #
  # herring[, MESHCAT := 'LG']

  #compute price/utilization from CF tables
  herring.comland <- comland[NESPP3 == 168, ]

  #Price from comland
  herring.price <- herring.comland[,
    (sum(SPPVALUE, na.rm = T) / sum(SPPLIVMT, na.rm = T)),
    by = c('YEAR', 'MONTH')
  ]

  data.table::setnames(herring.price, 'V1', 'price')

  herring <- merge(herring, herring.price, by = c('YEAR', 'MONTH'), all.x = T)

  #Use 1964 prices for < 1964
  herring[YEAR < 1964, price := mean(herring[YEAR == 1964, price])]
  #Calculate SPPVALUE from price
  herring[, SPPVALUE := round(price * SPPLIVMT)]

  #Utilization from comland
  herring.util <- herring.comland[,
    sum(SPPLIVMT),
    by = c('YEAR', 'MONTH', 'UTILCD')
  ]
  data.table::setnames(herring.util, 'V1', 'SPPLIVMT')

  herring.util[, SPPLIVMT.ALL := sum(SPPLIVMT), by = c('YEAR', 'MONTH')]

  herring.util[, Prop := SPPLIVMT / SPPLIVMT.ALL]

  data.table::setorder(herring.util, YEAR, MONTH, Prop)

  herring.util[, cum.prop := cumsum(Prop), by = c('YEAR', 'MONTH')]

  #Apply proportions to Maine data set
  #Not pulled all the time - current through 2017
  herring[, Total := sum(SPPLIVMT), by = c('YEAR', 'MONTH')]

  herring[, Prop := SPPLIVMT / Total]

  data.table::setorder(herring, YEAR, MONTH, Prop)
  herring[, cum.prop := cumsum(Prop), by = c('YEAR', 'MONTH')]

  for (iyear in unique(herring.util[, YEAR])) {
    for (imonth in unique(herring.util[YEAR == iyear, MONTH])) {
      cum.prop.low <- 0
      for (iutil in herring.util[YEAR == iyear & MONTH == imonth, UTILCD]) {
        cum.prop.high <- herring.util[
          YEAR == iyear & MONTH == imonth & UTILCD == iutil,
          cum.prop
        ]
        herring[
          YEAR == iyear &
            MONTH == imonth &
            cum.prop <= cum.prop.high &
            cum.prop > cum.prop.low,
          UTILCD := iutil
        ]
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

  if (!useForeign) {
    comland <- comland[US == T, ]
  }

  return(list(comland = comland, sql = c(comland$sql, herring_data$sql)))
}
