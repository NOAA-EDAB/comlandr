#' Extracts commercial discard data from Database
#'
#' Connects to obdbs and pulls discard data, calculates discard to kept ratios,
#' and applies to landings data obtained using \code{get_comland_data}.
#'
#'@inheritParams get_comland_data
#'@param comland Data frame. Result of \code{get_comland_data}
#'@param extendTS Boolean. Should the DK (Discard to kept) ratio be extended and applied
#'to landings beyond observer coverage time period (Discards started in 1989). Default = T
#'
#'
#'
#'
#'@return Data frame (data.table) (n x 10)
#'Each row of the data.table represents a species record for a given tow/trip
#'
#'\item{YEAR}{Year of trip/tow}
#'\item{MONTH}{Month of trip/tow}
#'\item{NEGEAR}{Fishing gear used on trip/tow}
#'\item{TONCL1}{Tonnage class of the fishing vessel}
#'\item{NESPP3}{Species code (3 charachters)}
#'\item{NESPP4}{Species code and market code (4 characters)}
#'\item{AREA}{Statistical area in which species was reportly caught}
#'\item{UTILCD}{Utilization code}
#'\item{SPPLIVLB}{live weight (landed = "n") or landed weight (landed="y") in lbs}
#'\item{SPPVALUE}{The value of landed catch to the nearest dollar (U.S.), paid to fisherman by dealer, for a given species.}
#'
#'
#'@importFrom data.table ":="
#'@importFrom magrittr "%>%"
#'
#'@export

get_comdisc_data <- function(
  channel,
  comland,
  aggArea = F,
  areaDescription = 'EPU',
  propDescription = 'MeanProp',
  aggGear = F,
  fleetDescription = 'Fleet',
  extendTS = T
) {
  call <- dbutils::capture_function_call()

  #Use data from comland object to determine time range in which to calculate discards
  filterByYear <- min(comland$comland$YEAR):max(comland$comland$YEAR)

  #filterByYear <- range(comland[[1]][, YEAR])[1]:range(comland[[1]][, YEAR])[2]

  #Pull raw data
  comdisc.raw <- get_comdisc_raw_data(channel, filterByYear) #, filterByArea)

  message("Observer data pulled ...")

  #Aggregate areas
  if (aggArea) {
    userAreas <- comland$userAreas
    comdisc.raw <- aggregate_area(
      comdisc.raw,
      userAreas,
      areaDescription,
      propDescription,
      useForeign = F,
      applyProp = F
    )
  }

  #Aggregate gears
  if (aggGear) {
    userGears <- comland$userGears
    comdisc.raw <- aggregate_gear(comdisc.raw, userGears, fleetDescription)
  }

  message("Calculating DK ratios ...")
  #Calculate the discard to kept ratio
  dk <- calc_DK(comdisc.raw, areaDescription, fleetDescription)

  #Extend dk ratios beyond observer data
  if (extendTS) {
    message("Extend DK ratio back in time ...")
    dk.mean <- dk[,
      .(meanDK = mean(DK, na.rm = T)),
      by = c('NESPP3', areaDescription, fleetDescription)
    ]

    #Create data table with unobserved periods
    areas <- unique(dk[, .SD, .SDcols = areaDescription])[[1]]
    fleets <- unique(dk[, .SD, .SDcols = fleetDescription])[[1]]
    spp <- unique(dk[, NESPP3])

    blank <- data.table::CJ(
      YEAR = filterByYear,
      Area = areas,
      Fleet = fleets,
      NESPP3 = spp
    )

    data.table::setnames(
      blank,
      c('Area', 'Fleet'),
      c(areaDescription, fleetDescription)
    )

    dk.blank <- merge(
      blank,
      dk,
      by = c('YEAR', areaDescription, fleetDescription, 'NESPP3'),
      all.x = T
    )

    #Replace NAs with Mean value
    dk <- merge(
      dk.blank,
      dk.mean,
      by = c('NESPP3', areaDescription, fleetDescription),
      all.x = T
    )
    dk[is.na(DK), DK := meanDK]
    dk[, meanDK := NULL]
  }

  message("Apply DK ratio ...")
  #Apply the discard to kept ratio
  comdisc <- calc_discards(comland, dk, areaDescription, fleetDescription)

  message(
    "Some data may be CONFIDENTIAL ... DO NOT disseminate without proper Non-disclosure agreement."
  )

  return(list(
    comdisc = comdisc[],
    sql = comdisc.raw$sql,
    call = c(call, comdisc.raw$call),
    userAreas = comdisc.raw$userAreas,
    userGears = comdisc.raw$userGears
  ))
}
