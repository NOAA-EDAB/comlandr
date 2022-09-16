#' Extracts commercial discard data from Database
#'
#' Connects to obdbs and pulls discard data, calculates discard to kept ratios,
#' and applies to landings data obtained using \code{get_comland_data}.
#'
#'@param channel an Object inherited from \link[DBI]{DBIConnection-class}. This object is used to connect
#' to communicate with the database engine. (see \code{\link[dbutils]{connect_to_database}})
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
#'@section File Creation:
#'
#'A file containing the data.table above will also be saved to the users machine in the directory provided
#'
#'
#'@importFrom data.table ":="
#'@importFrom magrittr "%>%"
#'
#'@export

get_comdisc_data <- function(channel, comland, 
                             aggArea = F, 
                             areaDescription = 'EPU', propDescription = 'MeanProp',
                             aggGear = F,
                             fleetDescription = 'Fleet') {
  

  call <- dbutils::capture_function_call()
  
  #Use data from comland object
  filterByYear <- range(comland[[1]][, YEAR])[1]:range(comland[[1]][, YEAR])[2]

  #Pull raw data
  comdisc.raw <- comlandr::get_comdisc_raw_data(channel, filterByYear)#, filterByArea)

  #Aggregate areas
  if(aggArea){
    userAreas <- comland$userAreas
    comdisc.raw <- comlandr::aggregate_area(comdisc.raw, userAreas, areaDescription,
                                            propDescription, useForeign = F,
                                            applyPropValue = F)
  } 

  #Aggregate gears
  if(aggGear){
    userGears <- comland$userGears
    comdisc.raw <- aggregate_gear(comdisc.raw, userGears, fleetDescription)
  } 
  
  #Calculate the discard to kept ratio
  dk <- comlandr::calc_DK(comdisc.raw, areaDescription, fleetDescription)
  
  #Apply the discard to kept ratio
  comdisc <- comlandr::calc_discards(comland, dk, areaDescription, fleetDescription)

  message("Some data may be CONFIDENTIAL ... DO NOT disseminate without proper Non-disclosure agreement.")
  
  return(list(comdisc = comdisc[],
              sql     = comdisc.raw$sql,
              call    = c(call, comdisc.raw$call),
              userAreas = comdisc.raw$userAreas,
              userGears = comdisc.raw$userGears))

}

