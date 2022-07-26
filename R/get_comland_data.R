#' Extracts commercial data from Database
#'
#' Connects to cfdbs and pulls fields from WOLANDS, WODETS, CFDETS
#'
#'@param channel an Object inherited from \link[DBI]{DBIConnection-class}. This object is used to connect
#' to communicate with the database engine. (see \code{\link[dbutils]{connect_to_database}})
#'@param endyear Numeric Scalar. Final year of query.
#'@param landed Character String. Use landed weight ("y" - meatwt) for scallops and clams instead of live weight ("n" - livewt).
#'@param out.dir path to directory where final output will be saved
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

get_comland_data <- function(channel, filterByYear = NA, filterByArea = NA, useLanded = T,
                             removeParts = T, useHerringMaine = T, useForeign = T,
                             refYear = NA, refMonth = NA, disagSkatesHakes = T,
                             aggArea = F, userAreas = comlandr::mskeyAreas,
                             areaDescription = 'EPU', propDescription = 'MeanProp',
                             aggGear = F, userGears = comlandr::mykeyGears,
                             fleetDescription = 'Fleet', unkVar = 'Area',
                             knStrata = c('NESPP3', 'YEAR', 'HY', 'QY', 'MONTH',
                                          'NEGEAR', 'TONCL1', 'AREA')) {
  

  call <- dbutils::capture_function_call()

  #Pull raw data
  comland <- comlandr::get_comland_raw_data(channel, filterByYear, filterByArea,
                                            useLanded, removeParts)

  #Pull herring data from the state of Maine
  if(useHerringMaine) comland <- comlandr::get_herring_data(channel, comland,
                                                            filterByYear, filterByArea)

  #Pull foreign landings
  if(useForeign){
    #Look up NAFO divisions that contain Stat areas
    if (all(!is.na(filterByArea))) {
      NAFOAreas <- comlandr::get_areas(channel)$data %>%
        dplyr::select(AREA,NAFDVCD) %>%
        dplyr::filter(AREA %in% filterByArea) %>%
        dplyr::pull(NAFDVCD) %>%
        unique() %>%
        as.integer()
      filterByArea <- c(filterByArea, NAFOAreas)
    }
    
    #Pull data and process to look like comland data
    comland.foreign <- comlandr::get_foreign_data(filterByYear, filterByArea)
    comland.foreign <- comlandr::process_foreign_data(channel, comland.foreign, 
                                                      useHerringMaine)
   
    #Combine foreign landings
    comland$comland <- data.table::rbindlist(list(comland$comland, comland.foreign), 
                                             use.names = T)
  } 
  
  
  #Apply correction for inflation
  if(!is.na(refYear)) comland <- comlandr::adjust_inflation(comland, refYear, refMonth)

  #Disaggregate skates and hakes
  if(disagSkatesHakes) comland <- comlandr::disaggregate_skates_hakes(comland, channel,
                                                            filterByYear, filterByArea)

  #Aggregate areas
  if(aggArea) comland <- aggregate_area(comland, userAreas, areaDescription,
                                          propDescription, useForeign)

  #Aggregate gears
  if(aggGear) comland <- aggregate_gear(comland, userGears, fleetDescription)
  
  #Impute unknown catch variables
  if(!is.null(unkVar)) comland <- assign_unknown(comland, unkVar, knStrata)

  comland$call <- call

  message("Some data may be CONFIDENTIAL ... DO NOT disseminate without proper Non-disclosure agreement.")
  return(comland)

}

