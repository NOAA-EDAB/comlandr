#' Extracts commercial data from Database
#'
#' Connects to cfdbs and pulls fields from WOLANDS, WODETS, CFDETS
#'
#'@param channel an Object inherited from \link[DBI]{DBIConnection-class}. This object is used to connect
#' to communicate with the database engine. (see \code{\link[dbutils]{connect_to_database}})
#'@param filterByYear numeric vector
#'@param filterByArea numeric vector
#'@param useLanded boolean
#'@param removeParts boolean
#'@param useHerringMaine boolean
#'@param useForeign boolean
#'@param refYear numeric
#'@param refMonth numeric
#'@param disaggSkatesHakes boolean
#'@param aggArea boolean
#'@param userAreas data frame
#'@param areaDescription character string
#'@param propDescription character string
#'@param applyPropLand boolean
#'@param applyPropValue boolean
#'@param aggGear boolean
#'@param userGears data frame
#'@param fleetDescription character string
#'@param unkVar character vector
#'@param knStrata character vector
#'
#'@return Data frame (data.table) (n x 12)
#'Each row of the data.table represents a species record for a given tow/trip
#'
#'\item{YEAR}{Year of trip/tow}
#'\item{MONTH}{Month of trip/tow}
#'\item{NEGEAR}{Fishing gear used on trip/tow}
#'\item{TONCL2}{Tonnage class of the fishing vessel (2 digit value)}
#'\item{NESPP3}{Species code (3 charachters)}
#'\item{MARKET_CODE}{market code (2 characters)}
#'\item{MESHCAT}{Code to describe the mesh size for a trawl vessel}
#'\item{AREA/EPU}{Statistical area/ Ecological Production Unit in which species was reportly caught}
#'\item{UTILCD}{Utilization code}
#'\item{US}{Landing from the USA vessels or foreign vessels}
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


get_comland_data <- function(channel, filterByYear = NA,
                             filterByArea = NA, useLanded = T, removeParts = T,
                             useHerringMaine = T, useForeign = T, refYear = NA,
                             refMonth = NA, disagSkatesHakes = T, aggArea = F,
                             userAreas = comlandr::mskeyAreas,
                             areaDescription = 'EPU', propDescription = 'MeanProp',
                             applyPropLand = T, applyPropValue = T,
                             aggGear = F, userGears = comlandr::mskeyGears,
                             fleetDescription = 'Fleet',
                             unkVar = c('MONTH','NEGEAR','AREA'),
                             knStrata = c('HY', 'QY','MONTH','NEGEAR', 'TONCL2', 'AREA')) {



  call <- dbutils::capture_function_call()

  #Pull raw data
  comland <- comlandr::get_comland_raw_data(channel,
                                            filterByYear, filterByArea,
                                            useLanded, removeParts)

  #Pull herring data from the state of Maine
  if(useHerringMaine){
    comland <- comlandr::get_herring_data(channel, comland,
                                          filterByYear, filterByArea,
                                          useForeign)
  }

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
                                                      useLanded, useHerringMaine)

    #Combine foreign landings
    comland$comland <- data.table::rbindlist(list(comland$comland, comland.foreign),
                                             use.names = T)
  }


  #Apply correction for inflation
  if(!is.na(refYear)) comland <- comlandr::adjust_inflation(comland, refYear, refMonth)

  #Disaggregate skates and hakes
  if(disagSkatesHakes) comland <- comlandr::disaggregate_skates_hakes(comland,
                                                                      channel,
                                                            filterByYear, filterByArea)

  #Aggregate areas
  if(aggArea) comland <- comlandr::aggregate_area(comland, userAreas,
                                                  areaDescription, propDescription,
                                                  useForeign, channel,
                                                  applyPropLand, applyPropValue)

  #Aggregate gears
  if(aggGear) comland <- comlandr::aggregate_gear(comland, userGears, fleetDescription)

  #Impute unknown catch variables
  if(!is.null(unkVar)) comland <- comlandr::assign_unknown(comland, unkVar, knStrata)

  comland$call <- call

  message("Some data may be CONFIDENTIAL ... DO NOT disseminate without proper Non-disclosure agreement.")
  return(comland)

}

