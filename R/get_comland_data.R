#' Extracts and processes commercial data from Database
#'
#' Connects to Population dynamics Database STOCKEFF to pull US landings data.
#' Data is also pulled from NAFO (foreign landings) and the State of Maine (Herring).
#' These sources of data are then aggregated, species value is adjusted to a user defined reference year,
#' skate and hake landings (often reported as an unclassified category) are split based on bottom trawl survey,
#' and missing values are imputed. For more information regarding these methods
#' see \code{vignette("Overview", package = "comlandr")}
#'
#'@param channel an Object inherited from \code{\link[ROracle]{Oracle}}. This object is used to connect
#' to communicate with the database engine. (see \code{\link[dbutils]{connect_to_database}})
#'@param filterByYear numeric vector. Years to retrieve data for (Default = NA, pull all years)
#'@param filterByArea numeric vector. Statistical Areas to retrieve data for (Default = NA, pull all areas)
#'@param useLanded boolean. Default = T
#'@param removeParts boolean. Remove species parts (Heads, wings, etc), Default = T
#'@param useHerringMaine boolean. Pull data from Maine Herring database or use herring data in commercial landings database (Default = T)
#'@param useForeign boolean. Pull foreign data from NAFO. Default = T
#'@param refYear numeric. Reference year to use when adjusting species value
#'@param refMonth numeric. Reference month to use when adjusting species value
#'@param disagSkatesHakes boolean. Partition skates and hake unclassified landings into species (Default = T)
#'@param aggArea boolean. Aggregate Statistical Areas into larger spatial units (Default = F)
#'@param userAreas data frame. Spatial units in which Statistical areas should be aggregated (eg. \code{\link{mskeyAreas}})
#'@param areaDescription character string. Field name in \code{userAreas} denoting spatial unit. (Default = "EPU")
#'@param propDescription character string. Field name in \code{userAreas} denoting the scaling factor. (Default = "MeanProp")
#'@param applyProp boolean. Apply the proportions in userAreas to the landings and value (Default = T)
#'@param aggGear boolean. Aggregate NEGEAR codes to larger "fleets" (Default = F)
#'@param userGears data frame. Fleet designations in which NEGEAR codes should be grouped (eg.  \code{\link{mskeyGears}})
#'@param fleetDescription character string. Field name in \code{userGears} denoting Fleet. (Default = "Fleet")
#'@param unkVar character vector. Variables in the data, with have missing values, that you wish to assign a value to. (unkVar = NULL skips assigning unknowns)
#'@param knStrata character vector. Variables in the data that you wish to use to use to assign values to \code{unkVar}
#'
#'@return Data frame (data.table) (n x 12)
#'Each row of the data.table represents a species record for a given tow/trip
#'
#'\item{YEAR}{Year of trip/tow}
#'\item{MONTH}{Month of trip/tow}
#'\item{NEGEAR/Fleet}{Fishing gear used on trip/tow or aggregated to Fleet}
#'\item{TONCL2}{Tonnage class of the fishing vessel (2 digit value)}
#'\item{NESPP3}{Species code (3 charachters)}
#'\item{MARKET_CODE}{market code (2 characters)}
#'\item{MESHCAT}{Code to describe the mesh size for a trawl vessel}
#'\item{AREA/EPU}{Statistical area/ Ecological Production Unit in which species was reportly caught}
#'\item{UTILCD}{Utilization code}
#'\item{US}{Landing from the USA vessels or foreign vessels}
#'\item{SPPLIVMT}{Weight in metric tons.}
#'\item{SPPVALUE}{The value of landed catch to the nearest dollar (U.S.), paid to fisherman by dealer, for a given species.}
#'
#'@importFrom data.table ":="
#'@importFrom magrittr "%>%"
#'
#'@section Argument choices:
#'
#'Some of the arguments rely on the choice of others.
#'
#'If \code{aggArea = T} then the user must also supply a \code{userAreas} data frame
#' and a \code{areaDescription} string to denote the field in \code{userArea} which
#' maps the statistical area to the larger spatial unit.
#'
#'If \code{aggGear = T} then the user must also supply a \code{userGears} data frame
#' and a \code{fleetDescription} string to denote the field in \code{userGears} which
#' maps the NEGEAR codes to the fleet designation.
#'
#' If either \code{aggArea = T} or \code{aggGear = T} and the user wants to assign values to
#' missing variables (i.e. if \code{unkVar} != NULL) then \code{unkVar} and \code{knStrata} need to
#' include the values of \code{areaDescription} and \code{fleetDescription} respectively
#'
#'@export


get_comland_data <- function(channel,
                             filterByYear = NA,
                             filterByArea = NA,
                             useLanded = T,
                             removeParts = T,
                             useHerringMaine = T,
                             useForeign = T,
                             refYear = NA,
                             refMonth = NA,
                             disagSkatesHakes = T,
                             aggArea = F,
                             userAreas = comlandr::mskeyAreas,
                             areaDescription = 'EPU',
                             propDescription = 'MeanProp',
                             applyProp = F,
                             aggGear = F,
                             userGears = comlandr::mskeyGears,
                             fleetDescription = 'Fleet',
                             unkVar = c('MONTH','NEGEAR','AREA'),
                             knStrata = c('HY', 'QY','MONTH','NEGEAR', 'TONCL2', 'AREA')) {


  # saves initial the function call and returns it with the data pull
  call <- dbutils::capture_function_call()

  # checks to make sure argument values are aligned
  check_argument_validation(aggArea,
                            userAreas,
                            areaDescription,
                            propDescription,
                            applyProp,
                            aggGear,
                            userGears,
                            fleetDescription,
                            unkVar,
                            knStrata
                            )


  #Pull raw data
  comland <- comlandr::get_comland_raw_data(channel,
                                            filterByYear, filterByArea,
                                            useLanded, removeParts)

  #Impute unknown catch variables
  if(!is.null(unkVar)) comland <- assign_unknown(comland, unkVar, knStrata)


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
  if(!is.na(refYear)) comland <- adjust_inflation(comland, refYear, refMonth)

  #Disaggregate skates and hakes
  if(disagSkatesHakes) comland <- disaggregate_skates_hakes(comland,
                                                            channel,
                                                            filterByYear,
                                                            filterByArea)

  #Aggregate areas
  if(aggArea) comland <- aggregate_area(comland,
                                        userAreas,
                                        areaDescription,
                                        propDescription,
                                        useForeign,
                                        channel,
                                        applyProp)

  #Aggregate gears
  if(aggGear) comland <- aggregate_gear(comland, userGears, fleetDescription)

  comland$call <- call

  message("Some data may be CONFIDENTIAL ... DO NOT disseminate without proper Non-disclosure agreement.")
  return(comland)

}

