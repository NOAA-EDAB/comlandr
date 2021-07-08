#' Processes NAFO data for comlandr use
#'
#' Adds cfdbs NESPP3, NEGEAR, TONCL1 codes to data.
#' Adds EPUs
#'
#'@param channel an Object inherited from \link[DBI]{DBIConnection-class}. This object is used to connect
#' to communicate with the database engine. (see \code{\link[dbutils]{connect_to_database}})
#'@param nafoland Data frame. output from \code{\url{get_foreign_data}}
#'@param EPUs Data frame.
#'
#'
#'@return Data frame: NAFO data
#'
#'
#'@importFrom data.table ":="
#'@importFrom magrittr "%>%"
#'
#'@export

clean_foreign_data <- function(channel, nafoland, EPUs = NULL){

  #Rectify NAFO codes with US codes
  #Species
  data.table::setnames(nafoland,
                       c('Year', 'GearCode', 'Tonnage', 'NAFOCode'),
                       c('YEAR', 'NAFOGEAR', 'TONCL1', 'NAFOSPP'))

  ## pull NESPP3 codes and add to nafo landings
  speciesInfo <- data.table::as.data.table(get_species(channel)$data )
  speciesInfo <- speciesInfo[,list(NESPP3,NAFOSPP)]
  speciesInfo[, NESPP3:= as.integer(NESPP3) ]
  speciesInfo[, NAFOSPP:= as.integer(NAFOSPP)]
  speciesInfo <- speciesInfo %>% dplyr::distinct()

  ###########################################################################
  # caution many to one relationship NESPP3 -> NAFO. Duplicated catch data
  # Fix this. eg. Cod NESPP3 = 81,82
  #############################################################################
  nafoland <- dplyr::left_join(nafoland, speciesInfo, by = c("Code"="NAFOSPP"))



  ## pull NEGEAR codes and add to nafo landings
  nafoGears <- unique(nafoland$NAFOGEAR) # find unique gears in nafo data

  gear <- tibble::as_tibble(get_gears(channel)$data) %>%
    dplyr::select(NEGEAR,NAFOGEAR) %>%
    dplyr::mutate(NEGEAR = as.integer(NEGEAR),NAFOGEAR = as.integer(NAFOGEAR)) %>%
    dplyr::filter(NAFOGEAR %in% nafoGears) %>%
    dplyr::distinct() %>%
    data.table::as.data.table()

  # join NEGEAR2 with nafo data
  ###########################################################################
  # caution many to one relationship NAFO -> NEGEAR2. Duplicated catch data
  # Fix this. eg. Cod NESPP3 = 81,82
  #############################################################################




  nafoland <- merge(nafoland, gear, by = 'NAFOGEAR', all.x = T)

  #fix codes
  nafoland[NAFOGEAR == 8,  NEGEAR := 50L]
  nafoland[NAFOGEAR == 9,  NEGEAR := 370L]
  nafoland[NAFOGEAR == 19, NEGEAR := 58L]
  nafoland[NAFOGEAR == 49, NEGEAR := 60L]
  nafoland[NAFOGEAR == 56, NEGEAR := 21L]




  ## Assign tonnage class to comlandr categories
  nafoland[TONCL1 %in% 1:3, SIZE := 'small']
  nafoland[(TONCL1 > 3 & TONCL1 <= 9),      SIZE := 'large']
  nafoland[(TONCL1 == 0 | TONCL1 = 99),     SIZE := 'unknown']
  nafoland[is.na(TONCL1),     SIZE := 'unknown']
  nafoland[, SIZE := as.factor(SIZE)]


  ###############################################################
  ###############################################################
  # What to do with catches of species in unknown months?
  # What to do with catches in UNKNOWN areas
  ###############################################################
  ###############################################################


  return(nafoland)
}
