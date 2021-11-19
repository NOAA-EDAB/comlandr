#' Processes NAFO data to fix skates and hakes
#'
#' Apportions skates and hakes in the same way commercial data is handled - using survey
#'
#' @param nafoland Data frame. output from \code{\url{process_foreign_data}}
#'
#'
#'@return Data frame: NAFO data
#'
#'
#'@importFrom data.table ":="
#'@importFrom magrittr "%>%"
#'
#'@export

process_foreign_data_skate_hake <- function(nafoland){

  #Fix skates
  #get little skates and winter skates from skates(ns) - use survey in half years
  #Generate Half year variable in comland
  nafoland.skates <- nafoland[NESPP3 == 365, ]
  nafoland.skates[MONTH %in% 1:6, Half := 1]
  nafoland.skates[MONTH %in% 7:12, Half := 2]

  data.table::setkey(skate.hake.nafo,
                     YEAR,
                     Half,
                     EPU)

  nafoland.skates <- merge(nafoland.skates, skate.hake.nafo, by = key(skate.hake.nafo), all.x = T)

  nafoland.skates[NESPP3 == 365, little := little.per * SPPLIVMT]
  nafoland.skates[is.na(little), little := 0]

  nafoland.skates[NESPP3 == 365, winter := winter.per * SPPLIVMT]
  nafoland.skates[is.na(winter), winter := 0]

  nafoland.skates[NESPP3 == 365, other.skate := SPPLIVMT - (little + winter)]

  #Little (366), winter (367), skates(ns) (365)
  #put skates in nafoland format to merge back
  little <- nafoland.skates[, list(YEAR, Half, EPU, TONCL1, MONTH,
                                   NESPP3, NEGEAR, little)]
  little[, NESPP3 := 366L]
  data.table::setnames(little, "little", "SPPLIVMT")
  little <- little[SPPLIVMT > 0, ]

  winter <- nafoland.skates[, list(YEAR, Half, EPU, TONCL1, MONTH,
                                   NESPP3, NEGEAR, winter)]
  winter[, NESPP3 := 367L]
  data.table::setnames(winter, "winter", "SPPLIVMT")
  winter <- winter[SPPLIVMT > 0, ]

  other <- nafoland.skates[, list(YEAR, Half, EPU, TONCL1, MONTH,
                                  NESPP3, NEGEAR, other.skate)]
  other[, NESPP3 := 365L]
  data.table::setnames(other, "other.skate", "SPPLIVMT")
  other <- other[SPPLIVMT > 0, ]

  #merge all three and reformat for nafoland
  skates.add.back <- data.table::rbindlist(list(little, winter, other))

  skates.add.back[, Half := NULL]
  setcolorder(skates.add.back, names(nafoland))

  nafoland <- data.table::rbindlist(list(nafoland[NESPP3 != 365, ], skates.add.back))


  return(nafoland)
}
