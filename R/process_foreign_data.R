#' Processes NAFO data for comlandr use
#'
#' Assigns NAFO Gear codes to NEGEAR codes and NEGEAR codes to fleets
#' Assigns NAFO Species to NESPP3 species
#' Assigns NAFO Divcodes to EPUs
#' Assigns NAFO tonnage to tonnage classes
#'
#' Fills in missing data by including from 21A (SS EPU only)
#'
#'
#'@param channel an Object inherited from \link[DBI]{DBIConnection-class}. This object is used to connect
#' to communicate with the database engine. (see \code{\link[dbutils]{connect_to_database}})
#'@param nafoland Data frame. output from \code{\url{get_foreign_data}}
#'@param EPUs Data frame. Currently a place holder
#'
#'
#'@return Data frame: NAFO data
#'
#'
#'@importFrom data.table ":="
#'@importFrom magrittr "%>%"
#'
#'@export

process_foreign_data <- function(channel, nafoland, EPUs = NULL,GEARS=comlandr::GEARs){

  ## All code copied directly from nafo_comland
  nafoland <- nafoland[Divcode %in% c(47, 51:56, 61:63) & Code > 3, ]
  nafoland <- nafoland[, Country :=NULL]

  nafoland <- nafoland[SPPLIVMT != 0,]


  nafoland[, EPU := factor(NA, levels = c('GOM', 'GB', 'MAB', 'SS', 'OTHER'))]
  nafoland[Divcode == 47,             EPU := 'SS']
  nafoland[Divcode == 51,             EPU := 'GOM']
  nafoland[Divcode %in% c(52, 54:56), EPU := 'GB']
  nafoland[Divcode %in% c(53, 61:63), EPU := 'MAB']
  nafoland[is.na(EPU),                EPU := 'OTHER']

  nafoland[, Divcode := NULL]
  ##Fix missing Scotian Shelf data from 21B
  SS.nafo <- as.data.table(read.csv(system.file("extdata","SS_NAFO_21A.csv",package="comlandr"), skip = 8))

  #Add NAFOSPP code to SS.nafo
  nafo.spp <- as.data.table(read.csv(system.file("extdata","species.txt",package="comlandr")))
  data.table::setnames(nafo.spp, "Abbreviation", "Species_ASFIS")
  nafo.spp <- nafo.spp[, list(Code, Species_ASFIS)]

  SS.nafo <- merge(SS.nafo, nafo.spp, by = 'Species_ASFIS', all.x = T)

  #Only grab missing data
  SS.nafo <- SS.nafo[Year %in% c(2003, 2008, 2009), ]

  data.table::setkey(SS.nafo,
                     Year,
                     Code)

  SS.land <- SS.nafo[, sum(Catch...000.Kg.), by = key(SS.nafo)]

  data.table::setnames(SS.land, "V1", "SPPLIVMT")

  #Add GearCode, Tonnage, Month, and EPU
  SS.land[, GearCode := 99]
  SS.land[, Tonnage  := 0]
  SS.land[, MONTH    := 0]
  SS.land[, EPU      := 'SS']
  SS.land[, QY       := 1]

  setcolorder(SS.land, names(nafoland))

  nafoland <- data.table::rbindlist(list(nafoland, SS.land))

  #Rectify NAFO codes with US codes
  #Species
  data.table::setnames(nafoland,
                       c('Year', 'GearCode', 'Tonnage', 'Code'),
                       c('YEAR', 'NAFOGEAR', 'TONCL1', 'NAFOSPP'))

  spp <- as.data.table(DBI::dbGetQuery(channel, "select NAFOSPP, NESPP3 from CFSPP"))
  spp$NAFOSPP <- as.integer(spp$NAFOSPP)
  spp$NESPP3 <- as.integer(spp$NESPP3)
  #Fix missing NAFO codes
  missing.spp <- data.table::data.table(NAFOSPP = c(110, 141, 189, 480, 484, 487, 488, 489),
                                        NESPP3  = c(240, 509, 512, 366, 368, 367, 370, 369))
  spp <- data.table::rbindlist(list(spp, missing.spp))

  data.table::setkey(spp, NAFOSPP)
  spp <- unique(spp, by = key(spp))

  #Fix many to one relationships
  spp[NAFOSPP == 199, NESPP3 := 524]
  spp[NAFOSPP == 299, NESPP3 := 525]
  spp[NAFOSPP == 469, NESPP3 := 359]
  spp[NAFOSPP == 499, NESPP3 := 526]
  spp[NAFOSPP == 529, NESPP3 := 764]
  spp[NAFOSPP == 699, NESPP3 := 899]

  spp$NAFOSPP <- as.integer(spp$NAFOSPP)
  spp$NESPP3 <- as.integer(spp$NESPP3)

  nafoland <- merge(nafoland, spp, by = 'NAFOSPP', all.x = T)


  #fix codes
  nafoland[NAFOSPP == 309, NESPP3 := 150L]
  nafoland[NAFOSPP == 462, NESPP3 := 481L]
  nafoland[NAFOSPP == 464, NESPP3 := 355L]
  nafoland[NAFOSPP == 468, NESPP3 := 493L]
  nafoland[NAFOSPP == 704, NESPP3 := 817L]

  #remove species without a match
  nafoland <- nafoland[!is.na(NESPP3), ]

  #Remove herring catch - already included from Maine Data earlier
  nafoland <- nafoland[NESPP3 != 168, ]

  #Gearcodes

  gear <- as.data.table(DBI::dbGetQuery(channel, "select NEGEAR, NAFOGEAR from Gear"))
  gear$NEGEAR <- as.integer(gear$NEGEAR)
  gear$NAFOGEAR <- as.integer(gear$NAFOGEAR)

  gear <- unique(gear, by = 'NAFOGEAR')

  nafoland <- merge(nafoland, gear, by = 'NAFOGEAR', all.x = T)

  #fix codes
  nafoland[NAFOGEAR == 8,  NEGEAR := 50L]
  nafoland[NAFOGEAR == 9,  NEGEAR := 370L]
  nafoland[NAFOGEAR == 19, NEGEAR := 58L]
  nafoland[NAFOGEAR == 49, NEGEAR := 60L]
  nafoland[NAFOGEAR == 56, NEGEAR := 21L]

  #Tonnage
  nafoland[TONCL1 == 7, TONCL1 := 6L]

  #Drop NAFO codes
  nafoland[, c('NAFOGEAR', 'NAFOSPP') := NULL]


  #aggregate nafo landings
  #2 - aggregate by quarter year, half year, major gear, and small/large TC
  nafoland[MONTH %in% 1:3,   QY := 1]
  nafoland[MONTH %in% 4:6,   QY := 2]
  nafoland[MONTH %in% 7:9,   QY := 3]
  nafoland[MONTH %in% 10:12, QY := 4]
  nafoland[MONTH == 0,       QY := 1]

  nafoland[NEGEAR %in% GEARS$otter,     GEAR := 'otter']
  nafoland[NEGEAR %in% GEARS$dredge.sc, GEAR := 'dredge.sc']
  nafoland[NEGEAR %in% GEARS$pot,       GEAR := 'pot']
  nafoland[NEGEAR %in% GEARS$longline,  GEAR := 'longline']
  nafoland[NEGEAR %in% GEARS$seine,     GEAR := 'seine']
  nafoland[NEGEAR %in% GEARS$gillnet,   GEAR := 'gillnet']
  nafoland[NEGEAR %in% GEARS$midwater,  GEAR := 'midwater']
  nafoland[NEGEAR %in% GEARS$dredge.o,  GEAR := 'dredge.o']
  nafoland[NEGEAR == 99,          GEAR := 'unknown']
  nafoland[is.na(GEAR),           GEAR := 'other']
  nafoland[, GEAR := as.factor(GEAR)]

  nafoland[TONCL1 %in% 1:3, SIZE := 'small']
  nafoland[TONCL1 > 3,      SIZE := 'large']
  nafoland[TONCL1 == 0,     SIZE := 'unknown']
  nafoland[, SIZE := as.factor(SIZE)]

  data.table::setkey(nafoland,
                     YEAR,
                     QY,
                     GEAR,
                     SIZE,
                     EPU,
                     NESPP3)

  nafoland.agg <- nafoland[, sum(SPPLIVMT), by = key(nafoland)]

  data.table::setnames(nafoland.agg, "V1", "SPPLIVMT")

  #Create dummy variable for value
  nafoland.agg[, SPPVALUE := 0]
  nafoland.agg[, UTILCD := 0]

  return(nafoland.agg)
}
