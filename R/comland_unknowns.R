#' Processes unknown data
#'
#'drop unknown species/landings
#'
#'@param comland Data frame. master data frame containing species landings
#'
#'@return Processed unknowns in comland
#'
#'

comland_unknowns <- function(comland){

  comland[NEGEAR == 999,  NEGEAR := 0]
  comland[is.na(TONCL1),  TONCL1 := 0]
  comland[is.na(AREA),    AREA   := as.factor(0)]
  comland[AREA == 999,    AREA   := as.factor(0)]
  comland[is.na(MKTCAT),  MKTCAT := 0]
  comland[is.na(UTILCD),  UTILCD := 0]

  #1 - drop unknown species/landings
  comland <- comland[NESPP3 != 0 & SPPLIVMT != 0, ]

  #Sumarry tables
  #missing area
  #known.area <-   comland[AREA != 0, sum(SPPLIVMT), by = NESPP3]
  #unknown.area <- comland[AREA == 0, sum(SPPLIVMT), by = NESPP3]
  #data.table::setnames(known.area,   "V1", "AREA.MT.known")
  #data.table::setnames(unknown.area, "V1", "AREA.MT.unknown")
  #missing.table <- merge(known.area, unknown.area, by = 'NESPP3', all = T)
  #
  #missing.table[is.na(AREA.MT.known),   AREA.MT.known   := 0]
  #missing.table[is.na(AREA.MT.unknown), AREA.MT.unknown := 0]
  #missing.table[, AREA.Ratio := AREA.MT.unknown / AREA.MT.known]
  #
  ##missing month
  #known.month <-   comland[MONTH != 0, sum(SPPLIVMT), by = NESPP3]
  #unknown.month <- comland[MONTH == 0, sum(SPPLIVMT), by = NESPP3]
  #data.table::setnames(known.month,   "V1", "MONTH.MT.known")
  #data.table::setnames(unknown.month, "V1", "MONTH.MT.unknown")
  #missing.table <- merge(missing.table, known.month,   by = 'NESPP3', all = T)
  #missing.table <- merge(missing.table, unknown.month, by = 'NESPP3', all = T)
  #
  #missing.table[is.na(MONTH.MT.known),   MONTH.MT.known   := 0]
  #missing.table[is.na(MONTH.MT.unknown), MONTH.MT.unknown := 0]
  #missing.table[, MONTH.Ratio := MONTH.MT.unknown / MONTH.MT.known]
  #
  ##missing gear
  #known.gear <-   comland[NEGEAR != 0, sum(SPPLIVMT), by = NESPP3]
  #unknown.gear <- comland[NEGEAR == 0, sum(SPPLIVMT), by = NESPP3]
  #data.table::setnames(known.gear,   "V1", "GEAR.MT.known")
  #data.table::setnames(unknown.gear, "V1", "GEAR.MT.unknown")
  #missing.table <- merge(missing.table, known.gear,   by = 'NESPP3', all = T)
  #missing.table <- merge(missing.table, unknown.gear, by = 'NESPP3', all = T)
  #
  #missing.table[is.na(GEAR.MT.known),   GEAR.MT.known   := 0]
  #missing.table[is.na(GEAR.MT.unknown), GEAR.MT.unknown := 0]
  #missing.table[, GEAR.Ratio := GEAR.MT.unknown / GEAR.MT.known]
  #
  ##missing tonnage class
  #known.tc <-   comland[TONCL1 != 0, sum(SPPLIVMT), by = NESPP3]
  #unknown.tc <- comland[TONCL1 == 0, sum(SPPLIVMT), by = NESPP3]
  #data.table::setnames(known.tc,   "V1", "TC.MT.known")
  #data.table::setnames(unknown.tc, "V1", "TC.MT.unknown")
  #missing.table <- merge(missing.table, known.tc,   by = 'NESPP3', all = T)
  #missing.table <- merge(missing.table, unknown.tc, by = 'NESPP3', all = T)
  #
  #missing.table[is.na(TC.MT.known),   TC.MT.known   := 0]
  #missing.table[is.na(TC.MT.unknown), TC.MT.unknown := 0]
  #missing.table[, TC.Ratio := TC.MT.unknown / TC.MT.known]
  #
  #write.csv(missing.table, paste(out.dir, "\\Missing_table.csv", sep = ''), row.names = F)
  #
  return(comland)
}
