#' aggregate data by time, gear, tonnage
#'
#'aggregate by quarter year, half year, major gear, and small/large TC
#'
#'@param comland Data frame. master data frame containing species landings
#'@param GEARS List. Designates the NEGEAR codes that comprise a fishing fleet.
#'
#'@return Aggreagted comland data
#'
#' @noRd
#'
comland_aggregate <- function(comland,GEARS){

  comland[MONTH %in% 1:3,   QY := 1]
  comland[MONTH %in% 4:6,   QY := 2]
  comland[MONTH %in% 7:9,   QY := 3]
  comland[MONTH %in% 10:12, QY := 4]
  comland[MONTH == 0,       QY := 0]

  comland[MONTH %in% 1:6,  HY := 1]
  comland[MONTH %in% 7:12, HY := 2]
  comland[MONTH == 0,      HY := 0]


  comland[NEGEAR %in% GEARS$otter,     GEAR := 'otter']
  comland[NEGEAR %in% GEARS$dredge.sc, GEAR := 'dredge.sc']
  comland[NEGEAR %in% GEARS$pot,       GEAR := 'pot']
  comland[NEGEAR %in% GEARS$longline,  GEAR := 'longline']
  comland[NEGEAR %in% GEARS$seine,     GEAR := 'seine']
  comland[NEGEAR %in% GEARS$gillnet,   GEAR := 'gillnet']
  comland[NEGEAR %in% GEARS$midwater,  GEAR := 'midwater']
  comland[NEGEAR %in% GEARS$dredge.o,  GEAR := 'dredge.o']
  comland[NEGEAR == 0,           GEAR := 'unknown']
  comland[is.na(GEAR),           GEAR := 'other']
  comland[, GEAR := as.factor(GEAR)]

  comland[TONCL1 %in% 1:3, SIZE := 'small']
  comland[TONCL1 > 3,      SIZE := 'large']
  comland[TONCL1 == 0,     SIZE := 'unknown']
  comland[, SIZE := as.factor(SIZE)]


  data.table::setkey(comland,
                     YEAR,
                     QY,
                     HY,
                     GEAR,
                     SIZE,
                     AREA,
                     NESPP3,
                     UTILCD)

  comland.agg <- comland[, list(sum(SPPLIVMT), sum(SPPVALUE)), by = key(comland)]

  data.table::setnames(comland.agg, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))


  return(comland.agg)
}
