#' aggregate data by epu
#'
#'aggregate by epu
#'
#'@param comland.agg Data frame. master data frame containing species landings
#'@param nafoland.agg Data frame. processed NAFO data
#'@param EPUS List. Designates the stat areas that comprise an EPU. Default = EPUs (lazily loaded data)
#'@param foreign Character String. Mark foreign landings and keep seperate.
#'
#'@return Aggregated comland data
#'


aggregate_by_epu <- function(comland.agg,nafoland.agg,EPUS,foreign){

  #Assign EPU based on statarea
  comland.agg[AREA %in% EPUS$GOM$statAreas, EPU := 'GOM']
  comland.agg[AREA %in% EPUS$GB$statAreas,  EPU := 'GB']
  comland.agg[AREA %in% EPUS$MAB$statAreas, EPU := 'MAB']
  comland.agg[AREA %in% EPUS$SS$statAreas,  EPU := 'SS']
  comland.agg[is.na(EPU),    EPU := 'OTHER']
  comland.agg[, EPU := factor(EPU, levels = c('GOM', 'GB', 'MAB', 'SS', 'OTHER'))]

  data.table::setkey(comland.agg,
                     YEAR,
                     NESPP3,
                     QY,
                     GEAR,
                     SIZE,
                     EPU,
                     UTILCD)

  comland.agg <- comland.agg[, list(sum(SPPLIVMT), sum(SPPVALUE)), by = key(comland.agg)]

  data.table::setnames(comland.agg, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

  #Merge comland and nafoland
  data.table::setcolorder(nafoland.agg, names(comland.agg))

  if(foreign == 'y'){
    comland.agg[,  US := T]
    nafoland.agg[, US := F]
  }

  comland.nafo <- data.table::rbindlist(list(comland.agg, nafoland.agg))

  #Remove Menhaden data
  #save(comland.nafo, file = paste(out.dir, "comland_Menhaden.RData", sep = ''))
  comland <- comland.nafo[NESPP3 != 221, ]

  return(comland)

}
