#' Assigns unknown catch variables
#'
#' Impute unknown variables associated with landings.
#'
#'
# @inheritParams 
#' @param na.keep Boolean. Logical value to indicate whether original strata names
#'  should be retained.
#'
#' @return Returns a \code{comdiscData} data.table with one additional column labeled
#'  with the value of \code{areaDescription}
#'
#' \item{areaDescription}{The name of the region (found in \code{areaPolygon})
#'  that a record in \code{surveyData} is assigned to}
#'
#' @importFrom magrittr "%>%"
#'
#'@family comland
#'
#' @export


assign_unknown <- function (comland, var, 
                            aggStrata = c('NESPP3', 'YEAR', 'HY', 'QY', 'MONTH',
                                          'NEGEAR', 'TONCL1')) {
  
  #Assign Quarter/Half Years
  comland[MONTH == 0,       QY := 0]
  comland[MONTH %in% 1:3,   QY := 1]
  comland[MONTH %in% 4:6,   QY := 2]
  comland[MONTH %in% 7:9,   QY := 3]
  comland[MONTH %in% 10:12, QY := 4]
  
  comland[QY == 0,     HY := 0]
  comland[QY %in% 1:2, HY := 1]
  comland[QY %in% 3:4, HY := 2]
  
  #Change names of strata
  strata <- paste0('STR', 1:length(aggStrata))
  data.table::setnames(comland, c(aggStrata, var), c(strata, 'VAR'))
  
  #Identify records with known and unknown areas
  known   <- comland[!VAR %in% c(0, 999), ]
  known   <- known[!is.na(VAR), ]
  unknown <- comland[ VAR %in% c(0, 999) |  is.na(VAR), ]
  
  #set output to known records only
  comland.out <- data.table::copy(known)
  
  if(nrow(unknown) > 0){
    cat(paste('Total', var, 'unknown records', nrow(unknown), '\n'))
    
    for(i in length(strata):2){
      #Identify columns that are not part of the stratification
      ext.col <- names(comland)[which(!names(comland) %in% c(strata[1:i], 
                                                             'VAR', 'SPPLIVMT'))]
      
      #Remove from known
      known[, c(names(known)[which(names(known) %in% ext.col)]) := NULL]
      
      #Sum landings per stratification
      known.sum <- known[, .(VARMT = sum(SPPLIVMT)), by = c(strata[1:i], 'VAR')]
      
      #Match records but keep unmatched records to carry forward
      match <- merge(known.sum, unknown, by = strata[1:i], all.y = T)
      
      #Remove unmatched for next round
      unknown <- match[is.na(VAR.x), ]
      unknown[, c('VAR.x', 'VARMT') := NULL]
      data.table::setnames(unknown, 'VAR.y', 'VAR')
      
      #Remove unmatched records from merge
      match <- match[!is.na(VAR.x), ]
      
      #Determine proportion of known catch per area
      match[, totlivmt := sum(VARMT), by = c(strata[1:i])]
      match[, prop := VARMT / totlivmt]
      
      #Proportion catch from unknown areas to known areas
      match[, newlivmt := SPPLIVMT * prop]
      
      #Drop extra columns
      match <- match[, c('VAR.y', 'VARMT', 'totlivmt', 'prop', 'SPPLIVMT') := NULL]
      data.table::setnames(match, c('VAR.x', 'newlivmt'), c('VAR', 'SPPLIVMT'))
      
      #Append new entries to output
      comland.out <- data.table::rbindlist(list(comland.out, match), use.names = T)
      cat(paste('Unknown records remaining', nrow(unknown), '\n'))
    }
  }
  
  #Revert names for output
  data.table::setnames(comland.out, c(strata, 'VAR'), c(aggStrata, var))
  
  #Drop QY and HY
  comland.out[, c('QY', 'HY') := NULL]
  
  return(comland.out)
}
