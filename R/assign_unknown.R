#' Assigns unknown catch variables
#'
#' Impute unknown variables associated with landings.
#'
#'
# @inheritParams 
#' @param unkVar Vector. Catch variables to be imputed.  Should be in the order
#'  you would like them to be solved.
#'  
#' @param knStrata Vector. Catch variables to be used to impute \code{unkVar}. An
#'  \code{unkVar} can be included in this list if it is used to solve a second 
#'  \code{unkVar}.  Should be in the order of most restrictive to least restrictive.
#'
#' @return Returns a \code{comlandData} data.table.
#'
#'
#' @importFrom data.table ":="
#'
#'@family comland
#'
#' @export


assign_unknown <- function (comData, unkVar, 
                            knStrata = c('NESPP3', 'YEAR', 'HY', 'QY', 'MONTH',
                                         'NEGEAR', 'TONCL1', 'AREA')) {
  
  call <- c(comData$call, dbutils::capture_function_call())
  
  #Pulling data
  message("Imputing unknown catch parameters ...")
  
  #pull out data
  sql <- comData$sql
  comdata <- data.table::copy(comData[[1]])
  
  #Assign Quarter/Half Years
  comdata[MONTH == 0,       QY := 0]
  comdata[MONTH %in% 1:3,   QY := 1]
  comdata[MONTH %in% 4:6,   QY := 2]
  comdata[MONTH %in% 7:9,   QY := 3]
  comdata[MONTH %in% 10:12, QY := 4]
  
  comdata[QY == 0,     HY := 0]
  comdata[QY %in% 1:2, HY := 1]
  comdata[QY %in% 3:4, HY := 2]
  
  for(ivar in 1:length(unkVar)){
    if(unkVar[ivar] %in% knStrata){
      strata <- knStrata[which(knStrata != unkVar[ivar])]  
    } else {
      strata <- knStrata
    }
    
    #Change names of strata
    strata.code <- paste0('STR', 1:length(strata))
    data.table::setnames(comdata, c(strata, unkVar[ivar]), c(strata.code, 'VAR'))
    
    #Identify records with known and unknown variable
    known   <- comdata[!VAR %in% c(0, 999), ]
    known   <- known[!is.na(VAR), ]
    unknown <- comdata[ VAR %in% c(0, 999) |  is.na(VAR), ]
    
    #Need record ID to calculate proportions correctly
    unknown[, ID := 1:nrow(unknown)]
    
    #set output to known records only
    comdata.out <- data.table::copy(known)
    
    if(nrow(unknown) > 0){
      cat(paste('Total', unkVar[ivar], 'unknown records', nrow(unknown), '\n'))
      
      for(i in length(strata.code):1){
        #Identify columns that are not part of the stratification
        ext.col <- names(comdata)[which(!names(comdata) %in% c(strata.code[1:i], 
                                                               'VAR', 'SPPLIVMT'))]
        
        #Remove from known
        known[, c(names(known)[which(names(known) %in% ext.col)]) := NULL]
        
        #Sum landings per stratification
        known.sum <- known[, .(VARMT = sum(SPPLIVMT)), by = c(strata.code[1:i], 
                                                              'VAR')]
        
        #Match records but keep unmatched records to carry forward
        match <- merge(known.sum, unknown, by = strata.code[1:i], all.y = T, 
                       allow.cartesian = T)
        
        #Remove unmatched for next round
        unknown <- match[is.na(VAR.x), ]
        unknown[, c('VAR.x', 'VARMT') := NULL]
        data.table::setnames(unknown, 'VAR.y', 'VAR')
        
        #Remove unmatched records from merge
        match <- match[!is.na(VAR.x), ]
        
        #Determine proportion of known catch per area
        match[, totlivmt := sum(VARMT), by = c(strata.code[1:i], 'ID')]
          #Catch zeros that lead to NaN
          match[totlivmt == 0, totlivmt := 1]
        match[, prop := VARMT / totlivmt]
        
        #Proportion catch from unknown areas to known areas
        match[, newlivmt := SPPLIVMT * prop]
        
        #Drop extra columns
        match <- match[, c('VAR.y', 'VARMT', 'totlivmt', 'prop', 'SPPLIVMT') := NULL]
        data.table::setnames(match, c('VAR.x', 'newlivmt'), c('VAR', 'SPPLIVMT'))
        
        #Append new entries to output
        comdata.out <- data.table::rbindlist(list(comdata.out, match), use.names = T)
        cat(paste('Unknown records remaining', nrow(unknown), '\n'))
      }
    }
    #Revert names for subsequent runs and the output
    data.table::setnames(comdata, c(strata.code, 'VAR'), c(strata, unkVar[ivar]))
    data.table::setnames(comdata.out, c(strata.code, 'VAR'), c(strata, unkVar[ivar]))
  }
  
  #Drop QY and HY
  comdata.out[, c('QY', 'HY') := NULL]
  
  #Add changes back into comdata
  comData[[1]] <- comdata.out[]
  
  return(comData[])
}
