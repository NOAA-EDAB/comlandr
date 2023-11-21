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
                            knStrata = c('HY', 'QY', 'MONTH', 'NEGEAR', 'TONCL1',
                                         'AREA')) {
  
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
  
  #Assign size classes
  #comdata[TONCL1 %in% 1:3, SIZE := 1] #Small
  #comdata[TONCL1 > 3,      SIZE := 2] #Large
  
  for(ivar in 1:length(unkVar)){
    if(unkVar[ivar] %in% knStrata){
      strata <- knStrata[which(knStrata != unkVar[ivar])]  
    } else {
      strata <- knStrata
    }
    
    #Change names of strata
    strata.code <- paste0('STR', 1:length(strata))
    data.table::setnames(comdata, c(strata, unkVar[ivar]), c(strata.code, 'VAR'))
    
    #Set unknown Var to NA
    comdata[VAR == 0,   VAR := NA]
    comdata[VAR == 999, VAR := NA]
    
    #Set all unknowns to NA
    for(ist in 1:length(strata.code)){
      data.table::setnames(comdata, strata.code[ist], 'ST')
      comdata[ST == 0,   ST := NA]
      comdata[ST == 999, ST := NA]
      data.table::setnames(comdata, 'ST', strata.code[ist])
    }
    
    #Identify records with known and unknown variable
    known   <- comdata[!is.na(VAR), ]
    unknown <- comdata[ is.na(VAR), ]
    
    #Need record ID to calculate proportions correctly
    unknown[, ID := 1:nrow(unknown)]
    
    #set output to known records only
    comdata.out <- data.table::copy(known)
    
    #Create combinations of strata variables to match
    combos <- c()
    for(inum in length(strata.code):1){
      #combinations of strata codes
      num.combos <- data.table::setDT(list(gtools::combinations(length(strata.code), 
                                                                inum, strata.code)))
      combos <- data.table::rbindlist(list(combos, num.combos), fill = T)
    }
    
    
    if(nrow(unknown) > 0){
      cat(paste('Total', unkVar[ivar], 'unknown records', nrow(unknown), '\n'))
      
      
      for(icom in 1:nrow(combos)){
        if(nrow(unknown) > 0){
          stra.combo <- combos[icom, ]
          num.stra.var <- stra.combo[, which(!is.na(stra.combo))]
          stra.combo <- unlist(stra.combo[, ..num.stra.var])
          
          #Identify columns that are not part of the stratification
          ext.col <- names(comdata)[which(!names(comdata) 
                                          %in% c('NESPP3', 'YEAR', stra.combo, 'VAR',
                                                 'SPPLIVMT'))]
            
          #Remove extra columns from known
          known.simple <- copy(known)
          known.simple <- known.simple[, c(names(known)[which(names(known) %in%
                                                                ext.col)]) := NULL]
            
          #Need to only match with known variables
          known.all <- known.simple[complete.cases(known.simple), ]
            
          #Sum landings per stratification
          known.sum <- known.all[, .(VARMT = sum(SPPLIVMT)), 
                                 by = c('NESPP3', 'YEAR', stra.combo, 'VAR')]
            
          #Match records but keep unmatched records to carry forward
          match <- merge(known.sum, unknown, by = c('NESPP3', 'YEAR', stra.combo),
                         all.y = T, allow.cartesian = T)
            
          #Remove unmatched for next round
          unknown <- match[is.na(VAR.x), ]
          unknown[, c('VAR.x', 'VARMT') := NULL]
          data.table::setnames(unknown, 'VAR.y', 'VAR')
            
          #Remove unmatched records from merge
          match <- match[!is.na(VAR.x), ]
            
          #Determine proportion of known catch per variable
          match[, totlivmt := sum(VARMT), by = c(stra.combo, 'ID')]
          #Catch zeros that lead to NaN
          match[totlivmt == 0, totlivmt := 1]
            
          match[, prop := VARMT / totlivmt]
            
          #Proportion catch from unknown variable to known variable
          match[, newlivmt := SPPLIVMT * prop]
          match[, newvalue := SPPVALUE * prop]
            
          #Drop extra columns
          match <- match[, c('VAR.y', 'VARMT', 'totlivmt', 'prop', 'SPPLIVMT', 
                             'SPPVALUE', 'ID') := NULL]
          data.table::setnames(match, c('VAR.x', 'newlivmt', 'newvalue'), 
                               c('VAR', 'SPPLIVMT', 'SPPVALUE'))
            
          #Append new entries to output
          comdata.out <- data.table::rbindlist(list(comdata.out, match), 
                                               use.names = T)
          cat(paste('After using', length(stra.combo), 
          'variables: Unknown records remaining', nrow(unknown), '\n'))
          }
        }
      ##Still no match - use 3 or 5 year window then drop year
      if(nrow(unknown) > 0){
        #Remove extra columns from known
        known.all <- copy(known)
        known.all <- known.all[, list(YEAR, NESPP3, VAR, SPPLIVMT)]
        
        #3 year window
        years <- unique(unknown[, YEAR])
        known.sum <- c()
        for(iyr in 1:length(years)){
          known.3 <- known.all[YEAR %in% (years[iyr] - 1):(years[iyr] + 1), ]
          
          #Sum landings per stratification
          sum.3 <- known.3[, .(VARMT = sum(SPPLIVMT)), by = c('NESPP3', 'VAR')]
          sum.3[, YEAR := years[iyr]]
          known.sum <- data.table::rbindlist(list(known.sum, sum.3))
          }
          
          #Match unknown records for the year
          match <- merge(known.sum, unknown, by = c('YEAR', 'NESPP3'), all.y = T, 
                         allow.cartesian = T)
          
          #Remove unmatched for next round
          unknown <- match[is.na(VAR.x), ]
          unknown[, c('VAR.x', 'VARMT') := NULL]
          data.table::setnames(unknown, 'VAR.y', 'VAR')
          
          #Remove unmatched records from merge
          match <- match[!is.na(VAR.x), ]
          
          #Determine proportion of known catch per variable
          match[, totlivmt := sum(VARMT), by = c(stra.combo, 'ID')]
          #Catch zeros that lead to NaN
          match[totlivmt == 0, totlivmt := 1]
          
          match[, prop := VARMT / totlivmt]
          
          #Proportion catch from unknown variable to known variable
          match[, newlivmt := SPPLIVMT * prop]
          match[, newvalue := SPPVALUE * prop]
          
          #Drop extra columns
          match <- match[, c('VAR.y', 'VARMT', 'totlivmt', 'prop', 'SPPLIVMT', 
                             'SPPVALUE', 'ID') := NULL]
          data.table::setnames(match, c('VAR.x', 'newlivmt', 'newvalue'), 
                               c('VAR', 'SPPLIVMT', 'SPPVALUE'))
          
          #Append new entries to output
          comdata.out <- data.table::rbindlist(list(comdata.out, match), 
                                               use.names = T)

          cat(paste('After using 3 year window: Unknown records remaining', nrow(unknown), '\n'))
          
          if(nrow(unknown) > 0){
            #5 Year window
            years <- unique(unknown[, YEAR])
            known.sum <- c()
            for(iyr in 1:length(years)){
              known.5 <- known.all[YEAR %in% (years[iyr] - 2):(years[iyr] + 2), ]
            
              #Sum landings per stratification
              sum.5 <- known.5[, .(VARMT = sum(SPPLIVMT)), by = c('NESPP3', 'VAR')]
              sum.5[, YEAR := years[iyr]]
              known.sum <- data.table::rbindlist(list(known.sum, sum.5))
              }
            
            #Match unknown records for the year
            match <- merge(known.sum, unknown, by = c('YEAR', 'NESPP3'), all.y = T, 
                           allow.cartesian = T)
          
            #Remove unmatched for next round
            unknown <- match[is.na(VAR.x), ]
            unknown[, c('VAR.x', 'VARMT') := NULL]
            data.table::setnames(unknown, 'VAR.y', 'VAR')
          
            #Remove unmatched records from merge
            match <- match[!is.na(VAR.x), ]
          
            #Determine proportion of known catch per variable
            match[, totlivmt := sum(VARMT), by = c(stra.combo, 'ID')]
            #Catch zeros that lead to NaN
            match[totlivmt == 0, totlivmt := 1]
          
            match[, prop := VARMT / totlivmt]
          
            #Proportion catch from unknown variable to known variable
            match[, newlivmt := SPPLIVMT * prop]
            match[, newvalue := SPPVALUE * prop]
          
            #Drop extra columns
            match <- match[, c('VAR.y', 'VARMT', 'totlivmt', 'prop', 'SPPLIVMT', 
                               'SPPVALUE', 'ID') := NULL]
            data.table::setnames(match, c('VAR.x', 'newlivmt', 'newvalue'), 
                                 c('VAR', 'SPPLIVMT', 'SPPVALUE'))
          
            #Append new entries to output
            comdata.out <- data.table::rbindlist(list(comdata.out, match), 
                                                 use.names = T)
            cat(paste('After using 5 year window: Unknown records remaining', 
                      nrow(unknown), '\n'))
          }
        }
      ##Still no match - match to species
      if(nrow(unknown) > 0){
        #Remove extra columns from known
        known.all <- copy(known)
        known.all <- known.all[, list(NESPP3, VAR, SPPLIVMT)]
          
        #Sum landings per stratification
        known.sum <- known.all[, .(VARMT = sum(SPPLIVMT)), by = c('NESPP3', 'VAR')]
        
        #Match unknown records for the year
        match <- merge(known.sum, unknown, by = 'NESPP3', all.y = T, 
                       allow.cartesian = T)
        
        #Remove unmatched for next round
        unknown <- match[is.na(VAR.x), ]
        unknown[, c('VAR.x', 'VARMT') := NULL]
        data.table::setnames(unknown, 'VAR.y', 'VAR')
        
        #Remove unmatched records from merge
        match <- match[!is.na(VAR.x), ]
        
        #Determine proportion of known catch per variable
        match[, totlivmt := sum(VARMT), by = c(stra.combo, 'ID')]
        #Catch zeros that lead to NaN
        match[totlivmt == 0, totlivmt := 1]
        
        match[, prop := VARMT / totlivmt]
        
        #Proportion catch from unknown variable to known variable
        match[, newlivmt := SPPLIVMT * prop]
        match[, newvalue := SPPVALUE * prop]
        
        #Drop extra columns
        match <- match[, c('VAR.y', 'VARMT', 'totlivmt', 'prop', 'SPPLIVMT', 
                           'SPPVALUE', 'ID') := NULL]
        data.table::setnames(match, c('VAR.x', 'newlivmt', 'newvalue'), 
                             c('VAR', 'SPPLIVMT', 'SPPVALUE'))
        
        #Append new entries to output
        comdata.out <- data.table::rbindlist(list(comdata.out, match), 
                                             use.names = T)
        
        cat(paste('After using only species: Unknown records remaining', 
                  nrow(unknown), '\n'))
      }
    }

    #Append any remaining unknown records
    comdata.out <- data.table::rbindlist(list(comdata.out, unknown[, ID := NULL]), 
                                         use.names = T)
    
    #Revert names for subsequent runs and the output
    data.table::setnames(comdata.out, c(strata.code, 'VAR'), c(strata, unkVar[ivar]))
    
    #Update comdata set for next variable to solve
    comdata <- comdata.out
  }
  
  #Drop QY and HY
  comdata[, c('QY', 'HY') := NULL]
  
  #Add changes back into comdata
  comData[[1]] <- comdata[]
  
  return(comData[])
}
