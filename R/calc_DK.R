#' Calculate discard to kept ratio
#'
#' Use observer data to calculate the ratio of discards to kept by species.
#' 
#'
#' @inheritParams strat_prep
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
#'@family comdisc
#'
#' @export


calc_DK <- function(comdiscData, areaDescription, fleetDescription){
  
  #Standardize column names
  comdiscData <- data.table::setnames(comdiscData, c(areaDescription, 
                                                     fleetDescription), 
                                      c('area', 'fleet'))
  
  #sum catch by species/disposition/area/fleet
  ob.sums <- comdiscData[, sum(SPPLIVMT), by = c('YEAR', 'area', 'fleet', 'NESPP3',
                                                 'CATDISP')]
  #identify discards
  ob.discard <- ob.sums[CATDISP == 0, ]
  
  setnames(ob.discard, "V1", "DISCARD")
  ob.discard[, CATDISP := NULL]
  
  #Sum kept by area/fleet
  ob.kept <- ob.sums[CATDISP == 1, sum(V1), by = c('YEAR', 'area', 'fleet')]
  
  setnames(ob.kept, "V1", "KEPT.ALL")
  
  #Merge discards and kept
  dk <- merge(ob.kept, ob.discard, by = c('YEAR', 'area', 'fleet'))
  
  #Calculate ratio
  dk[, DK := DISCARD / KEPT.ALL]
  #NAs result if divide by 0 so set DK to 1 (all discards)
  dk[is.na(DK), DK := 1.0]
  
  #Remove extra columns
  dk[, c('KEPT.ALL', 'DISCARD') := NULL]
  
  #Replace standard column names
  #Standardize column names
  data.table::setnames(dk, c('area', 'fleet'), 
                       c(areaDescription, fleetDescription))
  
  return(dk[])
}
  