#' Calculate discard to kept ratio
#'
#' Use observer data to calculate the ratio of discards to kept by species.
#'
#' @inheritParams get_comland_data
#' @param comData data frame. calculated from inside \code{get_comdisc_data}
#'
#' @return Returns a \code{comdiscData} data.table with one additional column labeled
#'  with the value of \code{areaDescription}
#'
#' \item{areaDescription}{The name of the region (found in \code{areaPolygon})
#'  that a record in \code{surveyData} is assigned to}
#'
#' @importFrom magrittr "%>%"
#'
#' @family comdisc
#'
#' Internal function
#' @noRd



calc_DK <- function(comData, areaDescription, fleetDescription){

  comdata <- data.table::copy(comData[[1]])

  #Standardize column names
  comdata <- data.table::setnames(comdata, c(areaDescription, fleetDescription),
                                      c('area', 'fleet'))

  #sum catch by species/disposition/area/fleet
  ob.sums <- comdata[, sum(SPPLIVMT), by = c('YEAR', 'area', 'fleet', 'NESPP3',
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
