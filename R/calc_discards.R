#' Calculate stratified mean
#'
#' Calculates the stratified mean. Details of method found here ...
#'
#' @inheritParams strat_prep
#' @inheritParams strat_mean
#' @param tidy Boolean. Return output in long format (Default = F).
#' @param returnPrepData Boolean. Return both \code{stratmeanData} and \code{prepData}
#'   as a list object. The default (F) returns only the \code{stratmeanData} as a
#'   \code{data.table}.
#'
#' @return
#'
#'@family survdat
#'
#' @examples
#' \dontrun{
#' # Pull data and apply conversion corrections
#' data <- get_survdat_data(channel)
#' # Calculate stratified mean for specific survey strata for the SPRING season
#' calc_stratified_mean(surveyData=data$survdat, filterByArea=c(1220, 1240, 1260:1290,1360:1400),filterBySeason = "SPRING")
#'
#' # Calculate stratified mean for area defined by EPU regions, for all seasons ("SPRING", "FALL")
#' # Read in EPU shapefile (loaded as part of the package)
#' area <- sf::st_read(dsn = system.file("extdata","EPU.shp",package="survdat"),quiet=T)
#' calc_stratified_mean(surveyData=data$survdat, areaPolygon=area, areaDescription="EPU", filterByArea="all",filterBySeason = "all")
#'
#' }
#'
#'
#' @export


calc_discards <- function(comland, dk, areaDescription, fleetDescription) {

  #Grab landings data
  comdata <- copy(comland[[1]])
  
  #calculate total landings
  totland <- comdata[, .(SPPLIVMT = sum(SPPLIVMT, na.rm = T)), 
                     by = c('YEAR', areaDescription, fleetDescription)]
    
  #Merge DK 
  comdisc <- merge(totland, dk, by = c('YEAR', areaDescription, fleetDescription))
  
  #expand dk ratio
  comdisc[, DISMT := SPPLIVMT * DK]
    
  #Drop extra columns
  comdisc[, c('SPPLIVMT', 'DK') := NULL]
  
  return(comdisc[])
}

