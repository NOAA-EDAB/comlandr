#' Processes Menhaden data
#'
#'Fix menhaden records - data from Tom Miller/ Andre Bouchheister
#'
#'@param comland Data frame. master data frame containing species landings
#'
#'@return Processed Menhaden data
#'
#'

comland_menhaden <- function(comland){
  ##fix menhaden records - data from Tom Miller/ Andre Bouchheister
  #menhaden <- as.data.table(read.csv(paste(data.dir, "Menhaden.csv", sep = '')))
  #menhaden.mab <- menhaden[, MA.Total + CB.Total, by = Year]
  ##file metric is 1000s of lbs - convert to mt
  #menhaden.mab[, SPPLIVMT := (V1 * 1000) *  0.00045359237]
  #menhaden.mab[, V1 := NULL]
  #
  #menhaden.gom <- menhaden[, list(Year, NE.Total)]
  #menhaden.gom[, SPPLIVMT := (NE.Total * 1000) *  0.00045359237]
  #menhaden.gom[, NE.Total := NULL]
  return(comland)
}
