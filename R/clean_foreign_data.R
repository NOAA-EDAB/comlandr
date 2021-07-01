#' Downloads all NAFO data
#'
#'Downloads and reads in all NAFO data then aggregates it
#'
#'@param channel
#'@param nafoland Data frame. output from \code{\url{get_foreign_data}}
#'
#'@return Data frame: NAFO data
#'
#'
#'@importFrom data.table ":="
#'@importFrom magrittr "%>%"
#'
#'@export

clean_foreign_data <- function(channel,nafoland){

  ## pull NESPP3 codes and add to nafo landings
  speciesInfo <- data.table::as.data.table(get_species(channel)$data )
  speciesInfo <- speciesInfo[,list(NESPP3,NAFOSPP)]
  speciesInfo[, NESPP3:= as.integer(NESPP3) ]
  speciesInfo[, NAFOSPP:= as.integer(NAFOSPP)]
  speciesInfo <- speciesInfo %>% dplyr::distinct()

  ###########################################################################
  # caution many to one relationship NESPP3 -> NAFO. Duplicated catch data
  # Fix this. eg. Cod NESPP3 = 81,82
  #############################################################################
  nafoland <- dplyr::left_join(nafoland, speciesInfo, by = c("Code"="NAFOSPP"))

  # What to do with catches of species in unknown months?
  # What to do with catches in UNKNOWN areas


  return(nafoland)
}
