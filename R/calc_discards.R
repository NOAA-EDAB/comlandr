#' Calculates total discard by species
#'
#' Applies discard ratio to landings to obtain total discards by species
#'
#' @inheritParams get_comland_data
#' @param comland Data frame. Internally calculated by \code{get_comdisc_data}
#' @param dk dk. Need to find out
#'
#'
#' @return dk
#'
#'@family comdisc
#'
#'
#' Internal function
#' @noRd

calc_discards <- function(comland, dk, areaDescription, fleetDescription) {
  #Grab landings data
  comdata <- data.table::copy(comland[[1]])

  #calculate total landings
  totland <- comdata[,
    .(SPPLIVMT = sum(SPPLIVMT, na.rm = T)),
    by = c('YEAR', areaDescription, fleetDescription)
  ]

  #Merge DK
  comdisc <- merge(
    totland,
    dk,
    by = c('YEAR', areaDescription, fleetDescription)
  )

  #expand dk ratio
  comdisc[, DISMT := SPPLIVMT * DK]

  #Drop extra columns
  comdisc[, c('SPPLIVMT', 'DK') := NULL]

  return(comdisc[])
}
