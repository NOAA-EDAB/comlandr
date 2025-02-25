#' comlandr: Pull and process commercial fisheries data
#'
#' Suite of functions to pull and process landings and discard data in addition to
#' helper function to pull lookup tables
#'
#'\itemize{
#'\item Pulls data from StockEff
#'\item Pulls Herring data from state of Maine database
#'\item Defines fleet structure and assigns fleets to gear types
#'\item Define geographic areas and assigns statistical areas to geographic areas
#'\item Assigns landings to unknown gear, area, quarter/half year, size based on similar trips (based on M Palmer, 2008 Working paper)
#'\item Pulls and processes NAFO (Northwest Atlantic Fisheries Organization) data
#'\item Uses survey data to apportion hake complex into species
#'\item Uses survey data to apportion skate complex into species
#'\item Uses observer data to estimate discards
#'\item Adjusts species value to specified date
#'}
#'
#' @keywords internal
#'
#'@section References:
#'
#'Palmer, M (2008). **A method to apportion landings with unknown area, month and unspecified market categories among landings with similar region and fleet characteristics** Working paper
#'
#'NAFO website: \url{https://www.nafo.int}
#'
#'
#'
"_PACKAGE"
