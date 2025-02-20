#' comlandr: Pull and process commercial fisheries data
#'
#' Suite of functions to pull and process landings and discard data in addition to
#' helper function to pull lookup tables
#'
#'\itemize{
#'\item Pulls data from StockEff: by area, gear, species, year, month
#'\item pulls Herring data from state of Maine database
#'\item Defines fleet structure and assigns fleets to gear types
#'\item Define geographic areas and assigns statistical areas to geographic areas
#'\item Assigns landings to unknown gear, area, quarter/half year, size based on similar trips (based on M Palmer, ????)
#'\item Pulls and processes NAFO (Northwest Atlantic Fisheries Organization) data
#'\item Uses survey data to apportion hake complex into species
#'\item Uses survey data to apportion skate complex into species
#'\item Ajusts species value to specified date
#'}
#'
#' @keywords internal
#'
#'@section References:
#'
#'Palmer, M (????) Working doc reference and title
#'
#'NAFO website: \url{https://www.nafo.int}
#'
#'To learn more about using \code{comlandr}, visit \url{https://noaa-edab.github.io/comlandr/articles/comlandr.html} or click the index below
#'
#'
"_PACKAGE"
