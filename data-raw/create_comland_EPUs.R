## Creates lazy data for EPUs
library(magrittr)
create_comland_EPUs <- function(){

  gom <- list()
  gb <- list()
  ss <- list()
  mab <- list()

  # define statistical areas
  gom$statAreas <-c(500, 510, 512:515)
  gb$statAreas <-c(521:526, 551, 552, 561, 562)
  mab$statAreas <-c(537, 539, 600, 612:616, 621, 622, 625, 626, 631, 632)
  ss$statAreas <-c(463:467, 511)

  # define areas
  statAreaShapefile <- sf::st_read(system.file("extdata","Statistical_Areas_2010.shp",package = "comlandr"))
  units

  gom$area <- sum(sf::st_area(statAreaShapefile %>% dplyr::filter(Id %in% gom$statAreas)))
  units(gom$area) <- "km^2"

  gb$area <- sum(sf::st_area(statAreaShapefile %>% dplyr::filter(Id %in% gb$statAreas)))
  units(gb$area) <- "km^2"

  mab$area <- sum(sf::st_area(statAreaShapefile %>% dplyr::filter(Id %in% mab$statAreas)))
  units(mab$area) <- "km^2"

  ss$area <- sum(sf::st_area(statAreaShapefile %>% dplyr::filter(Id %in% ss$statAreas)))
  units(ss$area) <- "km^2"


  EPUs <- list(GOM=gom, GB=gb,MAB=mab,SS=ss)

  usethis::use_data(EPUs,overwrite = T)

}
