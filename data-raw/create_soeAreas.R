#Area for all species
library(data.table); library(usethis); library(here)

load(here('data-raw', 'SOE_species_list.RData'))
nespp3 <- unique(species[, NESPP3])

GOM <- data.table::CJ(AREA = comlandr::EPUs$GOM$statAreas, NESPP3 = nespp3, 
                      MeanProp = 1, EPU = 'GOM')
GB  <- data.table::CJ(AREA = c(comlandr::EPUs$GB$statAreas, 538), NESPP3 = nespp3, 
                      MeanProp = 1, EPU = 'GB')
MAB <- data.table::CJ(AREA = c(comlandr::EPUs$MAB$statAreas, 611),NESPP3 = nespp3, 
                      MeanProp = 1, EPU = 'MAB')
SS  <- data.table::CJ(AREA = comlandr::EPUs$SS$statAreas, NESPP3 = nespp3, 
                      MeanProp = 1, EPU = 'SS')

soeAreas <- data.table::rbindlist(list(GOM, GB, MAB, SS))
usethis::use_data(soeAreas)
