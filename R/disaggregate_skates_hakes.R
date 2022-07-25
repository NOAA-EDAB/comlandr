#'#Disaggregate skates and hakes
#'
#'Determine proportion of little/winter skates and silver hake in landings data 7/13
#'SML
#'
#'@param comland Data frame. Master data frame containing species landings
#'@param skate.hake.us Data frame. Landings of skates and hakes in USA
#'
#'@return updated comland
#'
#'@importFrom data.table ":=" "key"
#'
#' @noRd
#' @export

disaggregate_skates_hakes <- function(comland, channel, filterByYear, filterByArea) {
        
        message("Grabbing survey data to disaggregate skates and hakes ... ")
        
        #Grab survey data from NEFSC bottom trawl survey
        survey <- survdat::get_survdat_data(channel, filterByYear, getLengths = F)
        
        #Skates----
        message("Disaggregating little and winter skates from skates(ns) ... ")
        skates <- 22:28
        skate.survey <- survey$survdat[SVSPP %in% skates, ]
        
        #Identify Stat areas/Divisions catch occurred in
        Stat.areas <- sf::st_read(dsn=system.file("extdata","Statistical_Areas_2010.shp",
                                                  package="comlandr"), quiet = T)
                        
        skate.survey <- survdat::post_strat(skate.survey, Stat.areas, 'Id')
        data.table::setnames(skate.survey, 'Id', 'AREA')
        
        # Add NAFO divisions for foreign landings
        NAFOAreas <- data.table::as.data.table(comlandr::get_areas(channel)$data)
        NAFOAreas <- unique(NAFOAreas[, c('AREA', 'NAFDVCD')])
        NAFOAreas <- NAFOAreas[, .(AREA = as.integer(AREA), 
                                   NAFDVCD = as.integer(NAFDVCD))]
        
        skate.survey <- merge(skate.survey, NAFOAreas, by = 'AREA', all.x = T)
        
        #Filter By Area
        if(all(!is.na(filterByArea))){
                skate.survey <- skate.survey[AREA %in% filterByArea | 
                                                     NAFDVCD %in% filterByArea, ]
        }
        
        #Figure out proportion of skates by stat area and division
        byStat <- c('YEAR', 'SEASON', 'AREA')
        byDiv  <- c('YEAR', 'SEASON', 'NAFDVCD')
        
        #All Skate biomass
        skates.prop <- skate.survey[, .(skates.all = sum(BIOMASS)), 
                                    by = byStat]
        skates.div <- skate.survey[, .(skates.all = sum(BIOMASS)),
                                   by = byDiv]
        #Little Skates
        little <- skate.survey[SVSPP == 26, .(little = sum(BIOMASS)), 
                               by = byStat]
        little.div <- skate.survey[SVSPP == 26, .(little = sum(BIOMASS)), 
                                   by = byDiv]
        #Winter Skates
        winter <- skate.survey[SVSPP == 23, .(winter = sum(BIOMASS)), 
                               by = byStat]
        winter.div <- skate.survey[SVSPP == 23, .(winter = sum(BIOMASS)),
                                   by = byDiv]
        
        #Merge data sets
        skates.prop <- merge(skates.prop, little, by = byStat, all = T)
        skates.prop <- merge(skates.prop, winter, by = byStat, all = T)
        skates.div <- merge(skates.div, little.div, by = byDiv, all = T)
        skates.div <- merge(skates.div, winter.div, by = byDiv, all = T)
        
        #Calc proportion
        skates.prop[, little.per := little/skates.all]
        skates.prop[, winter.per := winter/skates.all]
        skates.div[, little.per.div := little/skates.all]
        skates.div[, winter.per.div := winter/skates.all]
        
        #Drop extra columns and fix NAs
        skates.prop[, c('skates.all', 'little', 'winter') := NULL]
        skates.prop[is.na(little.per), little.per := 0]
        skates.prop[is.na(winter.per), winter.per := 0]
        skates.div[, c('skates.all', 'little', 'winter') := NULL]
        skates.div[is.na(little.per), little.per := 0]
        skates.div[is.na(winter.per), winter.per := 0]
        
        #disaggregate little and winter skates from skates(ns) - use survey in half years
        #Generate season variable in comland
        comland.skates <- comland$comland[NESPP3 == 365, ]
        comland.skates[MONTH %in% 1:6,  SEASON := 'SPRING']
        comland.skates[MONTH %in% 7:12, SEASON := 'FALL']

        comland.skates <- merge(comland.skates, skates.prop, 
                                by = byStat, all.x = T)
        data.table::setnames(skates.div, 'NAFDVCD', 'AREA')
        comland.skates <- merge(comland.skates, skates.div, 
                                by = byStat, all.x = T)
        #Fix NAs
        comland.skates[is.na(little.per), little.per := 0]
        comland.skates[is.na(winter.per), winter.per := 0]
        comland.skates[is.na(little.per.div), little.per.div := 0]
        comland.skates[is.na(winter.per.div), winter.per.div := 0]
        
        #Disaggregate - US landings by Area and Foreign by Division
        #Little Skates
        comland.skates[US == T, little       := little.per * SPPLIVMT]
        comland.skates[US == T, little.value := round(little.per * SPPVALUE)]
        comland.skates[US == F, little       := little.per.div * SPPLIVMT]
        comland.skates[US == F, little.value := round(little.per.div * SPPVALUE)]
        #Winter Skates
        comland.skates[US == T, winter       := winter.per * SPPLIVMT]
        comland.skates[US == T, winter.value := round(winter.per * SPPVALUE)]
        comland.skates[US == F, winter       := winter.per.div * SPPLIVMT]
        comland.skates[US == F, winter.value := round(winter.per.div * SPPVALUE)]
        #Other Skates
        comland.skates[, other.skate       := SPPLIVMT - (little       + winter)]
        comland.skates[, other.skate.value := SPPVALUE - (little.value + winter.value)]

        #Little (366), winter (367), skates(ns) (365)
        #put skates in comland format to merge back
        #Little
        little <- comland.skates[, list(YEAR, AREA, MONTH, NEGEAR,
                                        TONCL1, NESPP3, UTILCD, MESHCAT, MKTCAT, 
                                        little, little.value, US)]
        little[, NESPP3 := 366]
        data.table::setnames(little, c('little', 'little.value'), c('SPPLIVMT', 'SPPVALUE'))
        little <- little[SPPLIVMT > 0, ]
        #Winter
        winter <- comland.skates[, list(YEAR, AREA, MONTH, NEGEAR,
                                        TONCL1, NESPP3, UTILCD, MESHCAT, MKTCAT, 
                                        winter, winter.value, US)]
        winter[, NESPP3 := 367]
        data.table::setnames(winter, c('winter', 'winter.value'), c('SPPLIVMT', 'SPPVALUE'))
        winter <- winter[SPPLIVMT > 0, ]
        #Other skates
        other <- comland.skates[, list(YEAR, AREA, MONTH, NEGEAR,
                                       TONCL1, NESPP3, UTILCD, MESHCAT, MKTCAT, 
                                       other.skate, other.skate.value, US)]
        other[, NESPP3 := 365]
        data.table::setnames(other, c('other.skate', 'other.skate.value'), c('SPPLIVMT', 'SPPVALUE'))
        other <- other[SPPLIVMT > 0, ]

        #merge all three and reformat for comland
        skates.add.back <- data.table::rbindlist(list(little, winter, other))

        data.table::setcolorder(skates.add.back, names(comland$comland))

        comland$comland <- data.table::rbindlist(list(comland$comland[NESPP3 != 365, ],
                                                      skates.add.back))

        #Hakes ----
        message("Disaggregating silver and offshore hake from whiting ... ")
        
        #Grab hake data from NEFSC bottom trawl survey
        hake <- c(69, 72)
        hake.survey <- survey$survdat[SVSPP %in% hake, ]
        
        #Identify Stat areas catch occured in
        hake.survey <- survdat::post_strat(hake.survey, Stat.areas, 'Id')
        data.table::setnames(hake.survey, 'Id', 'AREA')
        
        #Filter By Area
        if(!is.na(filterByArea[1])) hake.survey <- hake.survey[AREA %in% filterByArea, ]
        
        #Figure out proportion of skates
        data.table::setkey(hake.survey, YEAR, SEASON, AREA)
        
        hake.prop <- hake.survey[, .(hake.all = sum(BIOMASS, na.rm = T)), 
                                 by = key(hake.survey)]
        
        silvers <- hake.survey[SVSPP == 72, .(silver = sum(BIOMASS, na.rm = T)), 
                               by = key(hake.survey)]
        
        hake.prop <- merge(hake.prop, silvers, all = T)
        hake.prop[is.na(silver), silver := 0]
        
        hake.prop[, silver.per := silver / hake.all]
        
        hake.prop[, offshore.per := 1 - silver.per]
        hake.prop[, c('hake.all', 'silver') := NULL]
        
        #disaggregate silver and offshore hake from whiting - use survey in half years
        #Generate season variable in comland
        comland.hakes <- comland$comland[NESPP3 == 507, ]
        comland.hakes[MONTH %in% 1:6,  SEASON := 'SPRING']
        comland.hakes[MONTH %in% 7:12, SEASON := 'FALL']
        
        comland.hakes <- merge(comland.hakes, hake.prop, 
                               by = c('YEAR', 'SEASON', 'AREA'), all.x = T)
        
        #Fix NAs
        comland.hakes[is.na(silver.per), silver.per := 0]
        comland.hakes[is.na(offshore.per), offshore.per := 0]
        
        #Disaggregate
        comland.hakes[, silver       := silver.per * SPPLIVMT]
        comland.hakes[, silver.value := round(silver.per * SPPVALUE)]
        
        comland.hakes[, offshore       := offshore.per * SPPLIVMT]
        comland.hakes[, offshore.value := round(offshore.per * SPPVALUE)]
        
        comland.hakes[, other.hakes       := SPPLIVMT - (silver       + offshore)]
        comland.hakes[, other.hakes.value := SPPVALUE - (silver.value + offshore.value)]
        
        #Silver (509), offshore (508), whiting (507)
        #put hakes in comland format to merge back
        silver <- comland.hakes[, list(YEAR, AREA, MONTH, NEGEAR,
                                       TONCL1, NESPP3, UTILCD, MESHCAT, MKTCAT, 
                                       silver, silver.value)]
        silver[, NESPP3 := 509]
        data.table::setnames(silver, c('silver', 'silver.value'), c('SPPLIVMT', 'SPPVALUE'))
        silver <- silver[SPPLIVMT > 0, ]
        
        offshore <- comland.hakes[, list(YEAR, AREA, MONTH, NEGEAR,
                                         TONCL1, NESPP3, UTILCD, MESHCAT, MKTCAT, 
                                         offshore, offshore.value)]
        offshore[, NESPP3 := 508]
        data.table::setnames(offshore, c('offshore', 'offshore.value'), c('SPPLIVMT', 'SPPVALUE'))
        offshore <- offshore[SPPLIVMT > 0, ]
        
        other <- comland.hakes[, list(YEAR, AREA, MONTH, NEGEAR,
                                      TONCL1, NESPP3, UTILCD, MESHCAT, MKTCAT, 
                                      other.hakes, other.hakes.value)]
        other[, NESPP3 := 507]
        data.table::setnames(other, c('other.hakes', 'other.hakes.value'), c('SPPLIVMT', 'SPPVALUE'))
        other <- other[SPPLIVMT > 0, ]
        
        #merge all three and reformat for comland
        hakes.add.back <- data.table::rbindlist(list(silver, offshore, other))
        
        data.table::setcolorder(hakes.add.back, names(comland$comland))
        
        comland$comland <- data.table::rbindlist(list(comland$comland[NESPP3 != 507, ],
                                                      hakes.add.back))
        
        return(comland)

}
