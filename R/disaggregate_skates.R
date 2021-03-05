#'#Comcatch_skates_hakes.r
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

disaggregate_skates <- function(comland, channel, filterByYear) {
        
        message("Disaggregating little and winter skates from skates(ns) ... ")
        
        #Grab skate data from NEFSC bottom trawl survey
        skates <- 22:28
        survey <- survdat::get_survdat_data(channel, filterByYear, getLengths = F)
        skate.survey <- survey$survdat[SVSPP %in% skates, ]
        
        #Identify Stat areas catch occured in
        Stat.areas <- sf::st_read(dsn=system.file("extdata","Statistical_Areas_2010.shp",
                                                  package="comlandr"), quiet = T)
        skate.survey <- survdat::post_strat(skate.survey, Stat.areas, 'Id')
        data.table::setnames(skate.survey, 'Id', 'AREA')
        
        #Figure out proportion of skates
        data.table::setkey(skate.survey, YEAR, SEASON, AREA)
        
        skates.prop <- skate.survey[, .(skates.all = sum(BIOMASS)), 
                                    by = key(skate.survey)]
        
        little <- skate.survey[SVSPP == 26, .(little = sum(BIOMASS)), 
                               by = key(skate.survey)]
        
        skates.prop <- merge(skates.prop, little, by = key(skate.survey), all = T)
        
        winter <- skate.survey[SVSPP == 23, .(winter = sum(BIOMASS)), 
                               by = key(skate.survey)]
        
        skates.prop <- merge(skates.prop, winter, by = key(skate.survey), all = T)
        
        skates.prop[, little.per := little/skates.all]
        skates.prop[, winter.per := winter/skates.all]
        
        #Drop extra columns and fix NAs
        skates.prop[, c('skates.all', 'little', 'winter') := NULL]
        skates.prop[is.na(little.per), little.per := 0]
        skates.prop[is.na(winter.per), winter.per := 0]
        
        #disaggregate little and winter skates from skates(ns) - use survey in half years
        #Generate season variable in comland
        comland.skates <- comland$comland[NESPP3 == 365, ]
        comland.skates[MONTH %in% 1:6,  SEASON := 'SPRING']
        comland.skates[MONTH %in% 7:12, SEASON := 'FALL']

        comland.skates <- merge(comland.skates, skates.prop, 
                                by = c('YEAR', 'SEASON', 'AREA'), all.x = T)
        
        #Fix NAs
        comland.skates[is.na(little.per), little.per := 0]
        comland.skates[is.na(winter.per), winter.per := 0]

        #Disaggregate
        comland.skates[, little       := little.per * SPPLIVMT]
        comland.skates[, little.value := round(little.per * SPPVALUE)]

        comland.skates[, winter       := winter.per * SPPLIVMT]
        comland.skates[, winter.value := round(winter.per * SPPVALUE)]

        comland.skates[, other.skate       := SPPLIVMT - (little       + winter)]
        comland.skates[, other.skate.value := SPPVALUE - (little.value + winter.value)]

        #Little (366), winter (367), skates(ns) (365)
        #put skates in comland format to merge back
        little <- comland.skates[, list(YEAR, AREA, MONTH, NEGEAR,
                                        TONCL1, NESPP3, UTILCD, MKTCAT, little,
                                        little.value)]
        little[, NESPP3 := 366]
        data.table::setnames(little, c('little', 'little.value'), c('SPPLIVMT', 'SPPVALUE'))
        little <- little[SPPLIVMT > 0, ]

        winter <- comland.skates[, list(YEAR, AREA, MONTH, NEGEAR,
                                        TONCL1, NESPP3, UTILCD, MKTCAT, winter,
                                        winter.value)]
        winter[, NESPP3 := 367]
        data.table::setnames(winter, c('winter', 'winter.value'), c('SPPLIVMT', 'SPPVALUE'))
        winter <- winter[SPPLIVMT > 0, ]

        other <- comland.skates[, list(YEAR, AREA, MONTH, NEGEAR,
                                       TONCL1, NESPP3, UTILCD, MKTCAT, other.skate,
                                       other.skate.value)]
        other[, NESPP3 := 365]
        data.table::setnames(other, c('other.skate', 'other.skate.value'), c('SPPLIVMT', 'SPPVALUE'))
        other <- other[SPPLIVMT > 0, ]

        #merge all three and reformat for comland
        skates.add.back <- data.table::rbindlist(list(little, winter, other))

        data.table::setcolorder(skates.add.back, names(comland$comland))

        comland$comland <- data.table::rbindlist(list(comland$comland[NESPP3 != 365, ],
                                                      skates.add.back))

        return(comland)

}
