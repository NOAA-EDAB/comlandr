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

comland_winter_little <- function(comland,skate.hake.us) {

        #get little skates and winter skates from skates(ns) - use survey in half years
        #Generate Half year variable in comland
        comland.skates <- comland[NESPP3 == 365, ]
        comland.skates[MONTH %in% 1:6,  Half := 1]
        comland.skates[MONTH %in% 7:12, Half := 2]

        data.table::setkey(skate.hake.us,
                           YEAR,
                           Half,
                           AREA)
        #beet
        comland.skates$AREA <- as.integer(comland.skates$AREA)


        comland.skates <- merge(comland.skates, skate.hake.us, by = data.table::key(skate.hake.us), all.x = T)


        #return(comland.skates)

        comland.skates[, little       := little.per * SPPLIVMT]
        comland.skates[, little.value := round(little.per * SPPVALUE)]
        comland.skates[is.na(little),       little       := 0]
        comland.skates[is.na(little.value), little.value := 0]

        comland.skates[, winter       := winter.per * SPPLIVMT]
        comland.skates[, winter.value := round(winter.per * SPPVALUE)]
        comland.skates[is.na(winter),       winter       := 0]
        comland.skates[is.na(winter.value), winter.value := 0]

        comland.skates[, other.skate       := SPPLIVMT - (little       + winter)]
        comland.skates[, other.skate.value := SPPVALUE - (little.value + winter.value)]

        #Little (366), winter (367), skates(ns) (365)
        #put skates in comland format to merge back
        little <- comland.skates[, list(YEAR, Half, AREA, MONTH, NEGEAR,
                                        TONCL1, NESPP3, UTILCD, MKTCAT, little,
                                        little.value)]
        little[, NESPP3 := 366]
        data.table::setnames(little, c('little', 'little.value'), c('SPPLIVMT', 'SPPVALUE'))
        little <- little[SPPLIVMT > 0, ]

        winter <- comland.skates[, list(YEAR, Half, AREA, MONTH, NEGEAR,
                                        TONCL1, NESPP3, UTILCD, MKTCAT, winter,
                                        winter.value)]
        winter[, NESPP3 := 367]
        data.table::setnames(winter, c('winter', 'winter.value'), c('SPPLIVMT', 'SPPVALUE'))
        winter <- winter[SPPLIVMT > 0, ]

        other <- comland.skates[, list(YEAR, Half, AREA, MONTH, NEGEAR,
                                       TONCL1, NESPP3, UTILCD, MKTCAT, other.skate,
                                       other.skate.value)]
        other[, NESPP3 := 365]
        data.table::setnames(other, c('other.skate', 'other.skate.value'), c('SPPLIVMT', 'SPPVALUE'))
        other <- other[SPPLIVMT > 0, ]

        #merge all three and reformat for comland
        skates.add.back <- data.table::rbindlist(list(little, winter, other))

        skates.add.back[, Half := NULL]
        data.table::setcolorder(skates.add.back, names(comland))

        comland <- data.table::rbindlist(list(comland[NESPP3 != 365, ], skates.add.back))
return(comland)

}
