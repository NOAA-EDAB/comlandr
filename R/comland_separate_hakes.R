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
#'
#'@export

comland_separate_hakes <- function(comland,skate.hake.us) {
        #get silver hake from mixed hakes - use survey in half years
        #Generate Half year variable in comland
        comland.hakes <- comland[NESPP3 == 507, ]
        comland.hakes[MONTH %in% 1:6,  Half := 1]
        comland.hakes[MONTH %in% 7:12, Half := 2]

        comland.hakes <- merge(comland.hakes, skate.hake.us, by = key(skate.hake.us), all.x = T)

        comland.hakes[, silver       := silver.per * SPPLIVMT]
        comland.hakes[, silver.value := round(silver.per * SPPVALUE)]
        comland.hakes[is.na(silver),       silver       := 0]
        comland.hakes[is.na(silver.value), silver.value := 0]

        comland.hakes[, off.hake       := SPPLIVMT - silver]
        comland.hakes[, off.hake.value := SPPVALUE - silver.value]

        #Silver hake (509), mix hakes (507)
        #put hakes in comland format to merge back
        silver <- comland.hakes[, list(YEAR, Half, AREA, MONTH, NEGEAR,
                                       TONCL1, NESPP3, UTILCD, MKTCAT, silver,
                                       silver.value)]
        silver[, NESPP3 := 509]
        data.table::setnames(silver, c('silver', 'silver.value'), c('SPPLIVMT', 'SPPVALUE'))
        silver <- silver[SPPLIVMT > 0, ]

        offshore <- comland.hakes[, list(YEAR, Half, AREA, MONTH, NEGEAR,
                                         TONCL1, NESPP3, UTILCD, MKTCAT, off.hake,
                                         off.hake.value)]
        offshore[, NESPP3 := 507]
        data.table::setnames(offshore, c('off.hake', 'off.hake.value'), c('SPPLIVMT', 'SPPVALUE'))
        offshore <- offshore[SPPLIVMT > 0, ]

        #merge both and reformat for comland
        hakes.add.back <- data.table::rbindlist(list(silver, offshore))

        hakes.add.back[, Half := NULL]
        data.table::setcolorder(hakes.add.back, names(comland))

        comland <- data.table::rbindlist(list(comland[NESPP3 != 507, ], hakes.add.back))

        return(comland)
}
