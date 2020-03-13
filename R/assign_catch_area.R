#' assign unknown catch using known catch characteristics
#'
#' Uses area. Expand .....
#'
#'
#'@return updated comland
#'
#'@importFrom data.table ":=" "key" "setcolorder" "as.data.table"
#'

assign_catch_area <- function(comland.agg){


  unk.area <- comland.agg[AREA == 0, ]
  k.area   <- comland.agg[AREA != 0, ]

  #3.C.1 - All match
  match.key <- c('YEAR', 'NESPP3', 'QY', 'HY', 'SIZE', 'GEAR')

  unk.area.all <- unk.area

  k.area.all <- k.area

  data.table::setkeyv(unk.area.all, match.key)
  data.table::setkeyv(k.area.all,   match.key)

  area.all <- k.area.all[unk.area.all]

  #No match - need to match with larger aggregation
  no.match  <- area.all[is.na(SPPLIVMT), ]
  no.match[, c('AREA', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match, c('i.AREA', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('AREA', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  #Drop QY
  data.table::setkey(no.match, YEAR, NESPP3, HY, SIZE, GEAR)
  data.table::setkeyv(k.area.all, key(no.match))
  area.all.2 <- k.area.all[no.match]
  no.match.2 <- area.all.2[is.na(SPPLIVMT), ]
  no.match.2[, c('AREA', 'QY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match.2, c('i.AREA', 'i.QY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('AREA', 'QY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  #Drop HY
  data.table::setkey(no.match.2, YEAR, NESPP3, SIZE, GEAR)
  data.table::setkeyv(k.area.all, key(no.match.2))
  area.all.3 <- k.area.all[no.match.2]
  no.match.3 <- area.all.3[is.na(SPPLIVMT), ]
  no.match.3[, c('AREA', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match.3, c('i.AREA', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('AREA', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  #Drop SIZE
  data.table::setkey(no.match.3, YEAR, NESPP3, GEAR)
  data.table::setkeyv(k.area.all, key(no.match.3))
  area.all.4 <- k.area.all[no.match.3]
  no.match.4 <- area.all.4[is.na(SPPLIVMT), ]
  no.match.4[, c('AREA', 'SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match.4, c('i.AREA', 'i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('AREA',   'SIZE',   'QY',   'HY',  'UTILCD',  'SPPLIVMT', 'SPPVALUE'))
  #Drop GEAR
  data.table::setkey(no.match.4, YEAR, NESPP3)
  data.table::setkeyv(k.area.all, key(no.match.4))
  area.all.5 <- k.area.all[no.match.4]
  no.match.5 <- area.all.5[is.na(SPPLIVMT), ]
  no.match.5[, c('AREA', 'GEAR', 'SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match.5, c('i.AREA', 'i.GEAR', 'i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD',
                                     'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('AREA', 'GEAR', 'SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  #Still no match - use 3 or 5 year window then drop year
  years <- unique(no.match.5[, YEAR], by = key(no.match.5))
  no.match.6 <- c()
  area.all.6 <- c()
  for(i in 1:length(years)){
    #3 year window
    k.area.3y <- comland.agg[AREA != 0 & YEAR %in% (years[i] - 1):(years[i] + 1), ]
    data.table::setkey(k.area.3y, NESPP3, AREA)
    k.area.3y <- k.area.3y[, list(sum(SPPLIVMT), sum(SPPVALUE)),
                           by = c(key(k.area.3y), 'UTILCD')]
    data.table::setnames(k.area.3y, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

    unk.area.3y <- no.match.5[YEAR == years[i], ]

    data.table::setkey(unk.area.3y, NESPP3)
    data.table::setkey(k.area.3y,   NESPP3)
    area.3y <- k.area.3y[unk.area.3y]

    no.match.3y <- area.3y[is.na(SPPLIVMT), ]
    no.match.3y[, c('AREA', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
    data.table::setnames(no.match.3y, c('i.AREA', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                         c('AREA', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
    no.match.6 <- data.table::rbindlist(list(no.match.6, no.match.3y))
    area.all.6 <- data.table::rbindlist(list(area.all.6, area.3y))
  }

  years <- unique(no.match.6[, YEAR], by = key(no.match.6))
  no.match.7 <- c()
  area.all.7 <- c()
  for(i in 1:length(years)){
    #5 year window
    k.area.5y <- comland.agg[AREA != 0 & YEAR %in% (years[i] - 2):(years[i] + 2), ]
    data.table::setkey(k.area.5y, NESPP3, AREA)
    k.area.5y <- k.area.5y[, list(sum(SPPLIVMT), sum(SPPVALUE)),
                           by = c(key(k.area.5y), 'UTILCD')]
    data.table::setnames(k.area.5y, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

    unk.area.5y <- no.match.6[YEAR == years[i], ]

    data.table::setkey(unk.area.5y, NESPP3)
    data.table::setkey(k.area.5y,   NESPP3)
    area.5y <- k.area.5y[unk.area.5y]

    no.match.5y <- area.5y[is.na(SPPLIVMT), ]
    no.match.5y[, c('AREA', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
    data.table::setnames(no.match.5y, c('i.AREA', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                         c('AREA', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))

    no.match.7 <- data.table::rbindlist(list(no.match.7, no.match.5y))
    area.all.7 <- data.table::rbindlist(list(area.all.7, area.5y))
  }
  #Drop year
  data.table::setkey(no.match.7, NESPP3)
  k.area.all <- k.area.all[, list(sum(SPPLIVMT), sum(SPPVALUE)),
                           by = c('NESPP3', 'AREA', 'UTILCD')]
  data.table::setnames(k.area.all, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))
  data.table::setkey(k.area.all, NESPP3)

  area.all.8 <- k.area.all[no.match.7]
  no.match.8 <- area.all.8[is.na(SPPLIVMT), ]
  no.match.8[, c('AREA', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match.8, c('i.AREA', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('AREA', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  #If still no match - leave as unknown


  #Merge all together and proportion catch to known areas
  area.all   <- area.all  [!is.na(SPPLIVMT), ]
  area.all.2 <- area.all.2[!is.na(SPPLIVMT), ]
  area.all.2[, QY   := i.QY]
  area.all.2[, i.QY := NULL]
  setcolorder(area.all.2, names(area.all))
  area.all.3 <- area.all.3[!is.na(SPPLIVMT), ]
  area.all.3[, QY   := i.QY]
  area.all.3[, HY   := i.HY]
  area.all.3[, i.QY := NULL]
  area.all.3[, i.HY := NULL]
  setcolorder(area.all.3, names(area.all))
  area.all.4 <- area.all.4[!is.na(SPPLIVMT), ]
  area.all.4[, QY     := i.QY]
  area.all.4[, HY     := i.HY]
  area.all.4[, SIZE   := i.SIZE]
  area.all.4[, i.QY   := NULL]
  area.all.4[, i.HY   := NULL]
  area.all.4[, i.SIZE := NULL]
  setcolorder(area.all.4, names(area.all))
  area.all.5 <- area.all.5[!is.na(SPPLIVMT), ]
  area.all.5[, QY     := i.QY]
  area.all.5[, HY     := i.HY]
  area.all.5[, SIZE   := i.SIZE]
  area.all.5[, GEAR   := i.GEAR]
  area.all.5[, i.QY   := NULL]
  area.all.5[, i.HY   := NULL]
  area.all.5[, i.SIZE := NULL]
  area.all.5[, i.GEAR := NULL]
  setcolorder(area.all.5, names(area.all))
  area.all.6 <- area.all.6[!is.na(SPPLIVMT), ]
  setcolorder(area.all.6, names(area.all))
  area.all.7 <- area.all.7[!is.na(SPPLIVMT), ]
  setcolorder(area.all.7, names(area.all))
  area.all.8 <- area.all.8[!is.na(SPPLIVMT), ]
  setcolorder(area.all.8, names(area.all))

  area.all <- data.table::rbindlist(list(area.all,   area.all.2, area.all.3, area.all.4,
                                         area.all.5, area.all.6, area.all.7, area.all.8))

  area.all[, prop := SPPLIVMT / sum(SPPLIVMT), by = match.key]
  area.all[, unk  := i.SPPLIVMT * prop]
  area.all[, unk2 := i.SPPVALUE * prop]
  area.all[, c('SPPLIVMT', 'SPPVALUE', 'i.SPPLIVMT', 'i.SPPVALUE', 'i.AREA',
               'i.UTILCD', 'prop') := NULL]
  data.table::setnames(area.all, c('unk','unk2'), c('SPPLIVMT', 'SPPVALUE'))

  setcolorder(no.match.8, names(area.all))
  area.solved <- data.table::rbindlist(list(area.all, no.match.8))
  rm(list = c(ls(pattern = 'area.all'), ls(pattern = 'no.match')))

  #Merge back area.solved
  setcolorder(area.solved, names(comland.agg))
  comland.agg <- data.table::rbindlist(list(k.area, area.solved))
  data.table::setkey(comland.agg,
                     YEAR,
                     QY,
                     HY,
                     SIZE,
                     GEAR,
                     AREA,
                     NESPP3,
                     UTILCD)
  comland.agg <- comland.agg[, list(sum(SPPLIVMT), sum(SPPVALUE)),
                             by = key(comland.agg)]
  data.table::setnames(comland.agg, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))


  return(comland.agg)
}
