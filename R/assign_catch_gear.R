#' assign unknown catch using known catch characteristics
#'
#' Uses gear. Expand .....
#'
#'@param comland Data frame. master data frame containing species landings
#'
#'@return updated comland data frame
#'
#'@importFrom data.table ":=" "key" "setcolorder" "as.data.table"
#'
#'@family assign catch

assign_catch_gear <- function(comland.agg){
  unk.gear <- comland.agg[GEAR == 'unknown', ]
  k.gear   <- comland.agg[GEAR != 'unknown', ]

  #3.C.1 - All match
  match.key <- c('YEAR', 'NESPP3', 'QY', 'HY', 'SIZE', 'AREA')

  unk.gear.all <- unk.gear[AREA != 0, ]

  k.gear.all <- k.gear[AREA != 0, ]

  data.table::setkeyv(unk.gear.all, match.key)
  data.table::setkeyv(k.gear.all,   match.key)

  gear.all <- k.gear.all[unk.gear.all]

  #No match - need to match with larger aggregation
  no.match  <- gear.all[is.na(SPPLIVMT), ]
  no.match[, c('GEAR', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match, c('i.GEAR', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('GEAR', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  #Drop QY
  data.table::setkey(no.match, YEAR, NESPP3, HY, SIZE, AREA)
  data.table::setkeyv(k.gear.all, key(no.match))
  gear.all.2 <- k.gear.all[no.match]
  no.match.2 <- gear.all.2[is.na(SPPLIVMT), ]
  no.match.2[, c('GEAR', 'QY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match.2, c('i.GEAR', 'i.QY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('GEAR', 'QY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  #Drop HY
  data.table::setkey(no.match.2, YEAR, NESPP3, SIZE, AREA)
  data.table::setkeyv(k.gear.all, key(no.match.2))
  gear.all.3 <- k.gear.all[no.match.2]
  no.match.3 <- gear.all.3[is.na(SPPLIVMT), ]
  no.match.3[, c('GEAR', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match.3, c('i.GEAR', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('GEAR', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  #Drop SIZE
  data.table::setkey(no.match.3, YEAR, NESPP3, AREA)
  data.table::setkeyv(k.gear.all, key(no.match.3))
  gear.all.4 <- k.gear.all[no.match.3]
  no.match.4 <- gear.all.4[is.na(SPPLIVMT), ]
  no.match.4[, c('GEAR', 'SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match.4, c('i.GEAR', 'i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('GEAR',   'SIZE',   'QY',   'HY',  'UTILCD',  'SPPLIVMT', 'SPPVALUE'))
  #Drop AREA
  data.table::setkey(no.match.4, YEAR, NESPP3)
  data.table::setkeyv(k.gear.all, key(no.match.4))
  gear.all.5 <- k.gear.all[no.match.4]
  no.match.5 <- gear.all.5[is.na(SPPLIVMT), ]
  no.match.5[, c('AREA', 'GEAR', 'SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match.5, c('i.AREA', 'i.GEAR', 'i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD',
                                     'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('AREA', 'GEAR', 'SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  #Still no match - assign to GEAR to other
  no.match.5[, GEAR := factor('other', levels = levels(k.gear[, GEAR]))]

  #Merge all together and proportion catch to known gears
  gear.all   <- gear.all  [!is.na(SPPLIVMT), ]
  gear.all.2 <- gear.all.2[!is.na(SPPLIVMT), ]
  gear.all.2[, QY   := i.QY]
  gear.all.2[, i.QY := NULL]
  setcolorder(gear.all.2, names(gear.all))
  gear.all.3 <- gear.all.3[!is.na(SPPLIVMT), ]
  gear.all.3[, QY   := i.QY]
  gear.all.3[, HY   := i.HY]
  gear.all.3[, i.QY := NULL]
  gear.all.3[, i.HY := NULL]
  setcolorder(gear.all.3, names(gear.all))
  gear.all.4 <- gear.all.4[!is.na(SPPLIVMT), ]
  gear.all.4[, QY     := i.QY]
  gear.all.4[, HY     := i.HY]
  gear.all.4[, SIZE   := i.SIZE]
  gear.all.4[, i.QY   := NULL]
  gear.all.4[, i.HY   := NULL]
  gear.all.4[, i.SIZE := NULL]
  setcolorder(gear.all.4, names(gear.all))
  gear.all.5 <- gear.all.5[!is.na(SPPLIVMT), ]
  gear.all.5[, QY     := i.QY]
  gear.all.5[, HY     := i.HY]
  gear.all.5[, SIZE   := i.SIZE]
  gear.all.5[, AREA   := i.AREA]
  gear.all.5[, i.QY   := NULL]
  gear.all.5[, i.HY   := NULL]
  gear.all.5[, i.SIZE := NULL]
  gear.all.5[, i.AREA := NULL]
  setcolorder(gear.all.5, names(gear.all))

  gear.all <- data.table::rbindlist(list(gear.all, gear.all.2, gear.all.3,
                                         gear.all.4, gear.all.5))

  gear.all[, prop := SPPLIVMT / sum(SPPLIVMT), by = match.key]
  gear.all[, unk  := i.SPPLIVMT * prop]
  gear.all[, unk2 := i.SPPVALUE * prop]
  gear.all[, c('SPPLIVMT', 'SPPVALUE', 'i.SPPLIVMT', 'i.SPPVALUE', 'i.GEAR',
               'i.UTILCD', 'prop') := NULL]
  data.table::setnames(gear.all, c('unk','unk2'), c('SPPLIVMT', 'SPPVALUE'))

  setcolorder(no.match.5, names(gear.all))
  gear.solved <- data.table::rbindlist(list(gear.all, no.match.5))
  rm(list = c(ls(pattern = 'gear.all'), ls(pattern = 'no.match')))

  #3.C.2 - Species only - no other match
  match.key <- c('YEAR', 'NESPP3', 'QY', 'HY', 'SIZE')

  unk.gear.sp <- unk.gear[AREA == 0, ]
  unk.gear.sp[, 'AREA' := NULL]

  k.gear.sp <- k.gear[, list(sum(SPPLIVMT), sum(SPPVALUE)),
                      by = c(match.key, 'GEAR', 'UTILCD')]
  data.table::setnames(k.gear.sp, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

  data.table::setkeyv(unk.gear.sp, match.key)
  data.table::setkeyv(k.gear.sp,   match.key)

  gear.sp <- k.gear.sp[unk.gear.sp]

  #No match - need to match with larger aggregation
  no.match  <- gear.sp[is.na(SPPLIVMT), ]
  no.match[, c('GEAR', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match, c('i.GEAR', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('GEAR', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  #Drop QY
  data.table::setkey(no.match, YEAR, NESPP3, HY, SIZE)
  data.table::setkeyv(k.gear.sp, key(no.match))
  gear.sp.2 <- k.gear.sp[no.match]
  no.match.2 <- gear.sp.2[is.na(SPPLIVMT), ]
  no.match.2[, c('GEAR', 'QY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match.2, c('i.GEAR', 'i.QY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('GEAR', 'QY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  #Drop HY
  data.table::setkey(no.match.2, YEAR, NESPP3, SIZE)
  data.table::setkeyv(k.gear.sp, key(no.match.2))
  gear.sp.3 <- k.gear.sp[no.match.2]
  no.match.3 <- gear.sp.3[is.na(SPPLIVMT), ]
  no.match.3[, c('GEAR', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match.3, c('i.GEAR', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('GEAR', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  #Drop SIZE
  data.table::setkey(no.match.3, YEAR, NESPP3)
  data.table::setkeyv(k.gear.sp, key(no.match.3))
  gear.sp.4 <- k.gear.sp[no.match.3]
  no.match.4 <- gear.sp.4[is.na(SPPLIVMT), ]
  no.match.4[, c('GEAR', 'SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match.4, c('i.GEAR', 'i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('GEAR',   'SIZE',   'QY',   'HY',  'UTILCD',  'SPPLIVMT', 'SPPVALUE'))
  #Still no match - assign to GEAR to other
  no.match.4[, GEAR := factor('other', levels = levels(k.gear[, GEAR]))]
  no.match.4[, AREA := 0]

  #Merge all together and proportion catch to known gears
  gear.sp   <- gear.sp  [!is.na(SPPLIVMT), ]
  gear.sp.2 <- gear.sp.2[!is.na(SPPLIVMT), ]
  gear.sp.2[, QY   := i.QY]
  gear.sp.2[, i.QY := NULL]
  setcolorder(gear.sp.2, names(gear.sp))
  gear.sp.3 <- gear.sp.3[!is.na(SPPLIVMT), ]
  gear.sp.3[, QY     := i.QY]
  gear.sp.3[, HY     := i.HY]
  gear.sp.3[, i.QY := NULL]
  gear.sp.3[, i.HY := NULL]
  setcolorder(gear.sp.3, names(gear.sp))
  gear.sp.4 <- gear.sp.4[!is.na(SPPLIVMT), ]
  gear.sp.4[, QY     := i.QY]
  gear.sp.4[, HY     := i.HY]
  gear.sp.4[, SIZE   := i.SIZE]
  gear.sp.4[, i.QY   := NULL]
  gear.sp.4[, i.HY   := NULL]
  gear.sp.4[, i.SIZE := NULL]
  setcolorder(gear.sp.4, names(gear.sp))

  gear.sp <- data.table::rbindlist(list(gear.sp, gear.sp.2, gear.sp.3, gear.sp.4))

  gear.sp[, prop := SPPLIVMT / sum(SPPLIVMT), by = match.key]
  gear.sp[, unk  := i.SPPLIVMT * prop]
  gear.sp[, unk2 := i.SPPVALUE * prop]
  gear.sp[, c('SPPLIVMT', 'SPPVALUE', 'i.SPPLIVMT', 'i.SPPVALUE', 'i.GEAR',
              'i.UTILCD', 'prop') := NULL]
  data.table::setnames(gear.sp, c('unk','unk2'), c('SPPLIVMT', 'SPPVALUE'))
  gear.sp[, AREA := 0]

  setcolorder(gear.sp,    names(gear.solved))
  setcolorder(no.match.4, names(gear.solved))
  gear.solved <- data.table::rbindlist(list(gear.solved, gear.sp, no.match.4))
  rm(list = c(ls(pattern = 'gear.sp'), ls(pattern = 'no.match')))

  #Merge back gear.solved
  setcolorder(gear.solved, names(comland.agg))
  comland.agg <- data.table::rbindlist(list(k.gear, gear.solved))
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
