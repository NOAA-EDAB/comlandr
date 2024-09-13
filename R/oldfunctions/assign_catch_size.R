#' assign unknown catch using known catch characteristics
#'
#' Uses size. Expand .....
#'
#'@param comland Data frame. master data frame containing species landings
#'
#'@return updated comland data frame
#'
#'@importFrom data.table ":=" "key" "setcolorder" "as.data.table"
#'
#'@family assign catch
#'
#' @noRd

assign_catch_size <- function(comland.agg){

  unk.size <- comland.agg[SIZE == 'unknown', ]
  k.size   <- comland.agg[SIZE != 'unknown', ]

  #3.B.1 - All match
  match.key <- c('YEAR', 'NESPP3', 'QY', 'HY', 'GEAR', 'AREA')

  unk.size.all <- unk.size[GEAR != 'unknown']
  unk.size.all <- unk.size.all[AREA != 0, ]

  k.size.all <- k.size[GEAR != 'unknown', ]
  k.size.all <- k.size.all[AREA != 0, ]

  data.table::setkeyv(unk.size.all, match.key)
  data.table::setkeyv(k.size.all,   match.key)

  size.all <- k.size.all[unk.size.all]

  #No match - need to match with larger aggregation
  no.match  <- size.all[is.na(SPPLIVMT), ]
  no.match[, c('SIZE', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match, c('i.SIZE', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('SIZE', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  #Drop QY
  data.table::setkey(no.match, YEAR, NESPP3, HY, GEAR, AREA)
  data.table::setkeyv(k.size.all, key(no.match))
  size.all.2 <- k.size.all[no.match]
  no.match.2 <- size.all.2[is.na(SPPLIVMT), ]
  no.match.2[, c('SIZE', 'QY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match.2, c('i.SIZE', 'i.QY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('SIZE', 'QY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  #Drop HY
  data.table::setkey(no.match.2, YEAR, NESPP3, GEAR, AREA)
  data.table::setkeyv(k.size.all, key(no.match.2))
  size.all.3 <- k.size.all[no.match.2]
  no.match.3 <- size.all.3[is.na(SPPLIVMT), ]
  no.match.3[, c('SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match.3, c('i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  #Drop GEAR
  data.table::setkey(no.match.3, YEAR, NESPP3, AREA)
  data.table::setkeyv(k.size.all, key(no.match.3))
  size.all.4 <- k.size.all[no.match.3]
  no.match.4 <- size.all.4[is.na(SPPLIVMT), ]
  no.match.4[, c('GEAR', 'SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match.4, c('i.GEAR', 'i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('GEAR',   'SIZE',   'QY',   'HY',  'UTILCD',  'SPPLIVMT', 'SPPVALUE'))
  #Drop AREA
  data.table::setkey(no.match.4, YEAR, NESPP3)
  data.table::setkeyv(k.size.all, key(no.match.4))
  size.all.5 <- k.size.all[no.match.4]
  no.match.5 <- size.all.5[is.na(SPPLIVMT), ]
  no.match.5[, c('AREA', 'GEAR', 'SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match.5, c('i.AREA', 'i.GEAR', 'i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('AREA',   'GEAR',   'SIZE',   'QY',   'HY',  'UTILCD',  'SPPLIVMT', 'SPPVALUE'))
  #Still no match - assign to SIZE to small
  no.match.5[, SIZE := factor('small', levels = c('large', 'small', 'unknown'))]

  #Merge all together and proportion catch to known sizes
  size.all   <- size.all  [!is.na(SPPLIVMT), ]
  size.all.2 <- size.all.2[!is.na(SPPLIVMT), ]
  size.all.2[, QY   := i.QY]
  size.all.2[, i.QY := NULL]
  setcolorder(size.all.2, names(size.all))
  size.all.3 <- size.all.3[!is.na(SPPLIVMT), ]
  size.all.3[, QY   := i.QY]
  size.all.3[, HY   := i.HY]
  size.all.3[, i.QY := NULL]
  size.all.3[, i.HY := NULL]
  setcolorder(size.all.3, names(size.all))
  size.all.4 <- size.all.4[!is.na(SPPLIVMT), ]
  size.all.4[, QY     := i.QY]
  size.all.4[, HY     := i.HY]
  size.all.4[, GEAR   := i.GEAR]
  size.all.4[, i.QY   := NULL]
  size.all.4[, i.HY   := NULL]
  size.all.4[, i.GEAR := NULL]
  setcolorder(size.all.4, names(size.all))
  size.all.5 <- size.all.5[!is.na(SPPLIVMT), ]
  size.all.5[, QY     := i.QY]
  size.all.5[, HY     := i.HY]
  size.all.5[, GEAR   := i.GEAR]
  size.all.5[, AREA   := i.AREA]
  size.all.5[, i.QY   := NULL]
  size.all.5[, i.HY   := NULL]
  size.all.5[, i.GEAR := NULL]
  size.all.5[, i.AREA := NULL]
  setcolorder(size.all.5, names(size.all))

  size.all <- data.table::rbindlist(list(size.all, size.all.2, size.all.3,
                                         size.all.4, size.all.5))

  size.all[, prop := SPPLIVMT / sum(SPPLIVMT), by = match.key]
  size.all[, unk  := i.SPPLIVMT * prop]
  size.all[, unk2 := i.SPPVALUE * prop]
  size.all[, c('SPPLIVMT', 'SPPVALUE', 'i.SPPLIVMT', 'i.SPPVALUE', 'i.SIZE',
               'i.UTILCD', 'prop') := NULL]
  data.table::setnames(size.all, c('unk','unk2'), c('SPPLIVMT', 'SPPVALUE'))

  setcolorder(no.match.5, names(size.all))
  size.solved <- data.table::rbindlist(list(size.all, no.match.5))
  rm(list = c(ls(pattern = 'size.all'), ls(pattern = 'no.match')))

  #3.B.2 - GEAR
  match.key <- c('YEAR', 'NESPP3', 'QY', 'HY', 'GEAR')

  unk.size.g <- unk.size[GEAR != 'unknown']
  unk.size.g <- unk.size.g[AREA == 0, ]
  unk.size.g[, AREA := NULL]

  k.size.g <- k.size[GEAR != 'unknown', ]
  k.size.g <- k.size.g[, list(sum(SPPLIVMT), sum(SPPVALUE)),
                       by = c(match.key, 'SIZE', 'UTILCD')]
  data.table::setnames(k.size.g, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

  data.table::setkeyv(unk.size.g, match.key)
  data.table::setkeyv(k.size.g,   match.key)

  size.g <- k.size.g[unk.size.g]

  #No match - need to match with larger aggregation
  no.match  <- size.g[is.na(SPPLIVMT), ]
  no.match[, c('SIZE', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match, c('i.SIZE', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('SIZE', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  #Drop QY
  data.table::setkey(no.match, YEAR, NESPP3, HY, GEAR)
  data.table::setkeyv(k.size.g, key(no.match))
  size.g.2 <- k.size.g[no.match]
  no.match.2 <- size.g.2[is.na(SPPLIVMT), ]
  no.match.2[, c('SIZE', 'QY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match.2, c('i.SIZE', 'i.QY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('SIZE', 'QY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  #Drop HY
  data.table::setkey(no.match.2, YEAR, NESPP3, GEAR)
  data.table::setkeyv(k.size.g, key(no.match.2))
  size.g.3 <- k.size.g[no.match.2]
  no.match.3 <- size.g.3[is.na(SPPLIVMT), ]
  no.match.3[, c('SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match.3, c('i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('SIZE',   'QY',   'HY',  'UTILCD',  'SPPLIVMT', 'SPPVALUE'))
  #Drop GEAR
  data.table::setkey(no.match.3, YEAR, NESPP3)
  data.table::setkeyv(k.size.g, key(no.match.3))
  size.g.4 <- k.size.g[no.match.3]
  no.match.4 <- size.g.4[is.na(SPPLIVMT), ]
  no.match.4[, c('GEAR', 'SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match.4, c('i.GEAR', 'i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('GEAR',   'SIZE',   'QY',   'HY',  'UTILCD',  'SPPLIVMT', 'SPPVALUE'))
  #Still no match - assign to SIZE to small
  no.match.4[, SIZE := factor('small', levels = c('large', 'small', 'unknown'))]
  no.match.4[, AREA := 0]

  #Merge all together and proportion catch to known sizes
  size.g   <- size.g  [!is.na(SPPLIVMT), ]
  size.g.2 <- size.g.2[!is.na(SPPLIVMT), ]
  size.g.2[, QY   := i.QY]
  size.g.2[, i.QY := NULL]
  setcolorder(size.g.2, names(size.g))
  size.g.3 <- size.g.3[!is.na(SPPLIVMT), ]
  size.g.3[, QY   := i.QY]
  size.g.3[, HY   := i.HY]
  size.g.3[, i.QY := NULL]
  size.g.3[, i.HY := NULL]
  setcolorder(size.g.3, names(size.g))
  size.g.4 <- size.g.4[!is.na(SPPLIVMT), ]
  size.g.4[, QY     := i.QY]
  size.g.4[, HY     := i.HY]
  size.g.4[, GEAR   := i.GEAR]
  size.g.4[, i.QY   := NULL]
  size.g.4[, i.HY   := NULL]
  size.g.4[, i.GEAR := NULL]
  setcolorder(size.g.4, names(size.g))

  size.g <- data.table::rbindlist(list(size.g, size.g.2, size.g.3, size.g.4))

  size.g[, prop := SPPLIVMT / sum(SPPLIVMT), by = match.key]
  size.g[, unk  := i.SPPLIVMT * prop]
  size.g[, unk2 := i.SPPVALUE * prop]
  size.g[, c('SPPLIVMT', 'SPPVALUE', 'i.SPPLIVMT', 'i.SPPVALUE', 'i.SIZE',
             'i.UTILCD', 'prop') := NULL]
  data.table::setnames(size.g, c('unk','unk2'), c('SPPLIVMT', 'SPPVALUE'))
  size.g[, AREA := 0]

  setcolorder(size.g,     names(size.solved))
  setcolorder(no.match.4, names(size.g))
  size.solved <- data.table::rbindlist(list(size.solved, size.g, no.match.4))
  rm(list = c(ls(pattern = 'size.g'), ls(pattern = 'no.match')))

  #3.B.3 - AREA
  match.key <- c('YEAR', 'NESPP3', 'QY', 'HY', 'AREA')

  unk.size.a <- unk.size[GEAR == 'unknown']
  unk.size.a <- unk.size.a[AREA != 0, ]
  unk.size.a[, GEAR := NULL]

  k.size.a <- k.size[AREA != 0, ]
  k.size.a <- k.size.a[, list(sum(SPPLIVMT), sum(SPPVALUE)),
                       by = c(match.key, 'SIZE', 'UTILCD')]
  data.table::setnames(k.size.a, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

  data.table::setkeyv(unk.size.a, match.key)
  data.table::setkeyv(k.size.a,   match.key)

  size.a <- k.size.a[unk.size.a]

  #No match - need to match with larger aggregation
  no.match  <- size.a[is.na(SPPLIVMT), ]
  no.match[, c('SIZE', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match, c('i.SIZE', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('SIZE', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  #Drop QY
  data.table::setkey(no.match, YEAR, NESPP3, HY, AREA)
  data.table::setkeyv(k.size.a, key(no.match))
  size.a.2 <- k.size.a[no.match]
  no.match.2 <- size.a.2[is.na(SPPLIVMT), ]
  no.match.2[, c('SIZE', 'QY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match.2, c('i.SIZE', 'i.QY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('SIZE', 'QY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  #Drop HY
  data.table::setkey(no.match.2, YEAR, NESPP3, AREA)
  data.table::setkeyv(k.size.a, key(no.match.2))
  size.a.3 <- k.size.a[no.match.2]
  no.match.3 <- size.a.3[is.na(SPPLIVMT), ]
  no.match.3[, c('SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match.3, c('i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('SIZE',   'QY',   'HY',  'UTILCD',  'SPPLIVMT', 'SPPVALUE'))
  #Drop AREA
  data.table::setkey(no.match.3, YEAR, NESPP3)
  data.table::setkeyv(k.size.a, key(no.match.3))
  size.a.4 <- k.size.a[no.match.3]
  no.match.4 <- size.a.4[is.na(SPPLIVMT), ]
  no.match.4[, c('AREA', 'SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match.4, c('i.AREA', 'i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('AREA',   'SIZE',   'QY',   'HY',  'UTILCD',  'SPPLIVMT', 'SPPVALUE'))
  #Still no match - assign to SIZE to small
  no.match.4[, SIZE := factor('small',   levels = c('large', 'small', 'unknown'))]
  no.match.4[, GEAR := factor('unknown', levels = levels(k.size[, GEAR]))]

  #Merge all together and proportion catch to known sizes
  size.a   <- size.a  [!is.na(SPPLIVMT), ]
  size.a.2 <- size.a.2[!is.na(SPPLIVMT), ]
  size.a.2[, QY   := i.QY]
  size.a.2[, i.QY := NULL]
  setcolorder(size.a.2, names(size.a))
  size.a.3 <- size.a.3[!is.na(SPPLIVMT), ]
  size.a.3[, QY     := i.QY]
  size.a.3[, HY     := i.HY]
  size.a.3[, i.QY := NULL]
  size.a.3[, i.HY := NULL]
  setcolorder(size.a.3, names(size.a))
  size.a.4 <- size.a.4[!is.na(SPPLIVMT), ]
  size.a.4[, QY     := i.QY]
  size.a.4[, HY     := i.HY]
  size.a.4[, AREA   := i.AREA]
  size.a.4[, i.QY   := NULL]
  size.a.4[, i.HY   := NULL]
  size.a.4[, i.AREA := NULL]
  setcolorder(size.a.4, names(size.a))

  size.a <- data.table::rbindlist(list(size.a, size.a.2, size.a.3, size.a.4))

  size.a[, prop := SPPLIVMT / sum(SPPLIVMT), by = match.key]
  size.a[, unk  := i.SPPLIVMT * prop]
  size.a[, unk2 := i.SPPVALUE * prop]
  size.a[, c('SPPLIVMT', 'SPPVALUE', 'i.SPPLIVMT', 'i.SPPVALUE', 'i.SIZE',
             'i.UTILCD', 'prop') := NULL]
  data.table::setnames(size.a, c('unk','unk2'), c('SPPLIVMT', 'SPPVALUE'))
  size.a[, GEAR := factor('unknown', levels = levels(k.size[, GEAR]))]

  setcolorder(size.a,     names(size.solved))
  setcolorder(no.match.4, names(size.a))
  size.solved <- data.table::rbindlist(list(size.solved, size.a, no.match.4))
  rm(list = c(ls(pattern = 'size.a'), ls(pattern = 'no.match')))

  #3.B.4 - Species only - no other match
  match.key <- c('YEAR', 'NESPP3', 'QY', 'HY')

  unk.size.sp <- unk.size[GEAR == 'unknown']
  unk.size.sp <- unk.size.sp[SIZE == 'unknown', ]
  unk.size.sp <- unk.size.sp[AREA == 0, ]
  unk.size.sp[, c('GEAR', 'AREA') := NULL]

  k.size.sp <- k.size[, list(sum(SPPLIVMT), sum(SPPVALUE)),
                      by = c(match.key, 'SIZE', 'UTILCD')]
  data.table::setnames(k.size.sp, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

  data.table::setkeyv(unk.size.sp, match.key)
  data.table::setkeyv(k.size.sp,   match.key)

  size.sp <- k.size.sp[unk.size.sp]

  #No match - need to match with larger aggregation
  no.match  <- size.sp[is.na(SPPLIVMT), ]
  no.match[, c('SIZE', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match, c('i.SIZE', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('SIZE', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  #Drop QY
  data.table::setkey(no.match, YEAR, NESPP3, HY)
  data.table::setkeyv(k.size.sp, key(no.match))
  size.sp.2 <- k.size.sp[no.match]
  no.match.2 <- size.sp.2[is.na(SPPLIVMT), ]
  no.match.2[, c('SIZE', 'QY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match.2, c('i.SIZE', 'i.QY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('SIZE', 'QY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  #Drop HY
  data.table::setkey(no.match.2, YEAR, NESPP3)
  data.table::setkeyv(k.size.sp, key(no.match.2))
  size.sp.3 <- k.size.sp[no.match.2]
  no.match.3 <- size.sp.3[is.na(SPPLIVMT), ]
  no.match.3[, c('SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match.3, c('i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('SIZE',   'QY',   'HY',  'UTILCD',  'SPPLIVMT', 'SPPVALUE'))
  #Still no match - assign to SIZE to small
  no.match.3[, SIZE := factor('small', levels = c('large', 'small', 'unknown'))]
  no.match.3[, GEAR := factor('unknown', levels = levels(k.size[, GEAR]))]
  no.match.3[, AREA := 0]

  #Merge together and proportion catch to known sizes
  size.sp   <- size.sp  [!is.na(SPPLIVMT), ]
  size.sp.2 <- size.sp.2[!is.na(SPPLIVMT), ]
  size.sp.2[, QY   := i.QY]
  size.sp.2[, i.QY := NULL]
  setcolorder(size.sp.2, names(size.sp))
  size.sp.3 <- size.sp.3[!is.na(SPPLIVMT), ]
  size.sp.3[, QY     := i.QY]
  size.sp.3[, HY     := i.HY]
  size.sp.3[, i.QY := NULL]
  size.sp.3[, i.HY := NULL]
  setcolorder(size.sp.3, names(size.sp))

  size.sp <- data.table::rbindlist(list(size.sp, size.sp.2, size.sp.3))

  size.sp[, prop := SPPLIVMT / sum(SPPLIVMT), by = match.key]
  size.sp[, unk  := i.SPPLIVMT * prop]
  size.sp[, unk2 := i.SPPVALUE * prop]
  size.sp[, c('SPPLIVMT', 'SPPVALUE', 'i.SPPLIVMT', 'i.SPPVALUE', 'i.SIZE',
              'i.UTILCD', 'prop') := NULL]
  data.table::setnames(size.sp, c('unk','unk2'), c('SPPLIVMT', 'SPPVALUE'))
  size.sp[, AREA := 0]
  size.sp[, GEAR := factor('unknown', levels = levels(k.size[, GEAR]))]

  setcolorder(size.sp,    names(size.solved))
  setcolorder(no.match.3, names(size.solved))
  size.solved <- data.table::rbindlist(list(size.solved, size.sp, no.match.3))
  rm(list = c(ls(pattern = 'size.sp'), ls(pattern = 'no.match')))

  #Merge back size.solved
  setcolorder(size.solved, names(comland.agg))
  comland.agg <- data.table::rbindlist(list(k.size, size.solved))
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
