#' assign unknown catch using known catch characteristics
#'
#' Uses Half year and Quarter year. Expand .....
#'
#'@param comland Data frame. master data frame containing species landings
#'
#'@return updated comland data frame
#'
#'@importFrom data.table ":=" "key" "setcolorder" "as.data.table"
#'
#'@family assign catch

assign_catch_qy_hy <- function(comland.agg){
  #3.A QY/HY------------------------------------------------------------------------------
  unk.month <- comland.agg[QY == 0, ]
  k.month   <- comland.agg[QY != 0, ]

  #3.A.1 - All match
  match.key <- c('YEAR', 'NESPP3', 'GEAR', 'SIZE', 'AREA')

  unk.month.all <- unk.month[GEAR != 'unknown']
  unk.month.all <- unk.month.all[SIZE != 'unknown', ]
  unk.month.all <- unk.month.all[AREA != 0, ]

  k.month.all <- k.month[GEAR != 'unknown', ]
  k.month.all <- k.month.all[SIZE != 'unknown', ]
  k.month.all <- k.month.all[AREA != 0, ]

  data.table::setkeyv(unk.month.all, match.key)
  data.table::setkeyv(k.month.all,   match.key)

  month.all <- k.month.all[unk.month.all]

  #No match - need to match with larger aggregation
  no.match  <- month.all[is.na(SPPLIVMT), ]
  no.match[, c('QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match, c('i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  #Drop SIZE
  data.table::setkey(no.match, YEAR, NESPP3, AREA, GEAR)
  data.table::setkeyv(k.month.all, key(no.match))
  month.all.2 <- k.month.all[no.match]
  no.match.2 <- month.all.2[is.na(SPPLIVMT), ]
  no.match.2[, c('SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match.2, c('i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  #Drop GEAR
  data.table::setkey(no.match.2, YEAR, NESPP3, AREA)
  data.table::setkeyv(k.month.all, key(no.match.2))
  month.all.3 <- k.month.all[no.match.2]
  no.match.3 <- month.all.3[is.na(SPPLIVMT), ]
  no.match.3[, c('GEAR', 'SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match.3, c('i.GEAR', 'i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('GEAR', 'SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  #Drop AREA
  data.table::setkey(no.match.3, YEAR, NESPP3)
  data.table::setkeyv(k.month.all, key(no.match.3))
  month.all.4 <- k.month.all[no.match.3]
  no.match.4 <- month.all.4[is.na(SPPLIVMT), ]
  no.match.4[, c('AREA', 'GEAR', 'SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match.4, c('i.AREA', 'i.GEAR', 'i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD',
                                     'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('AREA', 'GEAR', 'SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  #Still no match - assign to first QY/HY
  no.match.4[, c('QY', 'HY') := 1]

  #Merge all together and proportion catch to known months
  month.all   <- month.all  [!is.na(SPPLIVMT), ]
  month.all.2 <- month.all.2[!is.na(SPPLIVMT), ]
  month.all.2[, SIZE   := i.SIZE]
  month.all.2[, i.SIZE := NULL]
  setcolorder(month.all.2, names(month.all))
  month.all.3 <- month.all.3[!is.na(SPPLIVMT), ]
  month.all.3[, GEAR   := i.GEAR]
  month.all.3[, SIZE   := i.SIZE]
  month.all.3[, i.GEAR := NULL]
  month.all.3[, i.SIZE := NULL]
  setcolorder(month.all.3, names(month.all))
  month.all.4 <- month.all.4[!is.na(SPPLIVMT), ]
  month.all.4[, AREA   := i.AREA]
  month.all.4[, GEAR   := i.GEAR]
  month.all.4[, SIZE   := i.SIZE]
  month.all.4[, i.AREA := NULL]
  month.all.4[, i.GEAR := NULL]
  month.all.4[, i.SIZE := NULL]
  setcolorder(month.all.4, names(month.all))

  month.all <- data.table::rbindlist(list(month.all, month.all.2, month.all.3, month.all.4))

  month.all[, prop := SPPLIVMT / sum(SPPLIVMT), by = match.key]
  month.all[, unk  := i.SPPLIVMT * prop]
  month.all[, unk2 := i.SPPVALUE * prop]
  month.all[, c('SPPLIVMT', 'SPPVALUE', 'i.SPPLIVMT', 'i.SPPVALUE', 'i.HY',
                'i.QY', 'i.UTILCD', 'prop') := NULL]
  data.table::setnames(month.all, c('unk', 'unk2'), c('SPPLIVMT', 'SPPVALUE'))

  setcolorder(no.match.4, names(month.all))
  month.solved <- data.table::rbindlist(list(month.all, no.match.4))
  rm(list = c(ls(pattern = 'month.all'), ls(pattern = 'no.match')))

  #3.A.2 - GEAR/SIZE
  match.key <- c('YEAR', 'NESPP3', 'GEAR', 'SIZE')

  unk.month.g.s <- unk.month[GEAR != 'unknown']
  unk.month.g.s <- unk.month.g.s[SIZE != 'unknown', ]
  unk.month.g.s <- unk.month.g.s[AREA == 0, ]
  unk.month.g.s <- unk.month.g.s[, list(sum(SPPLIVMT), sum(SPPVALUE)),
                                 by = c(match.key, 'QY', 'HY', 'UTILCD')]
  data.table::setnames(unk.month.g.s, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

  k.month.g.s <- k.month[GEAR != 'unknown', ]
  k.month.g.s <- k.month.g.s[SIZE != 'unknown', ]
  k.month.g.s <- k.month.g.s[, list(sum(SPPLIVMT), sum(SPPVALUE)),
                             by = c(match.key, 'QY', 'HY', 'UTILCD')]
  data.table::setnames(k.month.g.s, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

  data.table::setkeyv(unk.month.g.s, match.key)
  data.table::setkeyv(k.month.g.s,   match.key)

  month.g.s <- k.month.g.s[unk.month.g.s]

  #No match - need to match with larger aggregation
  no.match  <- month.g.s[is.na(SPPLIVMT), ]
  no.match[, c('QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match, c('i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  #Drop SIZE
  data.table::setkey(no.match, YEAR, NESPP3, GEAR)
  data.table::setkeyv(k.month.g.s, key(no.match))
  month.g.s.2 <- k.month.g.s[no.match]
  no.match.2 <- month.g.s.2[is.na(SPPLIVMT), ]
  no.match.2[, c('SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match.2, c('i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  #Drop GEAR
  data.table::setkey(no.match.2, YEAR, NESPP3)
  data.table::setkeyv(k.month.g.s, key(no.match.2))
  month.g.s.3 <- k.month.g.s[no.match.2]
  no.match.3 <- month.g.s.3[is.na(SPPLIVMT), ]
  no.match.3[, c('GEAR', 'SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match.3, c('i.GEAR', 'i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('GEAR', 'SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  #Still no match - assign to first QY/HY
  no.match.3[, c('QY', 'HY') := 1]
  no.match.3[, AREA := 0]

  #Merge all together and proportion catch to known months
  month.g.s   <- month.g.s  [!is.na(SPPLIVMT), ]
  month.g.s.2 <- month.g.s.2[!is.na(SPPLIVMT), ]
  if(nrow(month.g.s.2) > 0){
    month.g.s.2[, SIZE   := i.SIZE]
    month.g.s.2[, i.SIZE := NULL]
    setcolorder(month.g.s.2, names(month.g.s))
    month.g.s <- data.table::rbindlist(list(month.g.s, month.g.s.2))
  }
  month.g.s.3 <- month.g.s.3[!is.na(SPPLIVMT), ]
  if(nrow(month.g.s.3) > 0){
    month.g.s.3[, GEAR   := i.GEAR]
    month.g.s.3[, SIZE   := i.SIZE]
    month.g.s.3[, i.GEAR := NULL]
    month.g.s.3[, i.SIZE := NULL]
    setcolorder(month.g.s.3, names(month.g.s))
    month.g.s <- data.table::rbindlist(list(month.g.s, month.g.s.3))
  }

  month.g.s[, prop := SPPLIVMT / sum(SPPLIVMT), by = match.key]
  month.g.s[, unk  := i.SPPLIVMT * prop]
  month.g.s[, unk2 := i.SPPVALUE * prop]
  month.g.s[, c('SPPLIVMT', 'SPPVALUE', 'i.SPPLIVMT', 'i.SPPVALUE', 'i.HY',
                'i.QY', 'i.UTILCD', 'prop') := NULL]
  data.table::setnames(month.g.s, c('unk', 'unk2'), c('SPPLIVMT', 'SPPVALUE'))
  month.g.s[, AREA := 0]

  setcolorder(month.g.s,  names(month.solved))
  setcolorder(no.match.3, names(month.solved))
  month.solved <- data.table::rbindlist(list(month.solved, month.g.s, no.match.3))
  rm(list = c(ls(pattern = 'month.g.s'), ls(pattern = 'no.match')))

  #3.A.3 - AREA/GEAR
  match.key <- c('YEAR', 'NESPP3', 'GEAR', 'AREA')

  unk.month.a.g <- unk.month[GEAR != 'unknown']
  unk.month.a.g <- unk.month.a.g[SIZE == 'unknown', ]
  unk.month.a.g <- unk.month.a.g[AREA != 0, ]
  unk.month.a.g <- unk.month.a.g[, list(sum(SPPLIVMT), sum(SPPVALUE)),
                                 by = c(match.key, 'QY', 'HY', 'UTILCD')]
  data.table::setnames(unk.month.a.g, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

  k.month.a.g <- k.month[GEAR != 'unknown', ]
  k.month.a.g <- k.month.a.g[AREA != 0, ]
  k.month.a.g <- k.month.a.g[, list(sum(SPPLIVMT), sum(SPPVALUE)),
                             by = c(match.key, 'QY', 'HY', 'UTILCD')]
  data.table::setnames(k.month.a.g, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

  data.table::setkeyv(unk.month.a.g, match.key)
  data.table::setkeyv(k.month.a.g,   match.key)

  month.a.g <- k.month.a.g[unk.month.a.g]

  #No match - need to match with larger aggregation
  no.match  <- month.a.g[is.na(SPPLIVMT), ]
  no.match[, c('QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match, c('i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  #Drop GEAR
  data.table::setkey(no.match, YEAR, NESPP3, AREA)
  data.table::setkeyv(k.month.a.g, key(no.match))
  month.a.g.2 <- k.month.a.g[no.match]
  no.match.2 <- month.a.g.2[is.na(SPPLIVMT), ]
  no.match.2[, c('GEAR', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match.2, c('i.GEAR', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('GEAR', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  #Drop AREA
  data.table::setkey(no.match.2, YEAR, NESPP3)
  data.table::setkeyv(k.month.a.g, key(no.match.2))
  month.a.g.3 <- k.month.a.g[no.match.2]
  no.match.3 <- month.a.g.3[is.na(SPPLIVMT), ]
  no.match.3[, c('AREA', 'GEAR', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match.3, c('i.AREA', 'i.GEAR', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('AREA', 'GEAR', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  #Still no match - assign to first QY/HY
  no.match.3[, c('QY', 'HY') := 1]
  no.match.3[, SIZE := factor('unknown', levels = c('large', 'small', 'unknown'))]

  #Merge all together and proportion catch to known months
  month.a.g   <- month.a.g  [!is.na(SPPLIVMT), ]
  month.a.g.2 <- month.a.g.2[!is.na(SPPLIVMT), ]
  if(nrow(month.a.g.2) > 0){
    month.a.g.2[, GEAR   := i.GEAR]
    month.a.g.2[, i.GEAR := NULL]
    setcolorder(month.a.g.2, names(month.a.g))
    month.a.g <- data.table::rbindlist(list(month.a.g, month.a.g.2))
  }
  month.a.g.3 <- month.a.g.3[!is.na(SPPLIVMT), ]
  if(nrow(month.a.g.3) > 0){
    month.a.g.3[, AREA   := i.AREA]
    month.a.g.3[, GEAR   := i.GEAR]
    month.a.g.3[, i.AREA := NULL]
    month.a.g.3[, i.GEAR := NULL]
    setcolorder(month.a.g.3, names(month.a.g))
    month.a.g <- data.table::rbindlist(list(month.a.g, month.a.g.3))
  }

  month.a.g[, prop := SPPLIVMT / sum(SPPLIVMT), by = match.key]
  month.a.g[, unk  := i.SPPLIVMT * prop]
  month.a.g[, unk2 := i.SPPVALUE * prop]
  month.a.g[, c('SPPLIVMT', 'SPPVALUE', 'i.SPPLIVMT', 'i.SPPVALUE', 'i.HY',
                'i.QY', 'i.UTILCD', 'prop') := NULL]
  data.table::setnames(month.a.g, c('unk', 'unk2'), c('SPPLIVMT', 'SPPVALUE'))
  month.a.g[, SIZE := factor('unknown', levels = c('large', 'small', 'unknown'))]

  setcolorder(month.a.g,  names(month.solved))
  setcolorder(no.match.3, names(month.solved))
  month.solved <- data.table::rbindlist(list(month.solved, month.a.g, no.match.3))
  rm(list = c(ls(pattern = 'month.a.g'), ls(pattern = 'no.match')))

  #3.A.4 - AREA/TC
  match.key <- c('YEAR', 'NESPP3', 'SIZE', 'AREA')

  unk.month.a.s <- unk.month[GEAR == 'unknown']
  unk.month.a.s <- unk.month.a.s[SIZE != 'unknown', ]
  unk.month.a.s <- unk.month.a.s[AREA != 0, ]
  unk.month.a.s <- unk.month.a.s[, list(sum(SPPLIVMT), sum(SPPVALUE)),
                                 by = c(match.key, 'QY', 'HY', 'UTILCD')]
  data.table::setnames(unk.month.a.s, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

  k.month.a.s <- k.month[SIZE != 'unknown', ]
  k.month.a.s <- k.month.a.s[AREA != 0, ]
  k.month.a.s <- k.month.a.s[, list(sum(SPPLIVMT), sum(SPPVALUE)),
                             by = c(match.key, 'QY', 'HY', 'UTILCD')]
  data.table::setnames(k.month.a.s, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

  data.table::setkeyv(unk.month.a.s, match.key)
  data.table::setkeyv(k.month.a.s,   match.key)

  month.a.s <- k.month.a.s[unk.month.a.s]

  #No match - need to match with larger aggregation
  no.match  <- month.a.s[is.na(SPPLIVMT), ]
  no.match[, c('QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match, c('i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  #Drop SIZE
  data.table::setkey(no.match, YEAR, NESPP3, AREA)
  data.table::setkeyv(k.month.a.s, key(no.match))
  month.a.s.2 <- k.month.a.s[no.match]
  no.match.2 <- month.a.s.2[is.na(SPPLIVMT), ]
  no.match.2[, c('SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match.2, c('i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  #Drop AREA
  data.table::setkey(no.match.2, YEAR, NESPP3)
  data.table::setkeyv(k.month.a.s, key(no.match.2))
  month.a.s.3 <- k.month.a.s[no.match.2]
  no.match.3 <- month.a.s.3[is.na(SPPLIVMT), ]
  no.match.3[, c('AREA', 'SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match.3, c('i.AREA', 'i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('AREA', 'SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  #Still no match - assign to first QY/HY
  no.match.3[, c('QY', 'HY') := 1]
  no.match.3[, GEAR := factor('unknown', levels = levels(k.month[, GEAR]))]

  #Merge all together and proportion catch to known months
  month.a.s   <- month.a.s  [!is.na(SPPLIVMT), ]
  month.a.s.2 <- month.a.s.2[!is.na(SPPLIVMT), ]
  if(nrow(month.a.s.2) > 0){
    month.a.s.2[, SIZE   := i.SIZE]
    month.a.s.2[, i.SIZE := NULL]
    setcolorder(month.a.s.2, names(month.a.s))
    month.a.s <- data.table::rbindlist(list(month.a.s, month.a.s.2))
  }
  month.a.s.3 <- month.a.s.3[!is.na(SPPLIVMT), ]
  if(nrow(month.a.s.3) > 0){
    month.a.s.3[, AREA   := i.AREA]
    month.a.s.3[, SIZE   := i.SIZE]
    month.a.s.3[, i.AREA := NULL]
    month.a.s.3[, i.SIZE := NULL]
    setcolorder(month.a.s.3, names(month.a.s))
    month.a.s <- data.table::rbindlist(list(month.a.s, month.a.s.3))
  }

  month.a.s[, prop := SPPLIVMT / sum(SPPLIVMT), by = match.key]
  month.a.s[, unk  := i.SPPLIVMT * prop]
  month.a.s[, unk2 := i.SPPVALUE * prop]
  month.a.s[, c('SPPLIVMT', 'SPPVALUE', 'i.SPPLIVMT', 'i.SPPVALUE', 'i.HY',
                'i.QY', 'i.UTILCD', 'prop') := NULL]
  data.table::setnames(month.a.s, c('unk', 'unk2'), c('SPPLIVMT', 'SPPVALUE'))
  month.a.s[, GEAR := factor('unknown', levels = levels(k.month[, GEAR]))]

  setcolorder(month.a.s,  names(month.solved))
  setcolorder(no.match.3, names(month.solved))
  month.solved <- data.table::rbindlist(list(month.solved, month.a.s, no.match.3))

  rm(list = c(ls(pattern = 'month.a.s'), ls(pattern = 'no.match')))

  #3.A.5 - SIZE
  match.key <- c('YEAR', 'NESPP3', 'SIZE')

  unk.month.si <- unk.month[GEAR == 'unknown']
  unk.month.si <- unk.month.si[SIZE != 'unknown', ]
  unk.month.si <- unk.month.si[AREA == 0, ]
  unk.month.si <- unk.month.si[, list(sum(SPPLIVMT), sum(SPPVALUE)),
                               by = c(match.key, 'QY', 'HY', 'UTILCD')]
  data.table::setnames(unk.month.si, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

  k.month.si <- k.month[SIZE != 'unknown', ]
  k.month.si <- k.month.si[, list(sum(SPPLIVMT), sum(SPPVALUE)),
                           by = c(match.key, 'QY', 'HY', 'UTILCD')]
  data.table::setnames(k.month.si, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

  data.table::setkeyv(unk.month.si, match.key)
  data.table::setkeyv(k.month.si,   match.key)

  month.si <- k.month.si[unk.month.si]

  #No match - need to match with larger aggregation
  no.match  <- month.si[is.na(SPPLIVMT), ]
  no.match[, c('QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match, c('i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  #Drop SIZE
  data.table::setkey(no.match, YEAR, NESPP3)
  data.table::setkeyv(k.month.si, key(no.match))
  month.si.2 <- k.month.si[no.match]
  no.match.2 <- month.si.2[is.na(SPPLIVMT), ]
  no.match.2[, c('SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match.2, c('i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  #Still no match - assign to first QY/HY
  no.match.2[, c('QY', 'HY') := 1]
  no.match.2[, AREA := 0]
  no.match.2[, GEAR := factor('unknown', levels = levels(k.month[, GEAR]))]

  #Merge all together and proportion catch to known months
  month.si   <- month.si  [!is.na(SPPLIVMT), ]
  month.si.2 <- month.si.2[!is.na(SPPLIVMT), ]
  if(nrow(month.si.2) > 0){
    month.si.2[, SIZE   := i.SIZE]
    month.si.2[, i.SIZE := NULL]
    setcolorder(month.si.2, names(month.si))
    month.si <- data.table::rbindlist(list(month.si, month.si.2))
  }

  month.si[, prop := SPPLIVMT / sum(SPPLIVMT), by = match.key]
  month.si[, unk  := i.SPPLIVMT * prop]
  month.si[, unk2 := i.SPPVALUE * prop]
  month.si[, c('SPPLIVMT', 'SPPVALUE', 'i.SPPLIVMT', 'i.SPPVALUE', 'i.HY', 'i.QY',
               'i.UTILCD', 'prop') := NULL]
  data.table::setnames(month.si, c('unk', 'unk2'), c('SPPLIVMT', 'SPPVALUE'))
  month.si[, AREA := 0]
  month.si[, GEAR := factor('unknown', levels = levels(k.month[, GEAR]))]

  setcolorder(month.si,  names(month.solved))
  setcolorder(no.match.2, names(month.solved))
  month.solved <- data.table::rbindlist(list(month.solved, month.si, no.match.2))
  rm(list = c(ls(pattern = 'month.si'), ls(pattern = 'no.match')))

  #3.A.6 - GEAR
  match.key <- c('YEAR', 'NESPP3', 'GEAR')

  unk.month.g <- unk.month[GEAR != 'unknown']
  unk.month.g <- unk.month.g[SIZE == 'unknown', ]
  unk.month.g <- unk.month.g[AREA == 0, ]
  unk.month.g <- unk.month.g[, list(sum(SPPLIVMT), sum(SPPVALUE)),
                             by = c(match.key, 'QY', 'HY', 'UTILCD')]
  data.table::setnames(unk.month.g, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

  k.month.g <- k.month[GEAR != 'unknown', ]
  k.month.g <- k.month.g[, list(sum(SPPLIVMT), sum(SPPVALUE)),
                         by = c(match.key, 'QY', 'HY', 'UTILCD')]
  data.table::setnames(k.month.g, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

  data.table::setkeyv(unk.month.g, match.key)
  data.table::setkeyv(k.month.g,   match.key)

  month.g <- k.month.g[unk.month.g]

  #No match - need to match with larger aggregation
  no.match  <- month.g[is.na(SPPLIVMT), ]
  no.match[, c('QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match, c('i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  #Drop GEAR
  data.table::setkey(no.match, YEAR, NESPP3)
  data.table::setkeyv(k.month.g, key(no.match))
  month.g.2 <- k.month.g[no.match]
  no.match.2 <- month.g.2[is.na(SPPLIVMT), ]
  no.match.2[, c('GEAR', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match.2, c('i.GEAR', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('GEAR', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  #Still no match - assign to first QY/HY
  no.match.2[, c('QY', 'HY') := 1]
  no.match.2[, SIZE := factor('unknown', levels = c('large', 'small', 'unknown'))]
  no.match.2[, AREA := 0]

  #Merge all together and proportion catch to known months
  month.g   <- month.g  [!is.na(SPPLIVMT), ]
  month.g.2 <- month.g.2[!is.na(SPPLIVMT), ]
  if(nrow(month.g.2) > 0){
    month.g.2[, GEAR   := i.GEAR]
    month.g.2[, i.GEAR := NULL]
    setcolorder(month.g.2, names(month.g))
    month.g <- data.table::rbindlist(list(month.g, month.g.2))
  }

  month.g[, prop := SPPLIVMT / sum(SPPLIVMT), by = match.key]
  month.g[, unk  := i.SPPLIVMT * prop]
  month.g[, unk2 := i.SPPVALUE * prop]
  month.g[, c('SPPLIVMT', 'SPPVALUE', 'i.SPPLIVMT', 'i.SPPVALUE', 'i.HY', 'i.QY',
              'i.UTILCD', 'prop') := NULL]
  data.table::setnames(month.g, c('unk', 'unk2'), c('SPPLIVMT', 'SPPVALUE'))
  month.g[, SIZE := factor('unknown', levels = c('large', 'small', 'unknown'))]
  month.g[, AREA := 0]

  setcolorder(month.g,  names(month.solved))
  setcolorder(no.match.2, names(month.solved))
  month.solved <- data.table::rbindlist(list(month.solved, month.g, no.match.2))
  rm(list = c(ls(pattern = 'month.g'), ls(pattern = 'no.match')))

  #3.A.7 - AREA
  match.key <- c('YEAR', 'NESPP3', 'AREA')

  unk.month.a <- unk.month[GEAR == 'unknown']
  unk.month.a <- unk.month.a[SIZE == 'unknown', ]
  unk.month.a <- unk.month.a[AREA != 0, ]
  unk.month.a <- unk.month.a[, list(sum(SPPLIVMT), sum(SPPVALUE)),
                             by = c(match.key, 'QY', 'HY', 'UTILCD')]
  data.table::setnames(unk.month.a, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

  k.month.a <- k.month[AREA != 0, ]
  k.month.a <- k.month.a[, list(sum(SPPLIVMT), sum(SPPVALUE)),
                         by = c(match.key, 'QY', 'HY', 'UTILCD')]
  data.table::setnames(k.month.a, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

  data.table::setkeyv(unk.month.a, match.key)
  data.table::setkeyv(k.month.a,   match.key)

  month.a <- k.month.a[unk.month.a]

  #No match - need to match with larger aggregation
  no.match  <- month.a[is.na(SPPLIVMT), ]
  no.match[, c('QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match, c('i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  #Drop AREA
  data.table::setkey(no.match, YEAR, NESPP3)
  data.table::setkeyv(k.month.a, key(no.match))
  month.a.2 <- k.month.a[no.match]
  no.match.2 <- month.a.2[is.na(SPPLIVMT), ]
  no.match.2[, c('AREA', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match.2, c('i.AREA', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('AREA', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  #Still no match - assign to first QY/HY
  no.match.2[, c('QY', 'HY') := 1]
  no.match.2[, SIZE := factor('unknown', levels = c('large', 'small', 'unknown'))]
  no.match.2[, GEAR := factor('unknown', levels = levels(k.month[, GEAR]))]

  #Merge all together and proportion catch to known months
  month.a   <- month.a  [!is.na(SPPLIVMT), ]
  month.a.2 <- month.a.2[!is.na(SPPLIVMT), ]
  if(nrow(month.a.2) > 0){
    month.a.2[, AREA   := i.AREA]
    month.a.2[, i.AREA := NULL]
    setcolorder(month.a.2, names(month.a))
    month.a <- data.table::rbindlist(list(month.a, month.a.2))
  }

  month.a[, prop := SPPLIVMT / sum(SPPLIVMT), by = match.key]
  month.a[, unk  := i.SPPLIVMT * prop]
  month.a[, unk2 := i.SPPVALUE * prop]
  month.a[, c('SPPLIVMT', 'SPPVALUE', 'i.SPPLIVMT', 'i.SPPVALUE', 'i.HY',
              'i.QY', 'i.UTILCD', 'prop') := NULL]
  data.table::setnames(month.a, c('unk','unk2'), c('SPPLIVMT', 'SPPVALUE'))
  month.a[, SIZE := factor('unknown', levels = c('large', 'small', 'unknown'))]
  month.a[, GEAR := factor('unknown', levels = levels(k.month[, GEAR]))]

  setcolorder(month.a,  names(month.solved))
  setcolorder(no.match.2, names(month.solved))
  month.solved <- data.table::rbindlist(list(month.solved, month.a, no.match.2))
  rm(list = c(ls(pattern = 'month.a'), ls(pattern = 'no.match')))

  #3.A.8 - Species only - no other match
  match.key <- c('YEAR', 'NESPP3')

  unk.month.sp <- unk.month[GEAR == 'unknown']
  unk.month.sp <- unk.month.sp[SIZE == 'unknown', ]
  unk.month.sp <- unk.month.sp[AREA == 0, ]
  unk.month.sp <- unk.month.sp[, list(sum(SPPLIVMT), sum(SPPVALUE)),
                               by = c(match.key, 'QY', 'HY', 'UTILCD')]
  data.table::setnames(unk.month.sp, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

  k.month.sp <- k.month[, list(sum(SPPLIVMT), sum(SPPVALUE)),
                        by = c(match.key, 'QY', 'HY', 'UTILCD')]
  data.table::setnames(k.month.sp, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

  data.table::setkeyv(unk.month.sp, match.key)
  data.table::setkeyv(k.month.sp,   match.key)

  month.sp <- k.month.sp[unk.month.sp]

  #No match - assign to first QY/HY
  no.match  <- month.sp[is.na(SPPLIVMT), ]
  no.match[, c('QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  data.table::setnames(no.match, c('i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'),
                       c('QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  no.match[, c('QY', 'HY') := 1]
  no.match[, AREA := 0]
  no.match[, GEAR := factor('unknown', levels = levels(k.month[, GEAR]))]
  no.match[, SIZE := factor('unknown', levels = c('large', 'small', 'unknown'))]

  #proportion catch to known months
  month.sp   <- month.sp  [!is.na(SPPLIVMT), ]

  month.sp[, prop := SPPLIVMT / sum(SPPLIVMT), by = match.key]
  month.sp[, unk  := i.SPPLIVMT * prop]
  month.sp[, unk2 := i.SPPVALUE * prop]
  month.sp[, c('SPPLIVMT', 'SPPVALUE', 'i.SPPLIVMT', 'i.SPPVALUE', 'i.HY', 'i.QY',
               'i.UTILCD', 'prop') := NULL]
  data.table::setnames(month.sp, c('unk','unk2'), c('SPPLIVMT', 'SPPVALUE'))
  month.sp[, AREA := 0]
  month.sp[, GEAR := factor('unknown', levels = levels(k.month[, GEAR]))]
  month.sp[, SIZE := factor('unknown', levels = c('large', 'small', 'unknown'))]

  setcolorder(month.sp,  names(month.solved))
  setcolorder(no.match, names(month.solved))
  month.solved <- data.table::rbindlist(list(month.solved, month.sp, no.match))
  rm(list = c(ls(pattern = 'month.sp'), ls(pattern = 'no.match')))

  #Merge back month.solved
  setcolorder(month.solved, names(comland.agg))
  comland.agg <- data.table::rbindlist(list(k.month, month.solved))
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
