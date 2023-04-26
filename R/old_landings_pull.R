#SOE revenue data
##SML

#Required packages
library(data.table); library(survdat); library(comlandr); library(here)

#-------------------------------------------------------------------------------
#If pulling data
channelSole <- dbutils::connect_to_database(server = "sole", uid = "slucey")
channelNova <- dbutils::connect_to_database(server = "NEFSC_USERS", uid = "slucey")

filterByYear <- 1964:2021

comland <- get_comland_raw_data(channelSole, channelNova, filterByYear = filterByYear)
#save(comland, file = here::here('data_raw', 'comland_raw.rda'))
load(here::here('data_raw', 'comland_raw.rda'))

#Pull herring data from the state of Maine
comland <- comlandr::get_herring_data(channelSole, comland, filterByYear, 
                                      filterByArea = NA, useForeign = T)


#Pull foreign landings
#comland.foreign <- comlandr::get_foreign_data(filterByYear, filterByArea = NA)
#comland.foreign <- comlandr::process_foreign_data(channelSole, comland.foreign, 
#                                                    useHerringMaine = T)
  
#Combine foreign landings
#comland$comland <- data.table::rbindlist(list(comland$comland, comland.foreign), 
#                                           use.names = T)

#Apply correction for inflation
comland <- comlandr::adjust_inflation(comland, refYear = 2021, refMonth = 1)

#Disaggregate skates and hakes
comland <- comlandr::disaggregate_skates_hakes(comland, channelSole, filterByYear, 
                                               filterByArea = NA)

#Aggregate gears
# comland <- comlandr::aggregate_gear(comland, userGears = comlandr::mskeyGears, 
#                                     fleetDescription = 'Fleet')

#Impute unknowns
#comland <- comlandr::assign_unknown(comland, unkVar = c('MONTH', 'Fleet', 'AREA'), 
#                                    knStrata = c('AREA', 'Fleet', 'HY', 'QY', 'MONTH'))

#unknowns----
comdata <- copy(comland$comland)

comdata[NEGEAR == 999,  NEGEAR := 0]
comdata[is.na(TONCL1),  TONCL1 := 0]
comdata[is.na(AREA),    AREA   := 0]
comdata[AREA == 999,    AREA   := 0]
comdata[is.na(MKTCAT),  MKTCAT := 0]
comdata[is.na(UTILCD),  UTILCD := 0]

#1 - drop unknown species/landings
comdata <- comdata[NESPP3 != 0 & SPPLIVMT != 0, ]

#Sumarry tables
#missing area
#known.area <-   comland[AREA != 0, sum(SPPLIVMT), by = NESPP3]
#unknown.area <- comland[AREA == 0, sum(SPPLIVMT), by = NESPP3]
#setnames(known.area,   "V1", "AREA.MT.known")
#setnames(unknown.area, "V1", "AREA.MT.unknown")
#missing.table <- merge(known.area, unknown.area, by = 'NESPP3', all = T)
#
#missing.table[is.na(AREA.MT.known),   AREA.MT.known   := 0]
#missing.table[is.na(AREA.MT.unknown), AREA.MT.unknown := 0]
#missing.table[, AREA.Ratio := AREA.MT.unknown / AREA.MT.known]
#
##missing month
#known.month <-   comland[MONTH != 0, sum(SPPLIVMT), by = NESPP3]
#unknown.month <- comland[MONTH == 0, sum(SPPLIVMT), by = NESPP3]
#setnames(known.month,   "V1", "MONTH.MT.known")
#setnames(unknown.month, "V1", "MONTH.MT.unknown")
#missing.table <- merge(missing.table, known.month,   by = 'NESPP3', all = T)
#missing.table <- merge(missing.table, unknown.month, by = 'NESPP3', all = T)
#
#missing.table[is.na(MONTH.MT.known),   MONTH.MT.known   := 0]
#missing.table[is.na(MONTH.MT.unknown), MONTH.MT.unknown := 0]
#missing.table[, MONTH.Ratio := MONTH.MT.unknown / MONTH.MT.known]
#
##missing gear
#known.gear <-   comland[NEGEAR != 0, sum(SPPLIVMT), by = NESPP3]
#unknown.gear <- comland[NEGEAR == 0, sum(SPPLIVMT), by = NESPP3]
#setnames(known.gear,   "V1", "GEAR.MT.known")
#setnames(unknown.gear, "V1", "GEAR.MT.unknown")
#missing.table <- merge(missing.table, known.gear,   by = 'NESPP3', all = T)
#missing.table <- merge(missing.table, unknown.gear, by = 'NESPP3', all = T)
#
#missing.table[is.na(GEAR.MT.known),   GEAR.MT.known   := 0]
#missing.table[is.na(GEAR.MT.unknown), GEAR.MT.unknown := 0]
#missing.table[, GEAR.Ratio := GEAR.MT.unknown / GEAR.MT.known]
#
##missing tonnage class
#known.tc <-   comland[TONCL1 != 0, sum(SPPLIVMT), by = NESPP3]
#unknown.tc <- comland[TONCL1 == 0, sum(SPPLIVMT), by = NESPP3]
#setnames(known.tc,   "V1", "TC.MT.known")
#setnames(unknown.tc, "V1", "TC.MT.unknown")
#missing.table <- merge(missing.table, known.tc,   by = 'NESPP3', all = T)
#missing.table <- merge(missing.table, unknown.tc, by = 'NESPP3', all = T)
#
#missing.table[is.na(TC.MT.known),   TC.MT.known   := 0]
#missing.table[is.na(TC.MT.unknown), TC.MT.unknown := 0]
#missing.table[, TC.Ratio := TC.MT.unknown / TC.MT.known]
#
#write.csv(missing.table, paste(out.dir, "\\Missing_table.csv", sep = ''), row.names = F)
#

#2 - aggregate by quarter year, half year, major gear, and small/large TC
comdata[MONTH %in% 1:3,   QY := 1]
comdata[MONTH %in% 4:6,   QY := 2]
comdata[MONTH %in% 7:9,   QY := 3]
comdata[MONTH %in% 10:12, QY := 4]
comdata[MONTH == 0,       QY := 0]

comdata[MONTH %in% 1:6,  HY := 1]
comdata[MONTH %in% 7:12, HY := 2]
comdata[MONTH == 0,      HY := 0]

otter     <- 50:59
dredge.sc <- 131:132
pot       <- c(180:190, 200:219, 300, 301)
longline  <- c(10, 40)
seine     <- c(70:79, 120:129, 360)
gillnet   <- c(100:119, 500, 510, 520)
midwater  <- c(170, 370)
dredge.o  <- c(281, 282, 380:400)

comdata[NEGEAR %in% otter,     GEAR := 'otter']
comdata[NEGEAR %in% dredge.sc, GEAR := 'dredge.sc']
comdata[NEGEAR %in% pot,       GEAR := 'pot']
comdata[NEGEAR %in% longline,  GEAR := 'longline']
comdata[NEGEAR %in% seine,     GEAR := 'seine']
comdata[NEGEAR %in% gillnet,   GEAR := 'gillnet']
comdata[NEGEAR %in% midwater,  GEAR := 'midwater']
comdata[NEGEAR %in% dredge.o,  GEAR := 'dredge.o']
comdata[NEGEAR == 0,           GEAR := 'unknown']
comdata[is.na(GEAR),           GEAR := 'other']
comdata[, GEAR := as.factor(GEAR)]


comdata[TONCL1 %in% 1:3, SIZE := 'small']
comdata[TONCL1 > 3,      SIZE := 'large']
comdata[TONCL1 == 0,     SIZE := 'unknown']
comdata[, SIZE := as.factor(SIZE)]

setkey(comdata,
       YEAR,
       QY,
       HY,
       GEAR,
       SIZE,
       AREA,
       NESPP3,
       UTILCD)

comland.agg <- comdata[, list(sum(SPPLIVMT), sum(SPPVALUE)), by = key(comdata)]

setnames(comland.agg, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

#3 - Use proportions of known catch to assign unknown catch
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

setkeyv(unk.month.all, match.key)
setkeyv(k.month.all,   match.key)

month.all <- k.month.all[unk.month.all]

#No match - need to match with larger aggregation
no.match  <- month.all[is.na(SPPLIVMT), ]
no.match[, c('QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match, c('i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
         c('QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
#Drop SIZE
setkey(no.match, YEAR, NESPP3, AREA, GEAR)
setkeyv(k.month.all, key(no.match))
month.all.2 <- k.month.all[no.match]
no.match.2 <- month.all.2[is.na(SPPLIVMT), ]
no.match.2[, c('SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match.2, c('i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
         c('SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
#Drop GEAR
setkey(no.match.2, YEAR, NESPP3, AREA)
setkeyv(k.month.all, key(no.match.2))
month.all.3 <- k.month.all[no.match.2]
no.match.3 <- month.all.3[is.na(SPPLIVMT), ]
no.match.3[, c('GEAR', 'SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match.3, c('i.GEAR', 'i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
         c('GEAR', 'SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
#Drop AREA
setkey(no.match.3, YEAR, NESPP3)
setkeyv(k.month.all, key(no.match.3))
month.all.4 <- k.month.all[no.match.3]
no.match.4 <- month.all.4[is.na(SPPLIVMT), ]
no.match.4[, c('AREA', 'GEAR', 'SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match.4, c('i.AREA', 'i.GEAR', 'i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD', 
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

month.all <- rbindlist(list(month.all, month.all.2, month.all.3, month.all.4))

month.all[, prop := SPPLIVMT / sum(SPPLIVMT), by = match.key]
month.all[, unk  := i.SPPLIVMT * prop]
month.all[, unk2 := i.SPPVALUE * prop]
month.all[, c('SPPLIVMT', 'SPPVALUE', 'i.SPPLIVMT', 'i.SPPVALUE', 'i.HY', 
              'i.QY', 'i.UTILCD', 'prop') := NULL]
setnames(month.all, c('unk', 'unk2'), c('SPPLIVMT', 'SPPVALUE'))

setcolorder(no.match.4, names(month.all))
month.solved <- rbindlist(list(month.all, no.match.4))
rm(list = c(ls(pattern = 'month.all'), ls(pattern = 'no.match')))

#3.A.2 - GEAR/SIZE
match.key <- c('YEAR', 'NESPP3', 'GEAR', 'SIZE')

unk.month.g.s <- unk.month[GEAR != 'unknown']
unk.month.g.s <- unk.month.g.s[SIZE != 'unknown', ]
unk.month.g.s <- unk.month.g.s[AREA == 0, ]
unk.month.g.s <- unk.month.g.s[, list(sum(SPPLIVMT), sum(SPPVALUE)), 
                               by = c(match.key, 'QY', 'HY', 'UTILCD')]
setnames(unk.month.g.s, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

k.month.g.s <- k.month[GEAR != 'unknown', ]
k.month.g.s <- k.month.g.s[SIZE != 'unknown', ]
k.month.g.s <- k.month.g.s[, list(sum(SPPLIVMT), sum(SPPVALUE)), 
                           by = c(match.key, 'QY', 'HY', 'UTILCD')]
setnames(k.month.g.s, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

setkeyv(unk.month.g.s, match.key)
setkeyv(k.month.g.s,   match.key)

month.g.s <- k.month.g.s[unk.month.g.s]

#No match - need to match with larger aggregation
no.match  <- month.g.s[is.na(SPPLIVMT), ]
no.match[, c('QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match, c('i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
         c('QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
#Drop SIZE
setkey(no.match, YEAR, NESPP3, GEAR)
setkeyv(k.month.g.s, key(no.match))
month.g.s.2 <- k.month.g.s[no.match]
no.match.2 <- month.g.s.2[is.na(SPPLIVMT), ]
no.match.2[, c('SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match.2, c('i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
         c('SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
#Drop GEAR
setkey(no.match.2, YEAR, NESPP3)
setkeyv(k.month.g.s, key(no.match.2))
month.g.s.3 <- k.month.g.s[no.match.2]
no.match.3 <- month.g.s.3[is.na(SPPLIVMT), ]
no.match.3[, c('GEAR', 'SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match.3, c('i.GEAR', 'i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
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
  month.g.s <- rbindlist(list(month.g.s, month.g.s.2))  
}
month.g.s.3 <- month.g.s.3[!is.na(SPPLIVMT), ]
if(nrow(month.g.s.3) > 0){
  month.g.s.3[, GEAR   := i.GEAR]
  month.g.s.3[, SIZE   := i.SIZE]
  month.g.s.3[, i.GEAR := NULL]
  month.g.s.3[, i.SIZE := NULL]
  setcolorder(month.g.s.3, names(month.g.s))
  month.g.s <- rbindlist(list(month.g.s, month.g.s.3))
}

month.g.s[, prop := SPPLIVMT / sum(SPPLIVMT), by = match.key]
month.g.s[, unk  := i.SPPLIVMT * prop]
month.g.s[, unk2 := i.SPPVALUE * prop]
month.g.s[, c('SPPLIVMT', 'SPPVALUE', 'i.SPPLIVMT', 'i.SPPVALUE', 'i.HY', 
              'i.QY', 'i.UTILCD', 'prop') := NULL]
setnames(month.g.s, c('unk', 'unk2'), c('SPPLIVMT', 'SPPVALUE'))
month.g.s[, AREA := 0]

setcolorder(month.g.s,  names(month.solved))
setcolorder(no.match.3, names(month.solved))
month.solved <- rbindlist(list(month.solved, month.g.s, no.match.3))
rm(list = c(ls(pattern = 'month.g.s'), ls(pattern = 'no.match')))

#3.A.3 - AREA/GEAR
match.key <- c('YEAR', 'NESPP3', 'GEAR', 'AREA')

unk.month.a.g <- unk.month[GEAR != 'unknown']
unk.month.a.g <- unk.month.a.g[SIZE == 'unknown', ]
unk.month.a.g <- unk.month.a.g[AREA != 0, ]
unk.month.a.g <- unk.month.a.g[, list(sum(SPPLIVMT), sum(SPPVALUE)), 
                               by = c(match.key, 'QY', 'HY', 'UTILCD')]
setnames(unk.month.a.g, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

k.month.a.g <- k.month[GEAR != 'unknown', ]
k.month.a.g <- k.month.a.g[AREA != 0, ]
k.month.a.g <- k.month.a.g[, list(sum(SPPLIVMT), sum(SPPVALUE)), 
                           by = c(match.key, 'QY', 'HY', 'UTILCD')]
setnames(k.month.a.g, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

setkeyv(unk.month.a.g, match.key)
setkeyv(k.month.a.g,   match.key)

month.a.g <- k.month.a.g[unk.month.a.g]

#No match - need to match with larger aggregation
no.match  <- month.a.g[is.na(SPPLIVMT), ]
no.match[, c('QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match, c('i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
         c('QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
#Drop GEAR
setkey(no.match, YEAR, NESPP3, AREA)
setkeyv(k.month.a.g, key(no.match))
month.a.g.2 <- k.month.a.g[no.match]
no.match.2 <- month.a.g.2[is.na(SPPLIVMT), ]
no.match.2[, c('GEAR', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match.2, c('i.GEAR', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
         c('GEAR', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
#Drop AREA
setkey(no.match.2, YEAR, NESPP3)
setkeyv(k.month.a.g, key(no.match.2))
month.a.g.3 <- k.month.a.g[no.match.2]
no.match.3 <- month.a.g.3[is.na(SPPLIVMT), ]
no.match.3[, c('AREA', 'GEAR', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match.3, c('i.AREA', 'i.GEAR', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
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
  month.a.g <- rbindlist(list(month.a.g, month.a.g.2))  
}
month.a.g.3 <- month.a.g.3[!is.na(SPPLIVMT), ]
if(nrow(month.a.g.3) > 0){
  month.a.g.3[, AREA   := i.AREA]
  month.a.g.3[, GEAR   := i.GEAR]
  month.a.g.3[, i.AREA := NULL]
  month.a.g.3[, i.GEAR := NULL]
  setcolorder(month.a.g.3, names(month.a.g))
  month.a.g <- rbindlist(list(month.a.g, month.a.g.3))  
}

month.a.g[, prop := SPPLIVMT / sum(SPPLIVMT), by = match.key]
month.a.g[, unk  := i.SPPLIVMT * prop]
month.a.g[, unk2 := i.SPPVALUE * prop]
month.a.g[, c('SPPLIVMT', 'SPPVALUE', 'i.SPPLIVMT', 'i.SPPVALUE', 'i.HY', 
              'i.QY', 'i.UTILCD', 'prop') := NULL]
setnames(month.a.g, c('unk', 'unk2'), c('SPPLIVMT', 'SPPVALUE'))
month.a.g[, SIZE := factor('unknown', levels = c('large', 'small', 'unknown'))]

setcolorder(month.a.g,  names(month.solved))
setcolorder(no.match.3, names(month.solved))
month.solved <- rbindlist(list(month.solved, month.a.g, no.match.3))
rm(list = c(ls(pattern = 'month.a.g'), ls(pattern = 'no.match')))

#3.A.4 - AREA/TC
match.key <- c('YEAR', 'NESPP3', 'SIZE', 'AREA')

unk.month.a.s <- unk.month[GEAR == 'unknown']
unk.month.a.s <- unk.month.a.s[SIZE != 'unknown', ]
unk.month.a.s <- unk.month.a.s[AREA != 0, ]
unk.month.a.s <- unk.month.a.s[, list(sum(SPPLIVMT), sum(SPPVALUE)), 
                               by = c(match.key, 'QY', 'HY', 'UTILCD')]
setnames(unk.month.a.s, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

k.month.a.s <- k.month[SIZE != 'unknown', ]
k.month.a.s <- k.month.a.s[AREA != 0, ]
k.month.a.s <- k.month.a.s[, list(sum(SPPLIVMT), sum(SPPVALUE)), 
                           by = c(match.key, 'QY', 'HY', 'UTILCD')]
setnames(k.month.a.s, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

setkeyv(unk.month.a.s, match.key)
setkeyv(k.month.a.s,   match.key)

month.a.s <- k.month.a.s[unk.month.a.s]

#No match - need to match with larger aggregation
no.match  <- month.a.s[is.na(SPPLIVMT), ]
no.match[, c('QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match, c('i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
         c('QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
#Drop SIZE
setkey(no.match, YEAR, NESPP3, AREA)
setkeyv(k.month.a.s, key(no.match))
month.a.s.2 <- k.month.a.s[no.match]
no.match.2 <- month.a.s.2[is.na(SPPLIVMT), ]
no.match.2[, c('SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match.2, c('i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
         c('SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
#Drop AREA
setkey(no.match.2, YEAR, NESPP3)
setkeyv(k.month.a.s, key(no.match.2))
month.a.s.3 <- k.month.a.s[no.match.2]
no.match.3 <- month.a.s.3[is.na(SPPLIVMT), ]
no.match.3[, c('AREA', 'SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match.3, c('i.AREA', 'i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
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
  month.a.s <- rbindlist(list(month.a.s, month.a.s.2))
}
month.a.s.3 <- month.a.s.3[!is.na(SPPLIVMT), ]
if(nrow(month.a.s.3) > 0){
  month.a.s.3[, AREA   := i.AREA]
  month.a.s.3[, SIZE   := i.SIZE]
  month.a.s.3[, i.AREA := NULL]
  month.a.s.3[, i.SIZE := NULL]
  setcolorder(month.a.s.3, names(month.a.s))
  month.a.s <- rbindlist(list(month.a.s, month.a.s.3))  
}

month.a.s[, prop := SPPLIVMT / sum(SPPLIVMT), by = match.key]
month.a.s[, unk  := i.SPPLIVMT * prop]
month.a.s[, unk2 := i.SPPVALUE * prop]
month.a.s[, c('SPPLIVMT', 'SPPVALUE', 'i.SPPLIVMT', 'i.SPPVALUE', 'i.HY', 
              'i.QY', 'i.UTILCD', 'prop') := NULL]
setnames(month.a.s, c('unk', 'unk2'), c('SPPLIVMT', 'SPPVALUE'))
month.a.s[, GEAR := factor('unknown', levels = levels(k.month[, GEAR]))]

setcolorder(month.a.s,  names(month.solved))
setcolorder(no.match.3, names(month.solved))
month.solved <- rbindlist(list(month.solved, month.a.s, no.match.3))
rm(list = c(ls(pattern = 'month.a.s'), ls(pattern = 'no.match')))

#3.A.5 - SIZE
match.key <- c('YEAR', 'NESPP3', 'SIZE')

unk.month.si <- unk.month[GEAR == 'unknown']
unk.month.si <- unk.month.si[SIZE != 'unknown', ]
unk.month.si <- unk.month.si[AREA == 0, ]
unk.month.si <- unk.month.si[, list(sum(SPPLIVMT), sum(SPPVALUE)), 
                             by = c(match.key, 'QY', 'HY', 'UTILCD')]
setnames(unk.month.si, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

k.month.si <- k.month[SIZE != 'unknown', ]
k.month.si <- k.month.si[, list(sum(SPPLIVMT), sum(SPPVALUE)), 
                         by = c(match.key, 'QY', 'HY', 'UTILCD')]
setnames(k.month.si, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

setkeyv(unk.month.si, match.key)
setkeyv(k.month.si,   match.key)

month.si <- k.month.si[unk.month.si]

#No match - need to match with larger aggregation
no.match  <- month.si[is.na(SPPLIVMT), ]
no.match[, c('QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match, c('i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
         c('QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
#Drop SIZE
setkey(no.match, YEAR, NESPP3)
setkeyv(k.month.si, key(no.match))
month.si.2 <- k.month.si[no.match]
no.match.2 <- month.si.2[is.na(SPPLIVMT), ]
no.match.2[, c('SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match.2, c('i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
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
  month.si <- rbindlist(list(month.si, month.si.2))
}

month.si[, prop := SPPLIVMT / sum(SPPLIVMT), by = match.key]
month.si[, unk  := i.SPPLIVMT * prop]
month.si[, unk2 := i.SPPVALUE * prop]
month.si[, c('SPPLIVMT', 'SPPVALUE', 'i.SPPLIVMT', 'i.SPPVALUE', 'i.HY', 'i.QY', 
             'i.UTILCD', 'prop') := NULL]
setnames(month.si, c('unk', 'unk2'), c('SPPLIVMT', 'SPPVALUE'))
month.si[, AREA := 0]
month.si[, GEAR := factor('unknown', levels = levels(k.month[, GEAR]))]

setcolorder(month.si,  names(month.solved))
setcolorder(no.match.2, names(month.solved))
month.solved <- rbindlist(list(month.solved, month.si, no.match.2))
rm(list = c(ls(pattern = 'month.si'), ls(pattern = 'no.match')))

#3.A.6 - GEAR
match.key <- c('YEAR', 'NESPP3', 'GEAR')

unk.month.g <- unk.month[GEAR != 'unknown']
unk.month.g <- unk.month.g[SIZE == 'unknown', ]
unk.month.g <- unk.month.g[AREA == 0, ]
unk.month.g <- unk.month.g[, list(sum(SPPLIVMT), sum(SPPVALUE)), 
                           by = c(match.key, 'QY', 'HY', 'UTILCD')]
setnames(unk.month.g, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

k.month.g <- k.month[GEAR != 'unknown', ]
k.month.g <- k.month.g[, list(sum(SPPLIVMT), sum(SPPVALUE)), 
                       by = c(match.key, 'QY', 'HY', 'UTILCD')]
setnames(k.month.g, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

setkeyv(unk.month.g, match.key)
setkeyv(k.month.g,   match.key)

month.g <- k.month.g[unk.month.g]

#No match - need to match with larger aggregation
no.match  <- month.g[is.na(SPPLIVMT), ]
no.match[, c('QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match, c('i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
         c('QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
#Drop GEAR
setkey(no.match, YEAR, NESPP3)
setkeyv(k.month.g, key(no.match))
month.g.2 <- k.month.g[no.match]
no.match.2 <- month.g.2[is.na(SPPLIVMT), ]
no.match.2[, c('GEAR', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match.2, c('i.GEAR', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
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
  month.g <- rbindlist(list(month.g, month.g.2))
}

month.g[, prop := SPPLIVMT / sum(SPPLIVMT), by = match.key]
month.g[, unk  := i.SPPLIVMT * prop]
month.g[, unk2 := i.SPPVALUE * prop]
month.g[, c('SPPLIVMT', 'SPPVALUE', 'i.SPPLIVMT', 'i.SPPVALUE', 'i.HY', 'i.QY', 
            'i.UTILCD', 'prop') := NULL]
setnames(month.g, c('unk', 'unk2'), c('SPPLIVMT', 'SPPVALUE'))
month.g[, SIZE := factor('unknown', levels = c('large', 'small', 'unknown'))]
month.g[, AREA := 0]

setcolorder(month.g,  names(month.solved))
setcolorder(no.match.2, names(month.solved))
month.solved <- rbindlist(list(month.solved, month.g, no.match.2))
rm(list = c(ls(pattern = 'month.g'), ls(pattern = 'no.match')))

#3.A.7 - AREA
match.key <- c('YEAR', 'NESPP3', 'AREA')

unk.month.a <- unk.month[GEAR == 'unknown']
unk.month.a <- unk.month.a[SIZE == 'unknown', ]
unk.month.a <- unk.month.a[AREA != 0, ]
unk.month.a <- unk.month.a[, list(sum(SPPLIVMT), sum(SPPVALUE)), 
                           by = c(match.key, 'QY', 'HY', 'UTILCD')]
setnames(unk.month.a, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

k.month.a <- k.month[AREA != 0, ]
k.month.a <- k.month.a[, list(sum(SPPLIVMT), sum(SPPVALUE)), 
                       by = c(match.key, 'QY', 'HY', 'UTILCD')]
setnames(k.month.a, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

setkeyv(unk.month.a, match.key)
setkeyv(k.month.a,   match.key)

month.a <- k.month.a[unk.month.a]

#No match - need to match with larger aggregation
no.match  <- month.a[is.na(SPPLIVMT), ]
no.match[, c('QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match, c('i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
         c('QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
#Drop AREA
setkey(no.match, YEAR, NESPP3)
setkeyv(k.month.a, key(no.match))
month.a.2 <- k.month.a[no.match]
no.match.2 <- month.a.2[is.na(SPPLIVMT), ]
no.match.2[, c('AREA', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match.2, c('i.AREA', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
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
  month.a <- rbindlist(list(month.a, month.a.2))
}

month.a[, prop := SPPLIVMT / sum(SPPLIVMT), by = match.key]
month.a[, unk  := i.SPPLIVMT * prop]
month.a[, unk2 := i.SPPVALUE * prop]
month.a[, c('SPPLIVMT', 'SPPVALUE', 'i.SPPLIVMT', 'i.SPPVALUE', 'i.HY', 
            'i.QY', 'i.UTILCD', 'prop') := NULL]
setnames(month.a, c('unk','unk2'), c('SPPLIVMT', 'SPPVALUE'))
month.a[, SIZE := factor('unknown', levels = c('large', 'small', 'unknown'))]
month.a[, GEAR := factor('unknown', levels = levels(k.month[, GEAR]))]

setcolorder(month.a,  names(month.solved))
setcolorder(no.match.2, names(month.solved))
month.solved <- rbindlist(list(month.solved, month.a, no.match.2))
rm(list = c(ls(pattern = 'month.a'), ls(pattern = 'no.match')))

#3.A.8 - Species only - no other match
match.key <- c('YEAR', 'NESPP3')

unk.month.sp <- unk.month[GEAR == 'unknown']
unk.month.sp <- unk.month.sp[SIZE == 'unknown', ]
unk.month.sp <- unk.month.sp[AREA == 0, ]
unk.month.sp <- unk.month.sp[, list(sum(SPPLIVMT), sum(SPPVALUE)), 
                             by = c(match.key, 'QY', 'HY', 'UTILCD')]
setnames(unk.month.sp, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

k.month.sp <- k.month[, list(sum(SPPLIVMT), sum(SPPVALUE)), 
                      by = c(match.key, 'QY', 'HY', 'UTILCD')]
setnames(k.month.sp, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

setkeyv(unk.month.sp, match.key)
setkeyv(k.month.sp,   match.key)

month.sp <- k.month.sp[unk.month.sp]

#No match - assign to first QY/HY
no.match  <- month.sp[is.na(SPPLIVMT), ]
no.match[, c('QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match, c('i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
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
setnames(month.sp, c('unk','unk2'), c('SPPLIVMT', 'SPPVALUE'))
month.sp[, AREA := 0]
month.sp[, GEAR := factor('unknown', levels = levels(k.month[, GEAR]))]
month.sp[, SIZE := factor('unknown', levels = c('large', 'small', 'unknown'))]

setcolorder(month.sp,  names(month.solved))
setcolorder(no.match, names(month.solved))
month.solved <- rbindlist(list(month.solved, month.sp, no.match))
rm(list = c(ls(pattern = 'month.sp'), ls(pattern = 'no.match')))

#Merge back month.solved
setcolorder(month.solved, names(comland.agg))
comland.agg <- rbindlist(list(k.month, month.solved))
setkey(comland.agg,
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
setnames(comland.agg, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

#3.B SIZE------------------------------------------------------------------------------
unk.size <- comland.agg[SIZE == 'unknown', ]
k.size   <- comland.agg[SIZE != 'unknown', ]

#3.B.1 - All match
match.key <- c('YEAR', 'NESPP3', 'QY', 'HY', 'GEAR', 'AREA')

unk.size.all <- unk.size[GEAR != 'unknown']
unk.size.all <- unk.size.all[AREA != 0, ]

k.size.all <- k.size[GEAR != 'unknown', ]
k.size.all <- k.size.all[AREA != 0, ]

setkeyv(unk.size.all, match.key)
setkeyv(k.size.all,   match.key)

size.all <- k.size.all[unk.size.all]

#No match - need to match with larger aggregation
no.match  <- size.all[is.na(SPPLIVMT), ]
no.match[, c('SIZE', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match, c('i.SIZE', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
         c('SIZE', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
#Drop QY
setkey(no.match, YEAR, NESPP3, HY, GEAR, AREA)
setkeyv(k.size.all, key(no.match))
size.all.2 <- k.size.all[no.match]
no.match.2 <- size.all.2[is.na(SPPLIVMT), ]
no.match.2[, c('SIZE', 'QY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match.2, c('i.SIZE', 'i.QY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
         c('SIZE', 'QY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
#Drop HY
setkey(no.match.2, YEAR, NESPP3, GEAR, AREA)
setkeyv(k.size.all, key(no.match.2))
size.all.3 <- k.size.all[no.match.2]
no.match.3 <- size.all.3[is.na(SPPLIVMT), ]
no.match.3[, c('SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match.3, c('i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
         c('SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
#Drop GEAR
setkey(no.match.3, YEAR, NESPP3, AREA)
setkeyv(k.size.all, key(no.match.3))
size.all.4 <- k.size.all[no.match.3]
no.match.4 <- size.all.4[is.na(SPPLIVMT), ]
no.match.4[, c('GEAR', 'SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match.4, c('i.GEAR', 'i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
         c('GEAR',   'SIZE',   'QY',   'HY',  'UTILCD',  'SPPLIVMT', 'SPPVALUE'))
#Drop AREA
setkey(no.match.4, YEAR, NESPP3)
setkeyv(k.size.all, key(no.match.4))
size.all.5 <- k.size.all[no.match.4]
no.match.5 <- size.all.5[is.na(SPPLIVMT), ]
no.match.5[, c('AREA', 'GEAR', 'SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match.5, c('i.AREA', 'i.GEAR', 'i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
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

size.all <- rbindlist(list(size.all, size.all.2, size.all.3, 
                           size.all.4, size.all.5))

size.all[, prop := SPPLIVMT / sum(SPPLIVMT), by = match.key]
size.all[, unk  := i.SPPLIVMT * prop]
size.all[, unk2 := i.SPPVALUE * prop]
size.all[, c('SPPLIVMT', 'SPPVALUE', 'i.SPPLIVMT', 'i.SPPVALUE', 'i.SIZE', 
             'i.UTILCD', 'prop') := NULL]
setnames(size.all, c('unk','unk2'), c('SPPLIVMT', 'SPPVALUE'))

setcolorder(no.match.5, names(size.all))
size.solved <- rbindlist(list(size.all, no.match.5))
rm(list = c(ls(pattern = 'size.all'), ls(pattern = 'no.match')))

#3.B.2 - GEAR
match.key <- c('YEAR', 'NESPP3', 'QY', 'HY', 'GEAR')

unk.size.g <- unk.size[GEAR != 'unknown']
unk.size.g <- unk.size.g[AREA == 0, ]
unk.size.g[, AREA := NULL]

k.size.g <- k.size[GEAR != 'unknown', ]
k.size.g <- k.size.g[, list(sum(SPPLIVMT), sum(SPPVALUE)), 
                     by = c(match.key, 'SIZE', 'UTILCD')]
setnames(k.size.g, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

setkeyv(unk.size.g, match.key)
setkeyv(k.size.g,   match.key)

size.g <- k.size.g[unk.size.g]

#No match - need to match with larger aggregation
no.match  <- size.g[is.na(SPPLIVMT), ]
no.match[, c('SIZE', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match, c('i.SIZE', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
         c('SIZE', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
#Drop QY
setkey(no.match, YEAR, NESPP3, HY, GEAR)
setkeyv(k.size.g, key(no.match))
size.g.2 <- k.size.g[no.match]
no.match.2 <- size.g.2[is.na(SPPLIVMT), ]
no.match.2[, c('SIZE', 'QY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match.2, c('i.SIZE', 'i.QY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
         c('SIZE', 'QY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
#Drop HY
setkey(no.match.2, YEAR, NESPP3, GEAR)
setkeyv(k.size.g, key(no.match.2))
size.g.3 <- k.size.g[no.match.2]
no.match.3 <- size.g.3[is.na(SPPLIVMT), ]
no.match.3[, c('SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match.3, c('i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
         c('SIZE',   'QY',   'HY',  'UTILCD',  'SPPLIVMT', 'SPPVALUE'))
#Drop GEAR
setkey(no.match.3, YEAR, NESPP3)
setkeyv(k.size.g, key(no.match.3))
size.g.4 <- k.size.g[no.match.3]
no.match.4 <- size.g.4[is.na(SPPLIVMT), ]
no.match.4[, c('GEAR', 'SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match.4, c('i.GEAR', 'i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
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

size.g <- rbindlist(list(size.g, size.g.2, size.g.3, size.g.4))

size.g[, prop := SPPLIVMT / sum(SPPLIVMT), by = match.key]
size.g[, unk  := i.SPPLIVMT * prop]
size.g[, unk2 := i.SPPVALUE * prop]
size.g[, c('SPPLIVMT', 'SPPVALUE', 'i.SPPLIVMT', 'i.SPPVALUE', 'i.SIZE', 
           'i.UTILCD', 'prop') := NULL]
setnames(size.g, c('unk','unk2'), c('SPPLIVMT', 'SPPVALUE'))
size.g[, AREA := 0]

setcolorder(size.g,     names(size.solved))
setcolorder(no.match.4, names(size.g))
size.solved <- rbindlist(list(size.solved, size.g, no.match.4))
rm(list = c(ls(pattern = 'size.g'), ls(pattern = 'no.match')))         

#3.B.3 - AREA
match.key <- c('YEAR', 'NESPP3', 'QY', 'HY', 'AREA')

unk.size.a <- unk.size[GEAR == 'unknown']
unk.size.a <- unk.size.a[AREA != 0, ]
unk.size.a[, GEAR := NULL]

k.size.a <- k.size[AREA != 0, ]
k.size.a <- k.size.a[, list(sum(SPPLIVMT), sum(SPPVALUE)), 
                     by = c(match.key, 'SIZE', 'UTILCD')]
setnames(k.size.a, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

setkeyv(unk.size.a, match.key)
setkeyv(k.size.a,   match.key)

size.a <- k.size.a[unk.size.a]

#No match - need to match with larger aggregation
no.match  <- size.a[is.na(SPPLIVMT), ]
no.match[, c('SIZE', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match, c('i.SIZE', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
         c('SIZE', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
#Drop QY
setkey(no.match, YEAR, NESPP3, HY, AREA)
setkeyv(k.size.a, key(no.match))
size.a.2 <- k.size.a[no.match]
no.match.2 <- size.a.2[is.na(SPPLIVMT), ]
no.match.2[, c('SIZE', 'QY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match.2, c('i.SIZE', 'i.QY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
         c('SIZE', 'QY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
#Drop HY
setkey(no.match.2, YEAR, NESPP3, AREA)
setkeyv(k.size.a, key(no.match.2))
size.a.3 <- k.size.a[no.match.2]
no.match.3 <- size.a.3[is.na(SPPLIVMT), ]
no.match.3[, c('SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match.3, c('i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
         c('SIZE',   'QY',   'HY',  'UTILCD',  'SPPLIVMT', 'SPPVALUE'))
#Drop AREA
setkey(no.match.3, YEAR, NESPP3)
setkeyv(k.size.a, key(no.match.3))
size.a.4 <- k.size.a[no.match.3]
no.match.4 <- size.a.4[is.na(SPPLIVMT), ]
no.match.4[, c('AREA', 'SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match.4, c('i.AREA', 'i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
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

size.a <- rbindlist(list(size.a, size.a.2, size.a.3, size.a.4))

size.a[, prop := SPPLIVMT / sum(SPPLIVMT), by = match.key]
size.a[, unk  := i.SPPLIVMT * prop]
size.a[, unk2 := i.SPPVALUE * prop]
size.a[, c('SPPLIVMT', 'SPPVALUE', 'i.SPPLIVMT', 'i.SPPVALUE', 'i.SIZE', 
           'i.UTILCD', 'prop') := NULL]
setnames(size.a, c('unk','unk2'), c('SPPLIVMT', 'SPPVALUE'))
size.a[, GEAR := factor('unknown', levels = levels(k.size[, GEAR]))]

setcolorder(size.a,     names(size.solved))
setcolorder(no.match.4, names(size.a))
size.solved <- rbindlist(list(size.solved, size.a, no.match.4))
rm(list = c(ls(pattern = 'size.a'), ls(pattern = 'no.match')))         

#3.B.4 - Species only - no other match
match.key <- c('YEAR', 'NESPP3', 'QY', 'HY')

unk.size.sp <- unk.size[GEAR == 'unknown']
unk.size.sp <- unk.size.sp[SIZE == 'unknown', ]
unk.size.sp <- unk.size.sp[AREA == 0, ]
unk.size.sp[, c('GEAR', 'AREA') := NULL]

k.size.sp <- k.size[, list(sum(SPPLIVMT), sum(SPPVALUE)), 
                    by = c(match.key, 'SIZE', 'UTILCD')]
setnames(k.size.sp, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

setkeyv(unk.size.sp, match.key)
setkeyv(k.size.sp,   match.key)

size.sp <- k.size.sp[unk.size.sp]

#No match - need to match with larger aggregation
no.match  <- size.sp[is.na(SPPLIVMT), ]
no.match[, c('SIZE', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match, c('i.SIZE', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
         c('SIZE', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
#Drop QY
setkey(no.match, YEAR, NESPP3, HY)
setkeyv(k.size.sp, key(no.match))
size.sp.2 <- k.size.sp[no.match]
no.match.2 <- size.sp.2[is.na(SPPLIVMT), ]
no.match.2[, c('SIZE', 'QY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match.2, c('i.SIZE', 'i.QY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
         c('SIZE', 'QY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
#Drop HY
setkey(no.match.2, YEAR, NESPP3)
setkeyv(k.size.sp, key(no.match.2))
size.sp.3 <- k.size.sp[no.match.2]
no.match.3 <- size.sp.3[is.na(SPPLIVMT), ]
no.match.3[, c('SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match.3, c('i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
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

size.sp <- rbindlist(list(size.sp, size.sp.2, size.sp.3))

size.sp[, prop := SPPLIVMT / sum(SPPLIVMT), by = match.key]
size.sp[, unk  := i.SPPLIVMT * prop]
size.sp[, unk2 := i.SPPVALUE * prop]
size.sp[, c('SPPLIVMT', 'SPPVALUE', 'i.SPPLIVMT', 'i.SPPVALUE', 'i.SIZE', 
            'i.UTILCD', 'prop') := NULL]
setnames(size.sp, c('unk','unk2'), c('SPPLIVMT', 'SPPVALUE'))
size.sp[, AREA := 0]
size.sp[, GEAR := factor('unknown', levels = levels(k.size[, GEAR]))]

setcolorder(size.sp,    names(size.solved))
setcolorder(no.match.3, names(size.solved))
size.solved <- rbindlist(list(size.solved, size.sp, no.match.3))
rm(list = c(ls(pattern = 'size.sp'), ls(pattern = 'no.match')))

#Merge back size.solved
setcolorder(size.solved, names(comland.agg))
comland.agg <- rbindlist(list(k.size, size.solved))
setkey(comland.agg,
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
setnames(comland.agg, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

#3.C GEAR------------------------------------------------------------------------------
unk.gear <- comland.agg[GEAR == 'unknown', ]
k.gear   <- comland.agg[GEAR != 'unknown', ]

#3.C.1 - All match
match.key <- c('YEAR', 'NESPP3', 'QY', 'HY', 'SIZE', 'AREA')

unk.gear.all <- unk.gear[AREA != 0, ]

k.gear.all <- k.gear[AREA != 0, ]

setkeyv(unk.gear.all, match.key)
setkeyv(k.gear.all,   match.key)

gear.all <- k.gear.all[unk.gear.all]

#No match - need to match with larger aggregation
no.match  <- gear.all[is.na(SPPLIVMT), ]
no.match[, c('GEAR', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match, c('i.GEAR', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
         c('GEAR', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
#Drop QY
setkey(no.match, YEAR, NESPP3, HY, SIZE, AREA)
setkeyv(k.gear.all, key(no.match))
gear.all.2 <- k.gear.all[no.match]
no.match.2 <- gear.all.2[is.na(SPPLIVMT), ]
no.match.2[, c('GEAR', 'QY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match.2, c('i.GEAR', 'i.QY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
         c('GEAR', 'QY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
#Drop HY
setkey(no.match.2, YEAR, NESPP3, SIZE, AREA)
setkeyv(k.gear.all, key(no.match.2))
gear.all.3 <- k.gear.all[no.match.2]
no.match.3 <- gear.all.3[is.na(SPPLIVMT), ]
no.match.3[, c('GEAR', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match.3, c('i.GEAR', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
         c('GEAR', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
#Drop SIZE
setkey(no.match.3, YEAR, NESPP3, AREA)
setkeyv(k.gear.all, key(no.match.3))
gear.all.4 <- k.gear.all[no.match.3]
no.match.4 <- gear.all.4[is.na(SPPLIVMT), ]
no.match.4[, c('GEAR', 'SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match.4, c('i.GEAR', 'i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
         c('GEAR',   'SIZE',   'QY',   'HY',  'UTILCD',  'SPPLIVMT', 'SPPVALUE'))
#Drop AREA
setkey(no.match.4, YEAR, NESPP3)
setkeyv(k.gear.all, key(no.match.4))
gear.all.5 <- k.gear.all[no.match.4]
no.match.5 <- gear.all.5[is.na(SPPLIVMT), ]
no.match.5[, c('AREA', 'GEAR', 'SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match.5, c('i.AREA', 'i.GEAR', 'i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD',
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

gear.all <- rbindlist(list(gear.all, gear.all.2, gear.all.3, 
                           gear.all.4, gear.all.5))

gear.all[, prop := SPPLIVMT / sum(SPPLIVMT), by = match.key]
gear.all[, unk  := i.SPPLIVMT * prop]
gear.all[, unk2 := i.SPPVALUE * prop]
gear.all[, c('SPPLIVMT', 'SPPVALUE', 'i.SPPLIVMT', 'i.SPPVALUE', 'i.GEAR', 
             'i.UTILCD', 'prop') := NULL]
setnames(gear.all, c('unk','unk2'), c('SPPLIVMT', 'SPPVALUE'))

setcolorder(no.match.5, names(gear.all))
gear.solved <- rbindlist(list(gear.all, no.match.5))
rm(list = c(ls(pattern = 'gear.all'), ls(pattern = 'no.match')))

#3.C.2 - Species only - no other match
match.key <- c('YEAR', 'NESPP3', 'QY', 'HY', 'SIZE')

unk.gear.sp <- unk.gear[AREA == 0, ]
unk.gear.sp[, 'AREA' := NULL]

k.gear.sp <- k.gear[, list(sum(SPPLIVMT), sum(SPPVALUE)), 
                    by = c(match.key, 'GEAR', 'UTILCD')]
setnames(k.gear.sp, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

setkeyv(unk.gear.sp, match.key)
setkeyv(k.gear.sp,   match.key)

gear.sp <- k.gear.sp[unk.gear.sp]

#No match - need to match with larger aggregation
no.match  <- gear.sp[is.na(SPPLIVMT), ]
no.match[, c('GEAR', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match, c('i.GEAR', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
         c('GEAR', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
#Drop QY
setkey(no.match, YEAR, NESPP3, HY, SIZE)
setkeyv(k.gear.sp, key(no.match))
gear.sp.2 <- k.gear.sp[no.match]
no.match.2 <- gear.sp.2[is.na(SPPLIVMT), ]
no.match.2[, c('GEAR', 'QY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match.2, c('i.GEAR', 'i.QY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
         c('GEAR', 'QY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
#Drop HY
setkey(no.match.2, YEAR, NESPP3, SIZE)
setkeyv(k.gear.sp, key(no.match.2))
gear.sp.3 <- k.gear.sp[no.match.2]
no.match.3 <- gear.sp.3[is.na(SPPLIVMT), ]
no.match.3[, c('GEAR', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match.3, c('i.GEAR', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
         c('GEAR', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
#Drop SIZE
setkey(no.match.3, YEAR, NESPP3)
setkeyv(k.gear.sp, key(no.match.3))
gear.sp.4 <- k.gear.sp[no.match.3]
no.match.4 <- gear.sp.4[is.na(SPPLIVMT), ]
no.match.4[, c('GEAR', 'SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match.4, c('i.GEAR', 'i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
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

gear.sp <- rbindlist(list(gear.sp, gear.sp.2, gear.sp.3, gear.sp.4))

gear.sp[, prop := SPPLIVMT / sum(SPPLIVMT), by = match.key]
gear.sp[, unk  := i.SPPLIVMT * prop]
gear.sp[, unk2 := i.SPPVALUE * prop]
gear.sp[, c('SPPLIVMT', 'SPPVALUE', 'i.SPPLIVMT', 'i.SPPVALUE', 'i.GEAR', 
            'i.UTILCD', 'prop') := NULL]
setnames(gear.sp, c('unk','unk2'), c('SPPLIVMT', 'SPPVALUE'))
gear.sp[, AREA := 0]

setcolorder(gear.sp,    names(gear.solved))
setcolorder(no.match.4, names(gear.solved))
gear.solved <- rbindlist(list(gear.solved, gear.sp, no.match.4))
rm(list = c(ls(pattern = 'gear.sp'), ls(pattern = 'no.match')))

#Merge back gear.solved
setcolorder(gear.solved, names(comland.agg))
comland.agg <- rbindlist(list(k.gear, gear.solved))
setkey(comland.agg,
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
setnames(comland.agg, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

#3.D AREA------------------------------------------------------------------------------
unk.area <- comland.agg[AREA == 0, ]
k.area   <- comland.agg[AREA != 0, ]

#3.C.1 - All match
match.key <- c('YEAR', 'NESPP3', 'QY', 'HY', 'SIZE', 'GEAR')

unk.area.all <- unk.area

k.area.all <- k.area

setkeyv(unk.area.all, match.key)
setkeyv(k.area.all,   match.key)

area.all <- k.area.all[unk.area.all]

#No match - need to match with larger aggregation
no.match  <- area.all[is.na(SPPLIVMT), ]
no.match[, c('AREA', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match, c('i.AREA', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
         c('AREA', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
#Drop QY
setkey(no.match, YEAR, NESPP3, HY, SIZE, GEAR)
setkeyv(k.area.all, key(no.match))
area.all.2 <- k.area.all[no.match]
no.match.2 <- area.all.2[is.na(SPPLIVMT), ]
no.match.2[, c('AREA', 'QY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match.2, c('i.AREA', 'i.QY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
         c('AREA', 'QY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
#Drop HY
setkey(no.match.2, YEAR, NESPP3, SIZE, GEAR)
setkeyv(k.area.all, key(no.match.2))
area.all.3 <- k.area.all[no.match.2]
no.match.3 <- area.all.3[is.na(SPPLIVMT), ]
no.match.3[, c('AREA', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match.3, c('i.AREA', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
         c('AREA', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
#Drop SIZE
setkey(no.match.3, YEAR, NESPP3, GEAR)
setkeyv(k.area.all, key(no.match.3))
area.all.4 <- k.area.all[no.match.3]
no.match.4 <- area.all.4[is.na(SPPLIVMT), ]
no.match.4[, c('AREA', 'SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match.4, c('i.AREA', 'i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
         c('AREA',   'SIZE',   'QY',   'HY',  'UTILCD',  'SPPLIVMT', 'SPPVALUE'))
#Drop GEAR
setkey(no.match.4, YEAR, NESPP3)
setkeyv(k.area.all, key(no.match.4))
area.all.5 <- k.area.all[no.match.4]
no.match.5 <- area.all.5[is.na(SPPLIVMT), ]
no.match.5[, c('AREA', 'GEAR', 'SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match.5, c('i.AREA', 'i.GEAR', 'i.SIZE', 'i.QY', 'i.HY', 'i.UTILCD', 
                       'i.SPPLIVMT', 'i.SPPVALUE'), 
         c('AREA', 'GEAR', 'SIZE', 'QY', 'HY', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
#Still no match - use 3 or 5 year window then drop year
years <- unique(no.match.5[, YEAR], by = key(no.match.5))
no.match.6 <- c()
area.all.6 <- c()
for(i in 1:length(years)){
  #3 year window
  k.area.3y <- comland.agg[AREA != 0 & YEAR %in% (years[i] - 1):(years[i] + 1), ]
  setkey(k.area.3y, NESPP3, AREA)
  k.area.3y <- k.area.3y[, list(sum(SPPLIVMT), sum(SPPVALUE)), 
                         by = c(key(k.area.3y), 'UTILCD')]
  setnames(k.area.3y, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))
  
  unk.area.3y <- no.match.5[YEAR == years[i], ]
  
  setkey(unk.area.3y, NESPP3)
  setkey(k.area.3y,   NESPP3)
  area.3y <- k.area.3y[unk.area.3y]
  
  no.match.3y <- area.3y[is.na(SPPLIVMT), ]
  no.match.3y[, c('AREA', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  setnames(no.match.3y, c('i.AREA', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
           c('AREA', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  no.match.6 <- rbindlist(list(no.match.6, no.match.3y))
  area.all.6 <- rbindlist(list(area.all.6, area.3y))
}

years <- unique(no.match.6[, YEAR], by = key(no.match.6))  
no.match.7 <- c()
area.all.7 <- c()  
for(i in 1:length(years)){    
  #5 year window
  k.area.5y <- comland.agg[AREA != 0 & YEAR %in% (years[i] - 2):(years[i] + 2), ]
  setkey(k.area.5y, NESPP3, AREA)
  k.area.5y <- k.area.5y[, list(sum(SPPLIVMT), sum(SPPVALUE)), 
                         by = c(key(k.area.5y), 'UTILCD')]
  setnames(k.area.5y, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))
  
  unk.area.5y <- no.match.6[YEAR == years[i], ]
  
  setkey(unk.area.5y, NESPP3)
  setkey(k.area.5y,   NESPP3)
  area.5y <- k.area.5y[unk.area.5y] 
  
  no.match.5y <- area.5y[is.na(SPPLIVMT), ]
  no.match.5y[, c('AREA', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
  setnames(no.match.5y, c('i.AREA', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
           c('AREA', 'UTILCD', 'SPPLIVMT', 'SPPVALUE'))
  
  no.match.7 <- rbindlist(list(no.match.7, no.match.5y))
  area.all.7 <- rbindlist(list(area.all.7, area.5y))
}   
#Drop year
setkey(no.match.7, NESPP3)
k.area.all <- k.area.all[, list(sum(SPPLIVMT), sum(SPPVALUE)), 
                         by = c('NESPP3', 'AREA', 'UTILCD')]
setnames(k.area.all, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))
setkey(k.area.all, NESPP3)

area.all.8 <- k.area.all[no.match.7]
no.match.8 <- area.all.8[is.na(SPPLIVMT), ]
no.match.8[, c('AREA', 'UTILCD', 'SPPLIVMT', 'SPPVALUE') := NULL]
setnames(no.match.8, c('i.AREA', 'i.UTILCD', 'i.SPPLIVMT', 'i.SPPVALUE'), 
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

area.all <- rbindlist(list(area.all,   area.all.2, area.all.3, area.all.4, 
                           area.all.5, area.all.6, area.all.7, area.all.8))

area.all[, prop := SPPLIVMT / sum(SPPLIVMT), by = match.key]
area.all[, unk  := i.SPPLIVMT * prop]
area.all[, unk2 := i.SPPVALUE * prop]
area.all[, c('SPPLIVMT', 'SPPVALUE', 'i.SPPLIVMT', 'i.SPPVALUE', 'i.AREA', 
             'i.UTILCD', 'prop') := NULL]
setnames(area.all, c('unk','unk2'), c('SPPLIVMT', 'SPPVALUE'))

setcolorder(no.match.8, names(area.all))
area.solved <- rbindlist(list(area.all, no.match.8))
rm(list = c(ls(pattern = 'area.all'), ls(pattern = 'no.match')))

#Merge back area.solved
setcolorder(area.solved, names(comland.agg))
comland.agg <- rbindlist(list(k.area, area.solved))
setkey(comland.agg,
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
setnames(comland.agg, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))  

#Agg old way----
#Assign EPU based on statarea
gom<-c(500, 510, 512:515)
gb<-c(521:526, 551, 552, 561, 562)
mab<-c(537, 539, 600, 612:616, 621, 622, 625, 626, 631, 632)
ss<-c(463:467, 511)

comland.agg[AREA %in% gom, EPU := 'GOM']
comland.agg[AREA %in% gb,  EPU := 'GB']
comland.agg[AREA %in% mab, EPU := 'MAB']
comland.agg[AREA %in% ss,  EPU := 'SS']
comland.agg[is.na(EPU),    EPU := 'OTHER']
comland.agg[, EPU := factor(EPU, levels = c('GOM', 'GB', 'MAB', 'SS', 'OTHER'))]

setkey(comland.agg,
       YEAR,
       NESPP3,
       QY,
       GEAR,
       SIZE,
       EPU,
       UTILCD)

comland.agg <- comland.agg[, list(sum(SPPLIVMT), sum(SPPVALUE)), by = key(comland.agg)]

setnames(comland.agg, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

#Fix foreign the old way----
#Note - NAFO landings by division only so not available in sum.by = "stat.area"
#Add NAFO foreign landings - Data from http://www.nafo.int/data/frames/data.html
temp <- tempfile()
download.file("https://www.nafo.int/Portals/0/Stats/nafo-21b-60-69.zip",temp)
nafo.60 <- as.data.table(read.csv(unz(temp, "NAFO21B-60-69.txt")))
unlink(temp)
download.file("https://www.nafo.int/Portals/0/Stats/nafo-21b-70-79.zip",temp)
nafo.70 <- as.data.table(read.csv(unz(temp, "NAFO21B-70-79.txt")))
unlink(temp)
download.file("https://www.nafo.int/Portals/0/Stats/nafo-21b-80-89.zip",temp)
nafo.80 <- as.data.table(read.csv(unz(temp, "NAFO21B-80-89.txt")))
unlink(temp)
download.file("https://www.nafo.int/Portals/0/Stats/nafo-21b-90-99.zip",temp)
nafo.90 <- as.data.table(read.csv(unz(temp, "NAFO21B-90-99.txt")))
unlink(temp)
download.file("https://www.nafo.int/Portals/0/Stats/nafo-21b-2000-09.zip",temp)
nafo.00 <- as.data.table(read.csv(unz(temp, "NAFO21B-2000-09.txt")))
unlink(temp)
download.file("https://www.nafo.int/Portals/0/Stats/nafo-21b-2010-18.zip",temp)
nafo.10 <- as.data.table(read.csv(unz(temp, "NAFO-21B-2010-18/NAFO-21b-2010-18.txt")))
unlink(temp)

#2010 + data have different column headers
setnames(nafo.10,
         c('Gear', 'AreaCode', 'SpeciesEffort'),
         c('GearCode', 'Divcode', 'Code'))

nafo <- rbindlist(list(nafo.60, nafo.70, nafo.80, nafo.90, nafo.00, nafo.10), fill = T)

#Remove US landings (Country code 22), extra divisions (use only 47, 51:56, 61:63),
#and effort codes (1:3)
nafo <- nafo[Country != 22 & Divcode %in% c(47, 51:56, 61:63) & Code > 3, ]

#Deal with unknown monthly catch?????

#Get nafo code in a similar format to comland
nafoland <- nafo[, list(Year, GearCode, Tonnage, Divcode, Code, Catches)]
nafoland[, MONTH := 0]
setnames(nafoland, 'Catches', 'SPPLIVMT')

month <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
for(i in 1:12){
  nafoland.month <- nafo[, list(Year, GearCode, Tonnage, Divcode, Code, get(month[i]))]
  nafoland.month[, MONTH := i]
  setnames(nafoland.month,
           names(nafoland.month)[6],
           'SPPLIVMT')
  nafoland <- rbindlist(list(nafoland, nafoland.month))
}

nafoland[, SPPLIVMT := as.numeric(SPPLIVMT)]

nafoland <- nafoland[SPPLIVMT != 0,]

nafoland[, EPU := factor(NA, levels = c('GOM', 'GB', 'MAB', 'SS', 'OTHER'))]
nafoland[Divcode == 47,             EPU := 'SS']
nafoland[Divcode == 51,             EPU := 'GOM']
nafoland[Divcode %in% c(52, 54:56), EPU := 'GB']
nafoland[Divcode %in% c(53, 61:63), EPU := 'MAB']
nafoland[is.na(EPU),                EPU := 'OTHER']

nafoland[, Divcode := NULL]

##Fix missing Scotian Shelf data from 21B
SS.nafo <- as.data.table(read.csv(here::here('data_raw', "SS_NAFO_21A.csv"), skip = 8))

#Add NAFOSPP code to SS.nafo
nafo.spp <- as.data.table(read.csv(here::here('data_raw', 'species.txt')))
setnames(nafo.spp, "Abbreviation", "Species_ASFIS")
nafo.spp <- nafo.spp[, list(Code, Species_ASFIS)]

SS.nafo <- merge(SS.nafo, nafo.spp, by = 'Species_ASFIS', all.x = T)

#Only grab missing data
SS.nafo <- SS.nafo[Year %in% c(2003, 2008, 2009), ]

setkey(SS.nafo,
       Year,
       Code)

SS.land <- SS.nafo[, sum(Catch...000.Kg.), by = key(SS.nafo)]

setnames(SS.land, "V1", "SPPLIVMT")

#Add GearCode, Tonnage, Month, and EPU 
SS.land[, GearCode := 99]
SS.land[, Tonnage  := 0]
SS.land[, MONTH    := 0]
SS.land[, EPU      := 'SS']

setcolorder(SS.land, names(nafoland))

nafoland <- rbindlist(list(nafoland, SS.land))

#Rectify NAFO codes with US codes
#Species
setnames(nafoland,
         c('Year', 'GearCode', 'Tonnage', 'Code'),
         c('YEAR', 'NAFOGEAR', 'TONCL1', 'NAFOSPP'))

spp <- as.data.table(DBI::dbGetQuery(channelSole, "select NAFOSPP, NESPP3 from CFSPP"))

#Fix missing NAFO codes
missing.spp <- data.table(NAFOSPP = c(110, 141, 189, 480, 484, 487, 488, 489),
                          NESPP3  = c(240, 509, 512, 366, 368, 367, 370, 369))
spp <- rbindlist(list(spp, missing.spp))

setkey(spp, NAFOSPP)
spp <- unique(spp, by = key(spp))

#Fix many to one relationships
spp[NAFOSPP == 199, NESPP3 := 524]
spp[NAFOSPP == 299, NESPP3 := 525]
spp[NAFOSPP == 469, NESPP3 := 359]
spp[NAFOSPP == 499, NESPP3 := 526]
spp[NAFOSPP == 529, NESPP3 := 764]
spp[NAFOSPP == 699, NESPP3 := 899]

spp[, NAFOSPP := as.numeric(NAFOSPP)]
nafoland <- merge(nafoland, spp, by = 'NAFOSPP', all.x = T)

#fix codes
nafoland[NAFOSPP == 309, NESPP3 := 150L]
nafoland[NAFOSPP == 462, NESPP3 := 481L]
nafoland[NAFOSPP == 464, NESPP3 := 355L]
nafoland[NAFOSPP == 468, NESPP3 := 493L]
nafoland[NAFOSPP == 704, NESPP3 := 817L]

#remove species without a match
nafoland <- nafoland[!is.na(NESPP3), ]

#Remove herring catch - already included from Maine Data earlier
nafoland <- nafoland[NESPP3 != 168, ]

#Gearcodes
gear <- as.data.table(DBI::dbGetQuery(channelSole, "select NEGEAR, NAFOGEAR from Gear"))
gear <- unique(gear, by = 'NAFOGEAR')

gear[, NAFOGEAR := as.numeric(NAFOGEAR)]
nafoland <- merge(nafoland, gear, by = 'NAFOGEAR', all.x = T)

#fix codes
nafoland[NAFOGEAR == 8,  NEGEAR := 50L]
nafoland[NAFOGEAR == 9,  NEGEAR := 370L]
nafoland[NAFOGEAR == 19, NEGEAR := 58L]
nafoland[NAFOGEAR == 49, NEGEAR := 60L]
nafoland[NAFOGEAR == 56, NEGEAR := 21L]

#Tonnage
nafoland[TONCL1 == 7, TONCL1 := 6L]

#Drop NAFO codes
nafoland[, c('NAFOGEAR', 'NAFOSPP') := NULL]


#aggregate nafo landings
#2 - aggregate by quarter year, half year, major gear, and small/large TC
nafoland[MONTH %in% 1:3,   QY := 1]
nafoland[MONTH %in% 4:6,   QY := 2]
nafoland[MONTH %in% 7:9,   QY := 3]
nafoland[MONTH %in% 10:12, QY := 4]
nafoland[MONTH == 0,       QY := 1]

nafoland[NEGEAR %in% otter,     GEAR := 'otter']
nafoland[NEGEAR %in% dredge.sc, GEAR := 'dredge.sc']
nafoland[NEGEAR %in% pot,       GEAR := 'pot']
nafoland[NEGEAR %in% longline,  GEAR := 'longline']
nafoland[NEGEAR %in% seine,     GEAR := 'seine']
nafoland[NEGEAR %in% gillnet,   GEAR := 'gillnet']
nafoland[NEGEAR %in% midwater,  GEAR := 'midwater']
nafoland[NEGEAR %in% dredge.o,  GEAR := 'dredge.o']
nafoland[NEGEAR == 99,          GEAR := 'unknown']
nafoland[is.na(GEAR),           GEAR := 'other']
nafoland[, GEAR := as.factor(GEAR)]

nafoland[TONCL1 %in% 1:3, SIZE := 'small']
nafoland[TONCL1 > 3,      SIZE := 'large']
nafoland[TONCL1 == 0,     SIZE := 'unknown']
nafoland[, SIZE := as.factor(SIZE)]

setkey(nafoland,
       YEAR,
       QY,
       GEAR,
       SIZE,
       EPU,
       NESPP3)


nafoland.agg <- nafoland[, sum(SPPLIVMT), by = key(nafoland)]

setnames(nafoland.agg, "V1", "SPPLIVMT")

#Create dummy variable for value
nafoland.agg[, SPPVALUE := 0]
nafoland.agg[, UTILCD := 0]

#Merge comland and nafoland
setcolorder(nafoland.agg, names(comland.agg))       


comland.agg[,  US := T]
nafoland.agg[, US := F]


comland.nafo <- rbindlist(list(comland.agg, nafoland.agg))




comland$comland <- comland.nafo



#Remove menhaden
comland$comland <- comland$comland[NESPP3 != 221, ]

#Save output
save(comland, file = here::here('data_raw', 'comland_oldway.rda'))


