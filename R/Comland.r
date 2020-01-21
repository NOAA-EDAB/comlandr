#' Comland.r Version now controlled by git - originally part of comcatch.r
#'
#'#Grab commercial landings data from US and Foreign countries (NAFO)
#'Need to fix menhaden data
#'SML
#'
#'@param channel an Object inherited from \link[DBI]{DBIConnection-class}. This object is used to connect
#' to communicate with the database engine. (see \code{\link{connect_to_database}})
#'@param EPUS List. Designates the stat areas that comprise an EPU. Default = EPUs (lazily loaded data)
#'@param GEARS List. Designates the NEGEAR codes that comprise a fishing fleet. Default = GEARs (lazily loaded data)
#'@param use.existing String. Pull from database "n" or use existing pull "y" (saves time) . Default = "y"
#'@param landed Character String. Use landed weight for scallops and clams instead of live weight. Default = "y"
#'@param foreign Character String. Mark foreign landings and keep seperate. Default = "y"
#'@param adjust.ppi Character String. Adjust value for inflation. Default = "y"
#'@param sum.by Character String. Variable to sum landings by either "EPU" (Default) or "stat.area"
#'@param endyear Numeric Scalar. Final year of query. Default = 2018
#'@param reftime Numeric Vector. (Length 2). Specifies the year and month if adjusting for inflation. Default = c(2016,1)
#'@param out.dir Character string. Path to directory where final output will be saved or where data is to be read from
#'@param Stand.alone Boolean. Flag to determine whether to save Skate and hake data to file. defualt = F (Both a US catch file and a NAFO catch file will be saved)
#'
#'@importFrom data.table ":=" "key" "setcolorder" "as.data.table"
#'
#'@export
#'
#Make sure to define your fleets below!

#Requires the following files:
# data.dir.2\\Comland_skates_hakes.R
# data.dir\\Menhaden.csv
# data.dir.3\\SS_NAFO_21A.csv
# data.dir.3\\species.txt
comland <- function(channel,GEARS=GEARs,EPUS=EPUs,use.existing="y",landed="y",foreign="y",adjust.ppi="y",sum.by="EPU",endyear=2018,reftime = c(2016,1),out.dir=here::here(),Stand.alone=F) {

  if(!(isS4(channel))) {
    message("Argument \"channel\", is not a valid DBI connection object. Please see dbutils::connect_to_database for details ...")
    return()
  }

refyear <- reftime[1]
refmonth <- reftime[2]

## Pull data from databases
if(use.existing == 'n'){
  comland <- get_comland_data(channel,landed,endyear,out.dir)
} else if(use.existing == 'y'){ # or read from directory
  if(landed == 'n') load(file = file.path(out.dir, "comland_raw_US.RData"))
  if(landed == 'y') load(file = file.path(out.dir, "comland_raw_US_meatwt.RData"))
}

#-------------------------------------------------------------------------------
#Convert from lbs to metric tons
comland[, SPPLIVMT := SPPLIVLB * 0.00045359237]
comland[, SPPLIVLB := NULL]
#fix years
comland$MONTH <- as.integer(comland$MONTH)
comland$YEAR <- as.integer(comland$YEAR)
comland$AREA <- as.integer(comland$AREA)
comland[YEAR < 100, YEAR := YEAR + 1900L]
#comland$YEAR <- as.character(comland$YEAR)

if(adjust.ppi == 'y'){
    #Adjust SPPVALUE for inflation
    temp <- tempfile()
    download.file("http://download.bls.gov/pub/time.series/wp/wp.data.3.ProcessedFoods", temp)
    inflate <- data.table::as.data.table(read.delim(temp))
    unlink(temp)

    inflate[, series_id := gsub(" ", "", inflate[, series_id])]
    deflate <- inflate[series_id == "WPU0223", ]
    deflate[, MONTH := as.numeric(substr(period, 2, 3))]
    data.table::setnames(deflate, c('year', 'value'), c('YEAR', 'PPI'))
    deflate <- deflate[, list(YEAR, MONTH, PPI)]

    #Set yearly deflator to 0 instead of 13 to match unknown month designation
    deflate[MONTH == 13, MONTH := 0]
    deflate.base <- deflate[YEAR == refyear & MONTH == refmonth, PPI]

    comland <- merge(comland, deflate, by = c('YEAR', 'MONTH'), all.x = T)
    comland[, SPPVALUE := round((SPPVALUE * deflate.base) / PPI)]

    #Remove extra column
    comland[, PPI := NULL]
}
#Remove market categories of parts
comland <- comland[!NESPP4 %in% c(119, 123, 125, 127, 812, 819, 828, 829, 1731, 2351,
                                  2690, 2699, 3472, as.numeric(paste(348:359, 8, sep = '')),
                                  3868, as.numeric(paste(469:471, 4, sep = '')),
                                  as.numeric(paste(480:499, 8, sep ='')), 5018, 5039,
                                  5261, 5265), ]

#Generate NESPP3 and MKTCAT in comland data
comland[NESPP4 < 100,                MKTCAT := as.numeric(substring(NESPP4, 2, 2))]
comland[NESPP4 > 99 & NESPP4 < 1000, MKTCAT := as.numeric(substring(NESPP4, 3, 3))]
comland[NESPP4 > 999,                MKTCAT := as.numeric(substring(NESPP4, 4, 4))]

#drop NESPP4
comland[, NESPP4 := NULL]

# Deal with Hakes and Skates------------------------------------------------------------------

skates_hakes <- comland_skates_hakes(EPUS,out.dir,Stand.alone)
skate.hake.us <- skates_hakes$skate.hake.us
skate.hake.nafo <- skates_hakes$skate.hake.nafo
#get little skates and winter skates from skates(ns) - use survey in half years
#Generate Half year variable in comland
comland.skates <- comland[NESPP3 == 365, ]
comland.skates[MONTH %in% 1:6,  Half := 1]
comland.skates[MONTH %in% 7:12, Half := 2]

data.table::setkey(skate.hake.us,
       YEAR,
       Half,
       AREA)

comland.skates <- merge(comland.skates, skate.hake.us, by = key(skate.hake.us), all.x = T)

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
setcolorder(skates.add.back, names(comland))

comland <- data.table::rbindlist(list(comland[NESPP3 != 365, ], skates.add.back))

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
setcolorder(hakes.add.back, names(comland))

comland <- data.table::rbindlist(list(comland[NESPP3 != 507, ], hakes.add.back))


#Herring---------------------------------------------------------------------------------
#Herring data is housed by the state of Maine.
herr.qry <- "select year, month, stock_area, negear, gearname, keptmt, discmt
             from maine_herring_catch"

herr.catch <- as.data.table(DBI::dbGetQuery(channel, herr.qry))
data.table::setkey(herr.catch, YEAR, MONTH, STOCK_AREA, NEGEAR)

herring <- herr.catch[, list(sum(KEPTMT), sum(DISCMT)), by = key(herr.catch)]
data.table::setnames(herring, c('STOCK_AREA', 'V1', 'V2'),
                  c('AREA', 'SPPLIVMT', 'DISCMT'))
herring$YEAR <- as.integer(herring$YEAR)
herring$MONTH <- as.double(herring$MONTH)

#Using averages from comland to fill in categories
herring[, MKTCAT := 5]
herring[, TONCL1 := 2]
herring[, UTILCD := 0]

#compute price/utilization from CF tables
herring.comland <- comland[NESPP3 == 168, ]
#Price from comland
herring.price <- herring.comland[, (sum(SPPVALUE) / sum(SPPLIVMT)), by = c('YEAR', 'MONTH')]
data.table::setnames(herring.price, 'V1', 'price')
herring <- merge(herring, herring.price, by = c('YEAR', 'MONTH'), all.x = T)
#Use 1964 prices for < 1964
herring[YEAR < 1964, price := mean(herring[YEAR == 1964, price])]
#Calculate SPPVALUE from price
herring[, SPPVALUE := round(price * SPPLIVMT)]

#Utilization from comland
herring.util <- herring.comland[, sum(SPPLIVMT), by = c('YEAR', 'MONTH', 'UTILCD')]
data.table::setnames(herring.util, 'V1', 'SPPLIVMT')
herring.util[, SPPLIVMT.ALL := sum(SPPLIVMT), by = c('YEAR', 'MONTH')]
herring.util[, Prop := SPPLIVMT/SPPLIVMT.ALL]
data.table::setorder(herring.util, YEAR, MONTH, Prop)
herring.util[, cum.prop := cumsum(Prop), by = c('YEAR', 'MONTH')]

#Apply proportions to Maine data set
#Not pulled all the time - current through 2017
herring[, Total := sum(SPPLIVMT), by = c('YEAR', 'MONTH')]
herring[, Prop := SPPLIVMT / Total]
data.table::setorder(herring, YEAR, MONTH, Prop)
herring[, cum.prop := cumsum(Prop), by = c('YEAR', 'MONTH')]

for(iyear in unique(herring.util[, YEAR])){
  for(imonth in unique(herring.util[YEAR == iyear, MONTH])){
    cum.prop.low <- 0
    for(iutil in herring.util[YEAR == iyear & MONTH == imonth, UTILCD]){
      cum.prop.high <- herring.util[YEAR == iyear & MONTH == imonth &
                                      UTILCD == iutil, cum.prop]
      herring[YEAR == iyear & MONTH == imonth & cum.prop <= cum.prop.high &
                cum.prop > cum.prop.low, UTILCD := iutil]
      cum.prop.low <- cum.prop.high
    }
  }
}

#fix column headings
herring[, c('Total', 'Prop', 'cum.prop', 'price', 'DISCMT') := NULL]
herring[, NESPP3 := 168]
setcolorder(herring, names(comland))

#remove herring from data pull and add in Maine numbers
comland <- data.table::rbindlist(list(comland[NESPP3 != 168, ], herring))

#Menhaden------------------------------------------------------------------------------------
##fix menhaden records - data from Tom Miller/ Andre Bouchheister
#menhaden <- as.data.table(read.csv(paste(data.dir, "Menhaden.csv", sep = '')))
#menhaden.mab <- menhaden[, MA.Total + CB.Total, by = Year]
##file metric is 1000s of lbs - convert to mt
#menhaden.mab[, SPPLIVMT := (V1 * 1000) *  0.00045359237]
#menhaden.mab[, V1 := NULL]
#
#menhaden.gom <- menhaden[, list(Year, NE.Total)]
#menhaden.gom[, SPPLIVMT := (NE.Total * 1000) *  0.00045359237]
#menhaden.gom[, NE.Total := NULL]

#save(comland, file = paste(out.dir, "Comland_unkA.RData", sep = ''))

#Deal with unknowns-------------------------------------------------------------------------
comland[NEGEAR == 999,  NEGEAR := 0]
comland[is.na(TONCL1),  TONCL1 := 0]
comland[is.na(AREA),    AREA   := as.factor(0)]
comland[AREA == 999,    AREA   := as.factor(0)]
comland[is.na(MKTCAT),  MKTCAT := 0]
comland[is.na(UTILCD),  UTILCD := 0]

#1 - drop unknown species/landings
comland <- comland[NESPP3 != 0 & SPPLIVMT != 0, ]

#Sumarry tables
#missing area
#known.area <-   comland[AREA != 0, sum(SPPLIVMT), by = NESPP3]
#unknown.area <- comland[AREA == 0, sum(SPPLIVMT), by = NESPP3]
#data.table::setnames(known.area,   "V1", "AREA.MT.known")
#data.table::setnames(unknown.area, "V1", "AREA.MT.unknown")
#missing.table <- merge(known.area, unknown.area, by = 'NESPP3', all = T)
#
#missing.table[is.na(AREA.MT.known),   AREA.MT.known   := 0]
#missing.table[is.na(AREA.MT.unknown), AREA.MT.unknown := 0]
#missing.table[, AREA.Ratio := AREA.MT.unknown / AREA.MT.known]
#
##missing month
#known.month <-   comland[MONTH != 0, sum(SPPLIVMT), by = NESPP3]
#unknown.month <- comland[MONTH == 0, sum(SPPLIVMT), by = NESPP3]
#data.table::setnames(known.month,   "V1", "MONTH.MT.known")
#data.table::setnames(unknown.month, "V1", "MONTH.MT.unknown")
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
#data.table::setnames(known.gear,   "V1", "GEAR.MT.known")
#data.table::setnames(unknown.gear, "V1", "GEAR.MT.unknown")
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
#data.table::setnames(known.tc,   "V1", "TC.MT.known")
#data.table::setnames(unknown.tc, "V1", "TC.MT.unknown")
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
comland[MONTH %in% 1:3,   QY := 1]
comland[MONTH %in% 4:6,   QY := 2]
comland[MONTH %in% 7:9,   QY := 3]
comland[MONTH %in% 10:12, QY := 4]
comland[MONTH == 0,       QY := 0]

comland[MONTH %in% 1:6,  HY := 1]
comland[MONTH %in% 7:12, HY := 2]
comland[MONTH == 0,      HY := 0]


comland[NEGEAR %in% GEARS$otter,     GEAR := 'otter']
comland[NEGEAR %in% GEARS$dredge.sc, GEAR := 'dredge.sc']
comland[NEGEAR %in% GEARS$pot,       GEAR := 'pot']
comland[NEGEAR %in% GEARS$longline,  GEAR := 'longline']
comland[NEGEAR %in% GEARS$seine,     GEAR := 'seine']
comland[NEGEAR %in% GEARS$gillnet,   GEAR := 'gillnet']
comland[NEGEAR %in% GEARS$midwater,  GEAR := 'midwater']
comland[NEGEAR %in% GEARS$dredge.o,  GEAR := 'dredge.o']
comland[NEGEAR == 0,           GEAR := 'unknown']
comland[is.na(GEAR),           GEAR := 'other']
comland[, GEAR := as.factor(GEAR)]

comland[TONCL1 %in% 1:3, SIZE := 'small']
comland[TONCL1 > 3,      SIZE := 'large']
comland[TONCL1 == 0,     SIZE := 'unknown']
comland[, SIZE := as.factor(SIZE)]

data.table::setkey(comland,
       YEAR,
       QY,
       HY,
       GEAR,
       SIZE,
       AREA,
       NESPP3,
       UTILCD)

comland.agg <- comland[, list(sum(SPPLIVMT), sum(SPPVALUE)), by = key(comland)]

data.table::setnames(comland.agg, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

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

#3.B SIZE------------------------------------------------------------------------------
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

#3.C GEAR------------------------------------------------------------------------------
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

#3.D AREA------------------------------------------------------------------------------
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

#-------------------------------------------------------------------------------
if(sum.by == 'EPU'){
  #Assign EPU based on statarea

  comland.agg[AREA %in% EPUS$GOM, EPU := 'GOM']
  comland.agg[AREA %in% EPUS$GB,  EPU := 'GB']
  comland.agg[AREA %in% EPUS$MAB, EPU := 'MAB']
  comland.agg[AREA %in% EPUS$SS,  EPU := 'SS']
  comland.agg[is.na(EPU),    EPU := 'OTHER']
  comland.agg[, EPU := factor(EPU, levels = c('GOM', 'GB', 'MAB', 'SS', 'OTHER'))]

  data.table::setkey(comland.agg,
         YEAR,
         NESPP3,
         QY,
         GEAR,
         SIZE,
         EPU,
         UTILCD)

  comland.agg <- comland.agg[, list(sum(SPPLIVMT), sum(SPPVALUE)), by = key(comland.agg)]

  data.table::setnames(comland.agg, c('V1', 'V2'), c('SPPLIVMT', 'SPPVALUE'))

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
  download.file("https://www.nafo.int/Portals/0/Stats/nafo-21b-2010-16.zip",temp)
  nafo.10 <- as.data.table(read.csv(unz(temp, "nafo-21b-2010-16/NAFO-21B-2010-16.txt")))
  unlink(temp)

  #2010 + data have different column headers
  data.table::setnames(nafo.10,
          c('Gear', 'AreaCode', 'SpeciesEffort'),
          c('GearCode', 'Divcode', 'Code'))

  nafo <- data.table::rbindlist(list(nafo.60, nafo.70, nafo.80, nafo.90, nafo.00, nafo.10), fill = T)

  #Remove US landings (Country code 22), extra divisions (use only 47, 51:56, 61:63),
  #and effort codes (1:3)
  nafo <- nafo[Country != 22 & Divcode %in% c(47, 51:56, 61:63) & Code > 3, ]

  #Deal with unknown monthly catch?????

  #Get nafo code in a similar format to comland
  nafoland <- nafo[, list(Year, GearCode, Tonnage, Divcode, Code, Catches)]
  nafoland[, MONTH := 0]
  data.table::setnames(nafoland, 'Catches', 'SPPLIVMT')

  month <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
  for(i in 1:12){
    nafoland.month <- nafo[, list(Year, GearCode, Tonnage, Divcode, Code, get(month[i]))]
    nafoland.month[, MONTH := i]
    data.table::setnames(nafoland.month,
            names(nafoland.month)[6],
            'SPPLIVMT')
    nafoland <- data.table::rbindlist(list(nafoland, nafoland.month))
    }

  nafoland <- nafoland[SPPLIVMT != 0,]

  nafoland[, EPU := factor(NA, levels = c('GOM', 'GB', 'MAB', 'SS', 'OTHER'))]
  nafoland[Divcode == 47,             EPU := 'SS']
  nafoland[Divcode == 51,             EPU := 'GOM']
  nafoland[Divcode %in% c(52, 54:56), EPU := 'GB']
  nafoland[Divcode %in% c(53, 61:63), EPU := 'MAB']
  nafoland[is.na(EPU),                EPU := 'OTHER']

  nafoland[, Divcode := NULL]
  ##Fix missing Scotian Shelf data from 21B
  SS.nafo <- as.data.table(read.csv(system.file("extdata","SS_NAFO_21A.csv",package="comlandr"), skip = 8))

  #Add NAFOSPP code to SS.nafo
  nafo.spp <- as.data.table(read.csv(system.file("extdata","species.txt",package="comlandr")))
  data.table::setnames(nafo.spp, "Abbreviation", "Species_ASFIS")
  nafo.spp <- nafo.spp[, list(Code, Species_ASFIS)]

  SS.nafo <- merge(SS.nafo, nafo.spp, by = 'Species_ASFIS', all.x = T)

  #Only grab missing data
  SS.nafo <- SS.nafo[Year %in% c(2003, 2008, 2009), ]

  data.table::setkey(SS.nafo,
         Year,
         Code)

  SS.land <- SS.nafo[, sum(Catch...000.Kg.), by = key(SS.nafo)]

  data.table::setnames(SS.land, "V1", "SPPLIVMT")

  #Add GearCode, Tonnage, Month, and EPU
  SS.land[, GearCode := 99]
  SS.land[, Tonnage  := 0]
  SS.land[, MONTH    := 0]
  SS.land[, EPU      := 'SS']

  setcolorder(SS.land, names(nafoland))

  nafoland <- data.table::rbindlist(list(nafoland, SS.land))

  #Rectify NAFO codes with US codes
  #Species
  data.table::setnames(nafoland,
          c('Year', 'GearCode', 'Tonnage', 'Code'),
          c('YEAR', 'NAFOGEAR', 'TONCL1', 'NAFOSPP'))

  spp <- as.data.table(DBI::dbGetQuery(channel, "select NAFOSPP, NESPP3 from CFSPP"))

  #Fix missing NAFO codes
  missing.spp <- data.table::data.table(NAFOSPP = c(110, 141, 189, 480, 484, 487, 488, 489),
                            NESPP3  = c(240, 509, 512, 366, 368, 367, 370, 369))
  spp <- data.table::rbindlist(list(spp, missing.spp))

  data.table::setkey(spp, NAFOSPP)
  spp <- unique(spp, by = key(spp))

  #Fix many to one relationships
  spp[NAFOSPP == 199, NESPP3 := 524]
  spp[NAFOSPP == 299, NESPP3 := 525]
  spp[NAFOSPP == 469, NESPP3 := 359]
  spp[NAFOSPP == 499, NESPP3 := 526]
  spp[NAFOSPP == 529, NESPP3 := 764]
  spp[NAFOSPP == 699, NESPP3 := 899]
  spp$NAFOSPP <- as.integer(spp$NAFOSPP)


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
  gear <- as.data.table(DBI::dbGetQuery(channel, "select NEGEAR, NAFOGEAR from Gear"))
  gear <- unique(gear, by = 'NAFOGEAR')
  gear$NAFOGEAR <- as.double(gear$NAFOGEAR)

  #return(list(nafoland=nafoland,gear=gear))

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

  #Fix skates
  #get little skates and winter skates from skates(ns) - use survey in half years
  #Generate Half year variable in comland
  nafoland.skates <- nafoland[NESPP3 == 365, ]
  nafoland.skates[MONTH %in% 1:6, Half := 1]
  nafoland.skates[MONTH %in% 7:12, Half := 2]

  data.table::setkey(skate.hake.nafo,
         YEAR,
         Half,
         EPU)

  nafoland.skates <- merge(nafoland.skates, skate.hake.nafo, by = key(skate.hake.nafo), all.x = T)

  nafoland.skates[NESPP3 == 365, little := little.per * SPPLIVMT]
  nafoland.skates[is.na(little), little := 0]

  nafoland.skates[NESPP3 == 365, winter := winter.per * SPPLIVMT]
  nafoland.skates[is.na(winter), winter := 0]

  nafoland.skates[NESPP3 == 365, other.skate := SPPLIVMT - (little + winter)]

  #Little (366), winter (367), skates(ns) (365)
  #put skates in nafoland format to merge back
  little <- nafoland.skates[, list(YEAR, Half, EPU, TONCL1, MONTH,
                                   NESPP3, NEGEAR, little)]
  little[, NESPP3 := 366L]
  data.table::setnames(little, "little", "SPPLIVMT")
  little <- little[SPPLIVMT > 0, ]

  winter <- nafoland.skates[, list(YEAR, Half, EPU, TONCL1, MONTH,
                                   NESPP3, NEGEAR, winter)]
  winter[, NESPP3 := 367L]
  data.table::setnames(winter, "winter", "SPPLIVMT")
  winter <- winter[SPPLIVMT > 0, ]

  other <- nafoland.skates[, list(YEAR, Half, EPU, TONCL1, MONTH,
                                  NESPP3, NEGEAR, other.skate)]
  other[, NESPP3 := 365L]
  data.table::setnames(other, "other.skate", "SPPLIVMT")
  other <- other[SPPLIVMT > 0, ]

  #merge all three and reformat for nafoland
  skates.add.back <- data.table::rbindlist(list(little, winter, other))

  skates.add.back[, Half := NULL]
  setcolorder(skates.add.back, names(nafoland))

  nafoland <- data.table::rbindlist(list(nafoland[NESPP3 != 365, ], skates.add.back))

  #aggregate nafo landings
  #2 - aggregate by quarter year, half year, major gear, and small/large TC
  nafoland[MONTH %in% 1:3,   QY := 1]
  nafoland[MONTH %in% 4:6,   QY := 2]
  nafoland[MONTH %in% 7:9,   QY := 3]
  nafoland[MONTH %in% 10:12, QY := 4]
  nafoland[MONTH == 0,       QY := 1]

  nafoland[NEGEAR %in% GEARS$otter,     GEAR := 'otter']
  nafoland[NEGEAR %in% GEARS$dredge.sc, GEAR := 'dredge.sc']
  nafoland[NEGEAR %in% GEARS$pot,       GEAR := 'pot']
  nafoland[NEGEAR %in% GEARS$longline,  GEAR := 'longline']
  nafoland[NEGEAR %in% GEARS$seine,     GEAR := 'seine']
  nafoland[NEGEAR %in% GEARS$gillnet,   GEAR := 'gillnet']
  nafoland[NEGEAR %in% GEARS$midwater,  GEAR := 'midwater']
  nafoland[NEGEAR %in% GEARS$dredge.o,  GEAR := 'dredge.o']
  nafoland[NEGEAR == 99,          GEAR := 'unknown']
  nafoland[is.na(GEAR),           GEAR := 'other']
  nafoland[, GEAR := as.factor(GEAR)]

  nafoland[TONCL1 %in% 1:3, SIZE := 'small']
  nafoland[TONCL1 > 3,      SIZE := 'large']
  nafoland[TONCL1 == 0,     SIZE := 'unknown']
  nafoland[, SIZE := as.factor(SIZE)]

  data.table::setkey(nafoland,
         YEAR,
         QY,
         GEAR,
         SIZE,
         EPU,
         NESPP3)

  nafoland.agg <- nafoland[, sum(SPPLIVMT), by = key(nafoland)]

  data.table::setnames(nafoland.agg, "V1", "SPPLIVMT")

  #Create dummy variable for value
  nafoland.agg[, SPPVALUE := 0]
  nafoland.agg[, UTILCD := 0]

  #Merge comland and nafoland
  setcolorder(nafoland.agg, names(comland.agg))

  if(foreign == 'y'){
    comland.agg[,  US := T]
    nafoland.agg[, US := F]
  }

  comland.nafo <- data.table::rbindlist(list(comland.agg, nafoland.agg))

  #Remove Menhaden data
  #save(comland.nafo, file = paste(out.dir, "comland_Menhaden.RData", sep = ''))
  comland <- comland.nafo[NESPP3 != 221, ]

}

if(sum.by == 'stat.area') comland <- comland.agg

#Output file
if(landed     == 'n') file.landed <- '' else file.landed <- '_meatwt'
if(adjust.ppi == 'n') file.adjust <- '' else file.adjust <- '_deflated'
if(sum.by == 'EPU') file.by <- '_EPU' else file.by <- '_stat_areas'
file.name <- paste0('comland', file.landed, file.adjust, file.by,Sys.Date())

save(comland, file = file.path(out.dir, paste0(file.name,".RData")))
saveRDS(comland, file = file.path(out.dir, paste0(file.name,".Rds")))

}
