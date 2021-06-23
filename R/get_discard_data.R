#Comdisc.r 
library(here); library(data.table); library(comlandr)

channel <- dbutils::connect_to_database(server="nova",uid="slucey")

get_observer_data <- function(channel, filterByYear, )
endyear <- 2017

strat.var <- c('YEAR', 'QY', 'EPU', 'GEAR')
haullevel <- F #Toggle whether to save haul by haul data or not
landings.file <- 'comland_meatwt_deflated.RData'



#-------------------------------------------------------------------------------
#User created functions 
#Sums the number of occurances
count<-function(x){
  num<-rep(1,length(x))
  out<-sum(num)
  return(out)
  }
  
#-------------------------------------------------------------------------------

filterByYear <- 1989

#Create year vector
if(is.na(filterByYear[1])){
  years <- ">= 1989"
}else{
  years <- paste0("in (", survdat:::sqltext(filterByYear), ")")
}

ob.qry <- paste0("select year, month, area, negear, nespp4, hailwt, catdisp, drflag, 
          tripid, haulnum, lathbeg, lonhbeg, link3
          from OBSPP
          where obsrflag = 1
          and program not in ('127', '900', '250', '160')
          and year ", years,
          "\n union
          select year, month, area, negear, nespp4, hailwt, catdisp, drflag, 
          tripid, haulnum, lathbeg, lonhbeg, link3
          from ASMSPP
          where obsrflag = 1
          and program not in ('127', '900', '250', '160')
          and year ", years)

ob <- data.table::as.data.table(DBI::dbGetQuery(channel, ob.qry))

#Add protected species here
mammal.qry <- paste0("select distinct a.year, a.month, b.area, b.negear, a.nespp4, 
               1 as hailwt, 0 as catdisp, 1 as drflag, a.tripid, a.haulnum, 
               b.lathbeg, b.lonhbeg, a.link3
               from obinc a, obspp b
               where a.tripid = b.tripid
               and a.year ", years,
               "\n union
               select distinct a.year, a.month, b.area, b.negear, a.nespp4, 
               1 as hailwt, 0 as catdisp, 1 as drflag, a.tripid, a.haulnum, 
               b.lathbeg, b.lonhbeg, a.link3
               from asminc a, asmspp b
               where a.tripid = b.tripid
               and a.year ", years)
               
mammal <- data.table::as.data.table(DBI::dbGetQuery(channel, mammal.qry))

ob <- rbindlist(list(ob, mammal))

#Grab otter trawl gear tables to get mesh size for small verses large mesh
mesh.qry <- paste0("select link3, codmsize
             from OBOTGH
             where year ", years)
mesh <- data.table::as.data.table(DBI::dbGetQuery(channel, mesh.qry))

#Convert mesh size from mm to inches
mesh[, CODMSIZE := CODMSIZE * 0.0393701]
mesh[CODMSIZE <= 3, MESHCAT := 'SM']
mesh[CODMSIZE >  3, MESHCAT := 'LG']
mesh[, CODMSIZE := NULL]

ob <- merge(ob, mesh, by = 'LINK3', all.x = T)

#Clean up data set
#Remove those with unknown disposition
ob <- ob[CATDISP != 9, ]
 
#remove record if weight is missing
ob <- ob[!is.na(HAILWT), ]    
 
#remove non-living items (clappers and stomach contents) and unknown living matter
ob <- ob[!(NESPP4 %in% c(0, 6800:6802, 6805, 6810, 6820, 6830, 6850:6857, 6882, 6883, 6894:6897))]  

#Convert lat/lon to decimal degrees
ob[, LATDD := substr(LATHBEG, 1, 2) + ((substr(LATHBEG, 3, 4) + substr(LATHBEG, 5, 6))
   / 60)]

#Convert weights
convert.qry <- "select nespp4_obs, catdisp_code, drflag_code, cf_lndlb_livlb, cf_rptqty_lndlb  
                from obspecconv"
convert <- data.table::as.data.table(DBI::dbGetQuery(channel, convert.qry))

setnames(convert,
        c('NESPP4_OBS', 'CATDISP_CODE', 'DRFLAG_CODE'),
        c('NESPP4',     'CATDISP',      'DRFLAG'))

setkey(convert,
      NESPP4,
      CATDISP,
      DRFLAG)

ob.code <- merge(ob, convert, by = key(convert), all.x = T) 

#missing cf's will be set to 1 Assume living
ob.code[is.na(CF_LNDLB_LIVLB), CF_LNDLB_LIVLB := 1]
ob.code[is.na(CF_RPTQTY_LNDLB), CF_RPTQTY_LNDLB := 1]
 
ob.code[, C.HAILWT := HAILWT * CF_RPTQTY_LNDLB * CF_LNDLB_LIVLB] 

#Grab common name and PR flags
comname.qry <- "select NESPP4, comname, sciname, cetacean, turtle, pinniped
                from obspec"

comname <- data.table::as.data.table(DBI::dbGetQuery(channel, comname.qry))
comname[CETACEAN == 1 | TURTLE == 1 | PINNIPED == 1, PR := 1]
comname[is.na(PR), PR := 0]
comname[, c('CETACEAN', 'TURTLE', 'PINNIPED') := NULL]

ob.code <- merge(comname, ob.code, by = 'NESPP4')

#Convert to metric tons to align with commercial landings data
ob.code[PR == 0, C.HAILWT := C.HAILWT * 0.00045359237]

#Change to NESPP3 to combine market categories
ob.code[, NESPP3 := substring(NESPP4, 1, 3)]
#Birds, mammals, etc don't have unique NESPP3 codes
ob.code[is.na(NESPP3), NESPP3 := NESPP4] 

ob.code[, MKTCAT := as.numeric(substring(NESPP4, 4, 4))]

#drop NESPP4
ob.code[, NESPP4 := NULL]

#Deal with skate(ns) for little and winter skates
source(file.path(data.dir.2, 'Comland_skates_hakes.R'))

#get little skates and winter skates from skates(ns) - use survey in half years
#Generate Half year variable in comland
ob.skates <- ob.code[NESPP3 == 365, ]
ob.skates[MONTH %in% 1:6,  Half := 1]
ob.skates[MONTH %in% 7:12, Half := 2]

setkey(skate.hake.us,
       YEAR,
       Half,
       AREA)

ob.skates <- merge(ob.skates, skate.hake.us, by = key(skate.hake.us), all.x = T)

ob.skates[NESPP3 == 365, little := little.per * C.HAILWT]
ob.skates[is.na(little), little := 0]

ob.skates[NESPP3 == 365, winter := winter.per * C.HAILWT]
ob.skates[is.na(winter), winter := 0]

ob.skates[NESPP3 == 365, other.skate := C.HAILWT - (little + winter)]

#Little (366), winter (367), skates(ns) (365)
#put skates in ob.code format to merge back
little <- ob.skates[, list(COMNAME, SCINAME, PR, CATDISP, DRFLAG,
                           YEAR, MONTH, AREA, NEGEAR, HAILWT, 
                           TRIPID, HAULNUM, LINK1, LINK3, CF_LNDLB_LIVLB, 
                           CF_RPTQTY_LNDLB, little, NESPP3, MKTCAT)]
little[, NESPP3 := 366]
setnames(little, "little", "C.HAILWT")
little <- little[C.HAILWT > 0, ]

winter <- ob.skates[, list(COMNAME, SCINAME, PR, CATDISP, DRFLAG,
                           YEAR, MONTH, AREA, NEGEAR, HAILWT, 
                           TRIPID, HAULNUM, LINK1, LINK3, CF_LNDLB_LIVLB, 
                           CF_RPTQTY_LNDLB, winter, NESPP3, MKTCAT)]
winter[, NESPP3 := 367]
setnames(winter, "winter", "C.HAILWT")
winter <- winter[C.HAILWT > 0, ]

other <- ob.skates[, list(COMNAME, SCINAME, PR, CATDISP, DRFLAG,
                          YEAR, MONTH, AREA, NEGEAR, HAILWT, 
                          TRIPID, HAULNUM, LINK1, LINK3, CF_LNDLB_LIVLB, 
                          CF_RPTQTY_LNDLB, other.skate, NESPP3, MKTCAT)]
other[, NESPP3 := 365]
setnames(other, "other.skate", "C.HAILWT")
other <- other[C.HAILWT > 0, ]

#merge all three and reformat for ob
skates.add.back <- rbindlist(list(little, winter, other))

setcolorder(skates.add.back, names(ob.code))

ob.code <- rbindlist(list(ob.code[NESPP3 != 365, ], skates.add.back))  

#Assign stat areas to EPUs
gom <- c(500,510,512:515)
gb  <- c(521:526,551,552,561,562)
mab <- c(537,539,600,612:616,621,622,625,626,631,632)
ss  <- c(463:467,511)

ob.code[AREA %in% gom, EPU := 'GOM']
ob.code[AREA %in% gb,  EPU := 'GB']
ob.code[AREA %in% mab, EPU := 'MAB']
ob.code[AREA %in% ss,  EPU := 'SS']
ob.code[is.na(EPU),    EPU := 'OTHER']
ob.code[, EPU := factor(EPU, levels = c('GOM', 'GB', 'MAB', 'SS', 'OTHER'))]

#Create quarter year variable
ob.code[MONTH %in% 1:3,   QY := 1]
ob.code[MONTH %in% 4:6,   QY := 2]
ob.code[MONTH %in% 7:9,   QY := 3]
ob.code[MONTH %in% 10:12, QY := 4]

#Aggregate Gear
otter     <- 50:59
dredge.sc <- 131:132
pot       <- c(189:190, 200:219, 300, 301)
longline  <- c(10, 40)
seine     <- c(70:79, 120:129, 360)
gillnet   <- c(100:119, 500, 510, 520)
midwater  <- c(170, 370)
dredge.o  <- c(281, 282, 380:400)

ob.code[NEGEAR %in% otter,     GEAR := 'otter']
ob.code[NEGEAR %in% dredge.sc, GEAR := 'dredge.sc']
ob.code[NEGEAR %in% pot,       GEAR := 'pot']
ob.code[NEGEAR %in% longline,  GEAR := 'longline']
ob.code[NEGEAR %in% seine,     GEAR := 'seine']
ob.code[NEGEAR %in% gillnet,   GEAR := 'gillnet']
ob.code[NEGEAR %in% midwater,  GEAR := 'midwater']
ob.code[NEGEAR %in% dredge.o,  GEAR := 'dredge.o']
ob.code[is.na(GEAR),           GEAR := 'other']
ob.code[, GEAR := as.factor(GEAR)]

ob.code[, c('DRFLAG', 'MONTH', 'AREA', 'NEGEAR', 
            'HAILWT', 'CF_LNDLB_LIVLB', 'CF_RPTQTY_LNDLB') := NULL]

setkeyv(ob.code, c(strat.var, 'NESPP3', 'CATDISP'))

if(haullevel == T){#This is broken
  ob.haul <- ob.code
  save(comdisc, file = file.path(out.dir, "Observer_Discards_by_Haul.RData"))
}

ob.sums <- ob.code[, sum(C.HAILWT), by = key(ob.code)]

#Make a new function
#Calculate kept and discards
ob.discard <- ob.sums[CATDISP == 0, ]

setnames(ob.discard,
        "V1",
        "DISCARD")

setkeyv(ob.sums, strat.var)
      
ob.kept <- ob.sums[CATDISP == 1, sum(V1), by = key(ob.sums)]

setnames(ob.kept,
        "V1",
        "KEPT.ALL")
        
ob.all <- merge(ob.kept, ob.discard, by = key(ob.sums))

ob.all[, CATDISP := NULL]

ob.all[, DK := DISCARD / KEPT.ALL]
ob.all[is.na(DK), DK := 1.0]
ob.all[, c('KEPT.ALL', 'DISCARD') := NULL]

#Get landings
load(file.path(data.dir, landings.file))

setkeyv(comland, strat.var)
      
tot.land <- comland[, sum(SPPLIVMT), by = key(comland)]

setnames(tot.land,
        "V1",
        "TOT.LAND")
        
comdisc <- merge(ob.all, tot.land, by = key(comland))

comdisc[, DISC := DK * TOT.LAND]

#Variance
#Need to add back individual trip data
rm(ob) #Free up memory
setkeyv(comdisc, c(strat.var, 'NESPP3'))

disc.var <- unique(comdisc, by = key(comdisc))

#Trip kept all
setkeyv(ob.code, c(strat.var, 'TRIPID'))
                 
trip.kept <- ob.code[CATDISP == 1, sum(C.HAILWT), by = key(ob.code)]                
setnames(trip.kept, "V1", "trip.k")

#Trip discard by species
setkeyv(ob.code, c(strat.var, 'TRIPID', 'NESPP3'))
                 
trip.disc <- ob.code[CATDISP == 0, sum(C.HAILWT), by = key(ob.code)]                
setnames(trip.disc, "V1", "trip.d")

trip.all <- merge(trip.disc, trip.kept, by = c(strat.var, 'TRIPID'), all = T)
trip.all[is.na(trip.k), trip.k := 0] 

disc.var <- merge(disc.var, trip.all, by = c(strat.var, 'NESPP3'))

#Calculate the number of observed trips
setkeyv(ob.code, c(strat.var, 'TRIPID'))
                
trips <- unique(ob.code, by = key(ob.code))

trip.count <- trips[, count(TRIPID), by = strat.var]

setnames(trip.count, "V1", "n")

disc.var <- merge(disc.var, trip.count, by = strat.var)

#Calculate the total number of trips
#CFDBS is on sole - need to switch connection
odbcClose(channel)
if(Sys.info()['sysname']=="Windows"){
  channel <- odbcDriverConnect()
} else {
  channel <- odbcConnect('sole', uid, pwd)
}

tables <- c(paste('WODETS',  89:93, sep = ''), 
            paste('CFDETS',  1994:endyear, 'AA', sep = ''))
            
comtrip.qry <- "select year, month, area, negear, count(link) as N
                from WODETS89
                group by year, month, area, negear"
comtrip <- as.data.table(sqlQuery(channel, comtrip.qry))

for(i in 2:length(tables)){
  tripyr.qry <- paste("select year, month, area, negear, count(link) as N
                       from", tables[i],
                       "group by year, month, area, negear")
  tripyr <- as.data.table(sqlQuery(channel, tripyr.qry))

  comtrip <- rbindlist(list(comtrip, tripyr))
  }
  
comtrip[AREA %in% gom, EPU := 'GOM']
comtrip[AREA %in% gb,  EPU := 'GB']
comtrip[AREA %in% mab, EPU := 'MAB']
comtrip[AREA %in% ss,  EPU := 'SS']
comtrip[is.na(EPU),    EPU := 'OTHER']
comtrip[, EPU := factor(EPU, levels = c('GOM', 'GB', 'MAB', 'SS', 'OTHER'))]

comtrip[YEAR < 100, YEAR := YEAR + 1900]

comtrip[MONTH %in% 1:3,   QY := 1]
comtrip[MONTH %in% 4:6,   QY := 2]
comtrip[MONTH %in% 7:9,   QY := 3]
comtrip[MONTH %in% 10:12, QY := 4]

comtrip[NEGEAR %in% otter,     GEAR := 'otter']
comtrip[NEGEAR %in% dredge.sc, GEAR := 'dredge.sc']
comtrip[NEGEAR %in% pot,       GEAR := 'pot']
comtrip[NEGEAR %in% longline,  GEAR := 'longline']
comtrip[NEGEAR %in% seine,     GEAR := 'seine']
comtrip[NEGEAR %in% gillnet,   GEAR := 'gillnet']
comtrip[NEGEAR %in% midwater,  GEAR := 'midwater']
comtrip[NEGEAR %in% dredge.o,  GEAR := 'dredge.o']
comtrip[is.na(GEAR),           GEAR := 'other']
comtrip[, GEAR := as.factor(GEAR)]

setkeyv(comtrip, strat.var)

comtrip.count <- comtrip[, sum(N), by = key(comtrip)]

setnames(comtrip.count, "V1", "N")

disc.var <- merge(disc.var, comtrip.count, by = key(comtrip), all.x = T)

#Fix groups that don't line up properly - actual value of N not that important only relative size
N.avg <- disc.var[, mean(N, na.rm = T)]
disc.var[is.na(N), N := N.avg]

#Calculate variance
#Need to expand so zero discards by species are represented
setkeyv(disc.var, c(strat.var, 'TRIPID'))
var.trips <- unique(disc.var, by = key(disc.var))
#drop species specific data
var.trips[, c('NESPP3', 'DK', 'DISC', 'trip.d') := NULL]

#Get list of species
spp <- unique(disc.var[, NESPP3])
all.spp.var <- c()
for(i in 1:length(spp)){
  spp.trip <- disc.var[NESPP3 == spp[i], ]  
  #Get rid of extra data
  spp.trip[, c('TOT.LAND', 'DISC', 'trip.k', 'n', 'N') := NULL]
  
  spp.var <- merge(var.trips, spp.trip, by = c(strat.var, 'TRIPID'), all.x = T)
  
  #Fix NAs
  spp.var[is.na(NESPP3), NESPP3 := spp[i]]
  spp.var[is.na(trip.d), trip.d := 0]
  
  #Merge in DK ratios
  setkeyv(spp.trip, strat.var)
  spp.dk <- unique(spp.trip, by = key(spp.trip))
  spp.var[, DK := NULL]
  spp.dk[, c('NESPP3', 'TRIPID', 'trip.d') := NULL]
  spp.var <- merge(spp.var, spp.dk, by = strat.var, all.x = T)
  spp.var[is.na(DK), DK := 0]
  
  spp.var[, step.1 := (sum(trip.d^2 + DK^2 * trip.k^2 - 2 * DK * trip.d * trip.k)/(n - 1)), by = strat.var]

  setkeyv(spp.var, strat.var)
  spp.var <- unique(spp.var, by = key(spp.var))
  spp.var[, c('TRIPID', 'trip.d', 'trip.k', 'DK') := NULL]

  spp.var[, DISC.VAR :=  TOT.LAND^2 * ((N - n)/n*N) * (1/(TOT.LAND/n)^2) * step.1]
  spp.var[, c('TOT.LAND', 'n', 'N', 'step.1') := NULL]

  all.spp.var <- rbindlist(list(all.spp.var, spp.var))
  }
comdisc <- merge(comdisc, all.spp.var, by = c(strat.var, 'NESPP3'), all.x = T)

#Add species names
#Change to NESPP3 to combine market categories
comname[NESPP4 < 100,                        NESPP3 := as.numeric(substring(NESPP4, 1, 1))]
comname[NESPP4 > 99 & NESPP4 < 1000,         NESPP3 := as.numeric(substring(NESPP4, 1, 2))]
comname[(NESPP4 > 999 & NESPP4 < 6100) | 
        NESPP4 %in% c(7100:7109, 8020:8029), NESPP3 := as.numeric(substring(NESPP4, 1, 3))]
#Birds, mammals, etc don't have unique NESPP3 codes
comname[NESPP4 > 6099 & !NESPP4 %in% c(7100:7109, 8020:8029), NESPP3 := NESPP4]

setkey(comname, NESPP3)
comname <- unique(comname, by = key(comname))
comname[, c('NESPP4', 'SCINAME') := NULL]

comdisc <- merge(comname, comdisc, by = 'NESPP3')

save(comdisc, file = file.path(out.dir, "Comdisc.RData"))
