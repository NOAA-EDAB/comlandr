#'#Comcatch_skates_hakes.r
#'
#'Determine proportion of little/winter skates and silver hake in landings data 7/13
#'SML
#'
#'@param EPUS List. Designates the stat areas that comprise an EPU.
#'@param out.dir Character string. Path to directory where skate data will be written
#'@param Stand.alone Boolean. Flag to determine whether to save Skate and hake data to file. defualt = F (Both a US catch file and a NAFO catch file will be saved)
#'
#'@return A list
#'
#'\item{skate.hake.us}{landings of skates and hakes in USA }
#'\item{skate.hake.nafo}{landings of skates and hakes outside USA}
#'
#'
#'@importFrom data.table ":=" "key"
#'
#' @noRd

comland_skates_hakes <- function(EPUS,out.dir,Stand.alone=F){


#-------------------------------------------------------------------------------
#Required packages
#library(rgdal); library(data.table); library(Survdat)

#-------------------------------------------------------------------------------
#User created functions
#source(paste(r.dir, "Poststrat.r", sep = ''))

#-------------------------------------------------------------------------------
#Skates and hakes
spp.name <- c('Little_Skate', 'Winter_Skate', 'Silver_Hake')
spp <- c(26, 23, 72)

#Grab survdat.r
pathToSurvdat <- system.file("extdata","Survdat.RData",package="comlandr")
load(pathToSurvdat)

#Remove length info
data.table::setkey(survdat,
       CRUISE6,
       STATION,
       STRATUM,
       SVSPP)

catch <- unique(survdat, by = key(survdat))

catch <- catch[SVSPP %in% c(22:28, 72, 69), ]
catch <- survdat[SVSPP %in% c(22:28, 72, 69), ]
#Calculate ratios within Stat Areas
#Post stratify to use Stat Area designations
#Stat Areas
pathToGIS <- system.file("extdata","Statistical_Areas_2010.shp",package="comlandr")
Stat.areas <- rgdal::readOGR(pathToGIS)

catch.stat <- Survdat::poststrat(catch, stratum = Stat.areas, strata.col = 'Id')

data.table::setnames(catch.stat,
        c("STRATUM",   "newstrata"),
        c("SVSTRATUM", "AREA"))

data.table::setkey(catch.stat,
       YEAR,
       SEASON,
       AREA)

#SKATES-----------------------------------------------------------------------------
#Figure out proportion of skates
skates <- catch.stat[SVSPP %in% 22:28, sum(BIOMASS), by = key(catch.stat)]
data.table::setnames(skates, "V1", "skates.all")

little <- catch.stat[SVSPP == 26, sum(BIOMASS), by = key(catch.stat)]
data.table::setnames(little, "V1", "little")

all.skates <- merge(skates, little, by = key(catch.stat), all = T)

winter <- catch.stat[SVSPP == 23, sum(BIOMASS), by = key(catch.stat)]
data.table::setnames(winter, "V1", "winter")

all.skates <- merge(all.skates, winter, by = key(catch.stat), all = T)

all.skates[, little.per := little/skates.all]
all.skates[, winter.per := winter/skates.all]

all.skates[, c('skates.all', 'little', 'winter') := NULL]
all.skates[is.na(little.per), little.per := 0]
all.skates[is.na(winter.per), winter.per := 0]

#HAKES--------------------------------------------------------------------------------
#Figure out proportion of silver hake/offshore hake
hakes <- catch.stat[SVSPP %in% c(72, 69), sum(BIOMASS), by = key(catch.stat)]
data.table::setnames(hakes, "V1", "hakes.all")

silvers <- catch.stat[SVSPP == 72, sum(BIOMASS), by = key(catch.stat)]
data.table::setnames(silvers, "V1", "silver")

all.hakes <- merge(hakes, silvers, all = T)

all.hakes[, silver.per := silver/hakes.all]

all.hakes[, c('hakes.all', 'silver') := NULL]
all.hakes[is.na(silver.per), silver.per := 0]

#Combine skates and hakes
skate.hake <- merge(all.skates, all.hakes, by = key(catch.stat), all = T)

skate.hake[SEASON == 'SPRING', Half := 1]
skate.hake[SEASON == 'FALL',   Half := 2]
skate.hake[, SEASON := NULL]

if(Stand.alone == T) save(skate.hake, file = file.path(out.dir, "skates_hakes.RData", sep =''))
#if(Stand.alone == F) skate.hake.us <- skate.hake
skate.hake.us <- skate.hake




#Foreign Landings--------------------------------------------------------------------
#NAFO uses divisions

catch.stat[, EPU := factor(NA, levels = c('GOM', 'GB', 'MAB', 'SS'))]
catch.stat[AREA %in% EPUS$GOM$statAreas, EPU := 'GOM']
catch.stat[AREA %in% EPUS$GB$statAreas,  EPU := 'GB']
catch.stat[AREA %in% EPUS$MAB$statAreas, EPU := 'MAB']
catch.stat[AREA %in% EPUS$SS$statAreas,  EPU := 'SS']



data.table::setkey(catch.stat,
       YEAR,
       SEASON,
       EPU)



#Figure out proportion of skates
skates <- catch.stat[SVSPP %in% 22:28, sum(BIOMASS), by = key(catch.stat)]
data.table::setnames(skates, "V1", "skates.all")

little <- catch.stat[SVSPP == 26, sum(BIOMASS), by = key(catch.stat)]
data.table::setnames(little, "V1", "little")

all.skates <- merge(skates, little, by = key(catch.stat), all = T)

winter <- catch.stat[SVSPP == 23, sum(BIOMASS), by = key(catch.stat)]
data.table::setnames(winter, "V1", "winter")

all.skates <- merge(all.skates, winter, by = key(catch.stat), all = T)

all.skates[, little.per := little/skates.all]
all.skates[, winter.per := winter/skates.all]

all.skates[, c('skates.all', 'little', 'winter') := NULL]
all.skates[is.na(little.per), little.per := 0]
all.skates[is.na(winter.per), winter.per := 0]

#Figure out proportion of silver hake
hakes <- catch.stat[SVSPP %in% c(72, 69), sum(BIOMASS), by = key(catch.stat)]
data.table::setnames(hakes, "V1", "hakes.all")

silvers <- catch.stat[SVSPP == 72, sum(BIOMASS), by = key(catch.stat)]
data.table::setnames(silvers, "V1", "silver")

all.hakes <- merge(hakes, silvers, all = T)

all.hakes[, silver.per := silver/hakes.all]

all.hakes[, c('hakes.all', 'silver') := NULL]
all.hakes[is.na(silver.per), silver.per := 0]

#Combine skates and hakes
skate.hake <- merge(all.skates, all.hakes, by = key(catch.stat), all = T)

skate.hake[SEASON == 'SPRING', Half := 1]
skate.hake[SEASON == 'FALL',   Half := 2]
skate.hake[, SEASON := NULL]
skate.hake <- skate.hake[!is.na(EPU), ]

if(Stand.alone == T) save(skate.hake, file = file.path(out.dir, "skates_hakes_nafo.RData"))
#if(Stand.alone == F) skate.hake.nafo <- skate.hake
skate.hake.nafo <- skate.hake

return(list(skate.hake.us=skate.hake.us,skate.hake.nafo=skate.hake.nafo))

}
