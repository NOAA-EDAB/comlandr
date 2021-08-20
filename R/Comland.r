#' Comland.r Version now controlled by git - originally part of comcatch.r
#'
#'Grab commercial landings data from US and Foreign countries (NAFO)
#'Need to fix menhaden data
#'SML
#'
#'@param channel an Object inherited from \link[DBI]{DBIConnection-class}. This object is used to connect
#' to communicate with the database engine. (see \code{\link{connect_to_database}})
#'@param EPUS List. Designates the stat areas that comprise an EPU. Default = EPUs (lazily loaded data)
#'@param GEARS List. Designates the NEGEAR codes that comprise a fishing fleet. Default = GEARs (lazily loaded data)
#'@param use.existing String. Pull from database "n" or use existing pull "y" (saves time) . Default = "y"
#'@param landed Character String. Use landed weight for scallops and clams or live weight. Default = "y" (meatwt), "n" (livewt)
#'@param foreign Character String. Mark foreign landings and keep seperate. Default = "y"
#'@param adjust.ppi Character String. Adjust value for inflation. Default = "y" (deflated) vs "n" (notdeflated)
#'@param sum.by Character String. Variable to sum landings by either "EPU" (Default) or "stat.area"
#'@param endyear Numeric Scalar. Final year of query. Default = 2018
#'@param reftime Numeric Vector. (Length 2). Specifies the year and month if adjusting for inflation. Default = c(2016,1)
#'@param out.dir Character string. Path to directory where final output will be saved or where data is to be read from
#'@param Stand.alone Boolean. Flag to determine whether to save Skate and hake data to file. defualt = F (Both a US catch file and a NAFO catch file will be saved)
#'
#'
#'@return An RDS file is created
#'
#'
#'A file will be written to your hard drive in the directory specified by \code{out.dir}. The name of the file will be named depending on user input. For example:
#'
#'Filename = 'comland_meatwt_deflated_stat_areas.RDS'
#' arises from user: landed = "y", adjust.ppi = "y", sum.by = "stat.area"
#'
#'@importFrom data.table ":=" "key" "setcolorder" "as.data.table"
#'@importFrom magrittr "%>%"
#'
#'@export
#'
#Make sure to define your fleets below!

#Requires the following files:
# data.dir.2\\Comland_skates_hakes.R
# data.dir\\Menhaden.csv
# data.dir.3\\SS_NAFO_21A.csv
# data.dir.3\\species.txt
comland <- function(channel,
                    GEARS=comlandr::GEARs,
                    EPUS=comlandr::EPUs,
                    use.existing="y",
                    landed="y",
                    foreign="y",
                    adjust.ppi="y",
                    sum.by="EPU",
                    endyear=2018,
                    reftime = c(2016,1),
                    out.dir=here::here(),
                    Stand.alone=F) {

  if(!(isS4(channel))) {
    message("Argument \"channel\", is not a valid DBI connection object. Please see dbutils::connect_to_database for details ...")
    return()
  }

  # informs user as to what he/she has requested since there a lot of options
  #input_checks(use.existing,landed,foreign,adjust.ppi,sum.by,endyear,reftime,out.dir)

  #Output file
  if(landed     == 'n') file.landed <- '_livewt' else file.landed <- '_meatwt'
  if(adjust.ppi == 'n') file.adjust <- '' else file.adjust <- '_deflated'
  if(sum.by == 'EPU') file.by <- '_EPU' else file.by <- '_stat_areas'
  file.name <- paste0('comland', file.landed, file.adjust, file.by)

  refyear <- reftime[1]
  refmonth <- reftime[2]


#  Pull data from databases or read existing ------------------------------
if(use.existing == 'n'){
  # capture the call to the comland function. Added to output
  call <- dbutils::capture_function_call()
  pullDate <- date()
  comland <- get_comland_data(channel,landed,endyear,out.dir)

} else if(use.existing == 'y'){ # or read from directory
  if(landed == 'n') {
    comlandFile <- file.path(out.dir, "comland_raw_US_livewt.RDS")
  } else if (landed == "y") {
    comlandFile <- file.path(out.dir, "comland_raw_US_meatwt.RDS")
  } else {
    stop(paste0("landed = ",landed," is not a valid entry. Please see help for valid argument values"))
  }

  if (!file.exists(comlandFile)) {
      message(paste0("The file, ",comlandFile," doesnt exist. If this is the first time you are running comland.R then you will need to use the argument \"use.existing=\"n\" and pull an initial data set. Fishing data are not provided with this package. Otherwise check to make sure your out.dir path is correct "))
    return()
  } else {
    comland <- readRDS(comlandFile)
    if (any(names(comland) == "pullDate")) {
      pullDate = comland$pullDate
    } else {
      message("Data used is from an older pull where the date of the original data pull was not recorded")
      pullDate <- NULL
    }


  }
}
  comland$YEAR <- as.integer(comland$YEAR)
  comland$MONTH <- as.integer(comland$MONTH)
  comland$NEGEAR <- as.integer(comland$NEGEAR)
  comland$TONCL1 <- as.integer(comland$TONCL1)
  comland$NESPP3 <- as.integer(comland$NESPP3)
  comland$NESPP4 <- as.integer(comland$NESPP4)
  comland$UTILCD <- as.integer(comland$UTILCD)

  comland$AREA <- levels(comland$AREA)[comland$AREA]
  ind <- comland$AREA %in% c("OFF","OFR")
  comland <- comland[!ind,]
  comland$AREA <- as.integer(comland$AREA)
  #comland$AREA <- as.factor(comland$AREA)

  # Convert from lbs to metric tons ----------------------------------------

  comland[, SPPLIVMT := SPPLIVLB * 0.00045359237]
  comland[, SPPLIVLB := NULL]
  #fix years
  comland[YEAR < 100, YEAR := YEAR + 1900L]
  #comland$YEAR <- as.character(comland$YEAR)

  # Adjust for inflation ----------------------------------------------------
  if(adjust.ppi == 'y'){
    # Adjust SPPVALUE for inflation
    comland <- adjust_inflation(comland,refyear,refmonth)
  }

  # Remove market categories of parts --------------------------------------
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


  # winter & little skates --------------------------------------------------

  #comland.skates <- comland_winter_little(comland,skate.hake.us)
  comland <- comland_winter_little(comland,skate.hake.us)

  # comland_separate_skates -------------------------------------------------
  comland <- comland_separate_hakes(comland,skate.hake.us)

  # Herring --------------------------------------------------------------
  #Herring data is housed by the state of Maine.
  comland <- comland_herring(channel,comland)

  # Menhaden -------------------------------------------------------------
  #fix menhaden records - data from Tom Miller/ Andre Bouchheister
  comland <- comland_menhaden(comland)

  # Deal with unknowns  -------------------------------------
  #1 - drop unknown species/landings
  comland <- comland_unknowns(comland)

  #2 - aggregate by quarter year, half year, major gear, and small/large TC
  comland.agg <- comland_aggregate(comland,GEARS)

  #3 - Use proportions of known catch to assign unknown catch
  #3.A QY/HY------------------------------------------------------------------------------
  comland.agg <- assign_catch_qy_hy(comland.agg)

  #3.B SIZE------------------------------------------------------------------------------
  comland.agg <- assign_catch_size(comland.agg)


  #3.C GEAR------------------------------------------------------------------------------
  comland.agg <- assign_catch_gear(comland.agg)

  #3.D AREA------------------------------------------------------------------------------
  comland.agg <- assign_catch_area(comland.agg)

  # NAFO data processed -----------------------------------------------------
  nafoland.agg <- comland_nafo(channel,skate.hake.nafo,GEARS)

  # aggregate by EPU  --------------------------------------------------------
  if(sum.by == 'EPU'){
    #Assign EPU based on statarea
    comland <- aggregate_by_epu(comland.agg,nafoland.agg,EPUS,foreign)

  } else if (sum.by == 'stat.area') {
    comland <- comland.agg
  } else {
    stop(paste0("sum.by = ",sum.by," has not been coded for. Select either EPU or stat.area"))
  }

  comland <- list(data=comland, pullDate=pullDate, functionCall = call)

  #save(comland, file = file.path(out.dir, paste0(file.name,Sys.Date(),".RData")))
  saveRDS(comland, file = file.path(out.dir, paste0(file.name,".Rds")))

  return(comland)

}
