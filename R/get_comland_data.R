#' Extracts commercial data from Database
#'
#' Connects to cfdbs and pulls fields from WOLANDS, WODETS, CFDETS
#'
#'@param channel an Object inherited from \link[DBI]{DBIConnection-class}. This object is used to connect
#' to communicate with the database engine. (see \code{\link[dbutils]{connect_to_database}})
#'@param endyear Numeric Scalar. Final year of query.
#'@param landed Character String. Use landed weight ("y" - meatwt) for scallops and clams instead of live weight ("n" - livewt).
#'@param out.dir path to directory where final output will be saved
#'
#'
#'@return Data frame (data.table) (n x 10)
#'Each row of the data.table represents a species record for a given tow/trip
#'
#'\item{YEAR}{Year of trip/tow}
#'\item{MONTH}{Month of trip/tow}
#'\item{NEGEAR}{Fishing gear used on trip/tow}
#'\item{TONCL1}{Tonnage class of the fishing vessel}
#'\item{NESPP3}{Species code (3 charachters)}
#'\item{NESPP4}{Species code and market code (4 characters)}
#'\item{AREA}{Statistical area in which species was reportly caught}
#'\item{UTILCD}{Utilization code}
#'\item{SPPLIVLB}{live weight (landed = "n") or landed weight (landed="y") in lbs}
#'\item{SPPVALUE}{The value of landed catch to the nearest dollar (U.S.), paid to fisherman by dealer, for a given species.}
#'
#'@section File Creation:
#'
#'A file containing the data.table above will also be saved to the users machine in the directory provided
#'
#'@export

get_comland_data <- function(channel,landed,endyear,out.dir) {


  message("Pulling landings data from database. This could take a while (> 1 hour) ... ")
  #Landings
  tables <- c(paste0('WOLANDS', 64:81),
              paste0('WODETS',  82:93),
              paste0('CFDETS',  1994:endyear, 'AA'))

  #Generate one table
  comland <- c()
  for(i in 1:length(tables)){
    landings.qry <- paste("select year, month, negear, toncl1, nespp3, nespp4, area,
                           spplivlb, spplndlb, sppvalue, utilcd
                           from", tables[i])

    comland.yr <- as.data.table(DBI::dbGetQuery(channel, landings.qry))

    data.table::setkey(comland.yr,
                       YEAR,
                       MONTH,
                       NEGEAR,
                       TONCL1,
                       NESPP3,
                       NESPP4,
                       AREA,
                       UTILCD)

    message("Pulled data from ",tables[i]," ...")

    # Use landed weight instead of live weight for shellfish
    if(landed == 'y') {comland.yr[NESPP3 %in% 743:800, SPPLIVLB := SPPLNDLB]}

    #Sum landings and value
    #landings
    comland.yr[, V1 := sum(SPPLIVLB), by = key(comland.yr)]
    #value
    #Fix null values
    comland.yr[is.na(SPPVALUE), SPPVALUE := 0]
    comland.yr[, V2 := sum(SPPVALUE), by = key(comland.yr)]

    #Remove extra rows/columns
    comland.yr <- unique(comland.yr, by = key(comland.yr))
    comland.yr[, c('SPPLIVLB', 'SPPLNDLB', 'SPPVALUE') := NULL]

    #Rename summed columns
    data.table::setnames(comland.yr, c('V1', 'V2'), c('SPPLIVLB', 'SPPVALUE'))

    comland <- data.table::rbindlist(list(comland, comland.yr))
  }

  # save in RODBC format
  comland$YEAR <- as.integer(comland$YEAR)
  comland$MONTH <- as.integer(comland$MONTH)
  comland$NEGEAR <- as.integer(comland$NEGEAR)
  comland$TONCL1 <- as.integer(comland$TONCL1)
  comland$NESPP3 <- as.integer(comland$NESPP3)
  comland$NESPP4 <- as.integer(comland$NESPP4)
  comland$UTILCD <- as.integer(comland$UTILCD)
  comland$AREA <- as.factor(comland$AREA)



  # Save file.
  if(landed == 'n') saveRDS(comland, file = file.path(out.dir, paste0("comland_raw_US_livewt.RDS")))
  if(landed == 'y') saveRDS(comland, file = file.path(out.dir, paste0("comland_raw_US_meatwt.RDS")))

  return(comland)

}

