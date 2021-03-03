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

get_comland_raw_data <- function(channel, filterByYear = NA, useLanded = T, 
                                 removeParts = T){
  
  message("Pulling landings data from database. This could take a while (> 1 hour) ... ")
  
  #Generate vector of tables to loop through
  if(any(filterByYear < 1964)) stop("Landings data start in 1964")
  
  tables <- as.numeric(c(substr(filterByYear[which(filterByYear <= 1993)], 3, 4),
              filterByYear[which(filterByYear >  1993)]))
  tables[which(tables > 1993)] <- paste0('CFDETS',  tables[which(tables > 1993)], 'AA')
  tables[which(tables > 63 & tables <= 81)] <- paste0('WOLANDS', tables[which(tables > 63 & tables <= 81)])
  tables[which(tables > 81 & tables <= 93)] <- paste0('WODETS',  tables[which(tables > 81 & tables <= 93)])
  
  #output objects
  comland <- c()
  sql <- c()
  
  for(itab in 1:length(tables)){
    #Data query
    landings.qry <- paste("select year, month, negear, toncl1, nespp3, nespp4, area,
                           spplivlb, spplndlb, sppvalue, utilcd
                           from", tables[itab])
    sql <- c(sql, landings.qry)

    comland.yr <- data.table::as.data.table(DBI::dbGetQuery(channel, landings.qry))

    # Use landed weight instead of live weight for shellfish
    if(useLanded) {comland.yr[NESPP3 %in% 743:800, SPPLIVLB := SPPLNDLB]}
    
    # Remove fish parts so live weight is not double counted
    if(removeParts){
      comland.yr <- comland.yr[!NESPP4 %in% c(119, 123, 125, 127, 812, 819, 828, 
                                              829, 1731, 2351, 2690, 2699, 3472,
                                              as.numeric(paste0(348:359, 8)), 3868, 
                                              as.numeric(paste0(469:471, 4)),
                                              as.numeric(paste0(480:499, 8)), 5018, 
                                              5039, 5261, 5265), ]
    }
    
    #Sum landings and value
    data.table::setkey(comland.yr,
                       YEAR,
                       MONTH,
                       NEGEAR,
                       TONCL1,
                       NESPP3,
                       AREA,
                       UTILCD)
    #landings
    comland.yr[, V1 := sum(SPPLIVLB, na.rm = T), by = key(comland.yr)]
    #value
    comland.yr[, V2 := sum(SPPVALUE, na.rm = T), by = key(comland.yr)]

    #Remove extra rows/columns
    comland.yr <- unique(comland.yr, by = key(comland.yr))
    comland.yr[, c('SPPLIVLB', 'SPPLNDLB', 'SPPVALUE', 'NESPP4') := NULL]

    #Rename summed columns
    data.table::setnames(comland.yr, c('V1', 'V2'), c('SPPLIVLB', 'SPPVALUE'))

    comland <- data.table::rbindlist(list(comland, comland.yr))
    
    message("Pulled data from ",tables[itab]," ...")
    
    }

  #Convert number fields from chr to num
  numberCols <- c('YEAR', 'MONTH', 'NEGEAR', 'TONCL1', 'NESPP3', 'UTILCD', 'AREA')
  comland[, (numberCols):= lapply(.SD, as.numeric), .SDcols = numberCols][]
  
  #Adjust pounds to metric tons
  comland[, SPPLIVMT := SPPLIVLB * 0.00045359237]
  comland[, SPPLIVLB := NULL]
  
  #standardize YEAR field
  comland[YEAR < 100, YEAR := YEAR + 1900L]

  
  return(list(comland = comland[], 
              sql     = sql))
}

