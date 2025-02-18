#' Extracts observer data from Database
#'
#' Connects to obdbs and pulls fields from OBSPP, OBINC, ASMSPP, and ASMINC
#'
#'@inheritParams get_comland_data
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
#'@export

get_comdisc_raw_data <- function(channel, filterByYear){

  message("Pulling observer data from database. This could take a while (> 1 hour) ... ")

  #output objects
  comdisc <- c()
  sql <- c()

  #Create year vector
  if(is.na(filterByYear[1])){
    years <- ">= 1989"
  }else{
    years <- paste0("in (", sqltext(filterByYear), ")")
  }

  ob.qry <- paste0("select year, month, area, negear, nespp4, hailwt, catdisp, drflag,
          tripid, haulnum, lathbeg, lonhbeg, link3
          from obdbs.OBSPP
          where obsrflag = 1
          and program not in ('127', '900', '250', '160')
          and year ", years,
                   "\n union
          select year, month, area, negear, nespp4, hailwt, catdisp, drflag,
          tripid, haulnum, lathbeg, lonhbeg, link3
          from obdbs.ASMSPP
          where obsrflag = 1
          and program not in ('127', '900', '250', '160')
          and year ", years)

  ob <- data.table::as.data.table(DBI::dbGetQuery(channel, ob.qry))
  sql <- c(sql, ob.qry)

  #Add protected species here
  mammal.qry <- paste0("select distinct a.year, a.month, b.area, b.negear, a.nespp4,
               1 as hailwt, 0 as catdisp, 1 as drflag, a.tripid, a.haulnum,
               b.lathbeg, b.lonhbeg, a.link3
               from obdbs.obinc a, obdbs.obspp b
               where a.tripid = b.tripid
               and a.year ", years,
                       "\n union
               select distinct a.year, a.month, b.area, b.negear, a.nespp4,
               1 as hailwt, 0 as catdisp, 1 as drflag, a.tripid, a.haulnum,
               b.lathbeg, b.lonhbeg, a.link3
               from obdbs.asminc a, obdbs.asmspp b
               where a.tripid = b.tripid
               and a.year ", years)

  mammal <- data.table::as.data.table(DBI::dbGetQuery(channel, mammal.qry))
  sql <- c(sql, mammal.qry)

  ob <- data.table::rbindlist(list(ob, mammal))

  #Grab otter trawl gear tables to get mesh size for small verses large mesh
  mesh.qry <- paste0("select link3, codmsize
             from obdbs.OBOTGH
             where year ", years)
  mesh <- data.table::as.data.table(DBI::dbGetQuery(channel, mesh.qry))
  sql <- c(sql, mesh.qry)

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
  ob <- ob[!(NESPP4 %in% c(0, 6800:6802, 6805, 6810, 6820, 6830, 6850:6857, 6882,
                           6883, 6894:6897))]

  #Convert lat/lon to decimal degrees
  ob[, LAT := as.numeric(substr(LATHBEG, 1, 2)) + ((as.numeric(substr(LATHBEG, 3, 4))
                                                  + as.numeric(substr(LATHBEG, 5, 6)))
                                                  /60)]
  ob[, LON := (as.numeric(substr(LONHBEG, 1, 2)) + ((as.numeric(substr(LONHBEG, 3, 4))
                                                  + as.numeric(substr(LONHBEG, 5, 6)))
                                                  /60)) * -1]
  ob[, c('LATHBEG', 'LONHBEG') := NULL]

  #Convert weights
  convert.qry <- "select nespp4_obs, catdisp_code, drflag_code, cf_lndlb_livlb, cf_rptqty_lndlb
                from obdbs.obspecconv"
  convert <- data.table::as.data.table(DBI::dbGetQuery(channel, convert.qry))
  sql <- c(sql, convert.qry)

  data.table::setnames(convert,
           c('NESPP4_OBS', 'CATDISP_CODE', 'DRFLAG_CODE'),
           c('NESPP4',     'CATDISP',      'DRFLAG'))

  data.table::setkey(convert,
         NESPP4,
         CATDISP,
         DRFLAG)

  ob.code <- merge(ob, convert, by = key(convert), all.x = T)

  #missing cf's will be set to 1 Assume living
  ob.code[is.na(CF_LNDLB_LIVLB), CF_LNDLB_LIVLB := 1]
  ob.code[is.na(CF_RPTQTY_LNDLB), CF_RPTQTY_LNDLB := 1]

  ob.code[, C.HAILWT := HAILWT * CF_RPTQTY_LNDLB * CF_LNDLB_LIVLB]

  #Grab PR flags
  prflag.qry <- "select NESPP4, cetacean, turtle, pinniped
                from obdbs.obspec"

  prflag <- data.table::as.data.table(DBI::dbGetQuery(channel, prflag.qry))
  sql <- c(sql, prflag.qry)

  prflag[CETACEAN == 1 | TURTLE == 1 | PINNIPED == 1, PR := 1]
  prflag[is.na(PR), PR := 0]
  prflag[, c('CETACEAN', 'TURTLE', 'PINNIPED') := NULL]

  # data.table::merge???
  comdisc <- merge(ob.code, prflag, by = 'NESPP4', all.x = T)

  #Convert to metric tons to align with commercial landings data
  comdisc[PR == 0, SPPLIVMT := C.HAILWT * 0.00045359237]

  #Change to NESPP3 to combine market categories
  comdisc[PR == 0, NESPP3 := substring(NESPP4, 1, 3)]
  #Birds, mammals, etc don't have unique NESPP3 codes
  comdisc[is.na(NESPP3), NESPP3 := NESPP4]

  comdisc[PR == 0, MKTCAT := as.numeric(substring(NESPP4, 4, 4))]
  comdisc[is.na(MKTCAT), MKTCAT := 0]

  #drop extra columns NESPP4
  comdisc[, c('DRFLAG', 'CF_LNDLB_LIVLB', 'CF_RPTQTY_LNDLB', 'HAILWT', 'C.HAILWT',
              'NESPP4') := NULL]

  #Convert number fields from chr to num
  numberCols <- c('YEAR', 'MONTH', 'NEGEAR', 'NESPP3', 'AREA', 'MKTCAT')
  comdisc[, (numberCols):= lapply(.SD, as.numeric), .SDcols = numberCols][]

  return(list(comdisc = comdisc[],
              sql     = sql))
}

