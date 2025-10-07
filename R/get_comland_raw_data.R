#' Extracts raw commercial data from Database
#'
#' @description
#' Pulls raw commercial fishing data from the Stock Efficiency (STOCKEFF) database.
#'
#'@inheritParams get_comland_data
#'
#'@returns List of 2 objects:
#'
#'Data frame (data.table) (n x 12)
#'\item{comland}{Each row of the data.table defines a group of trips fishing in the same YEAR, MONTH, AREA using the same NEGEAR and MESH, on the same sized vessel, TONCL2, catching species (NESPP3) with MARKET_CODE for the same purpose (UTILCD). The sum of the landings and value are returned for each 'group'}
#'\item{YEAR}{Year of trips}
#'\item{MONTH}{Month of trips}
#'\item{NEGEAR}{Three digit Fishing gear code used on trips}
#'\item{TONCL2}{Two digit Tonnage class code of the fishing vessel}
#'\item{NESPP3}{Three digit Species code}
#'\item{AREA}{Statistical area in which species was reportly caught}
#'\item{UTILCD}{Utilization code. Eg. The utilization code: 0=food fish or unknown; 2=aquaculture; 3=canned pet food (1984+); 4=Biomedical (2002+); 5=animal food (1984+); 7=bait; 8=industrial, other (2002+); 9=industrial, reduction.}
#'\item{MARKET_CODE}{Species market code (2 characters)}
#'\item{SPPVALUE}{The value of landed catch to the nearest dollar (U.S.), paid to fisherman by dealer, for a given species.}
#'\item{MESHCAT}{Code to describe the mesh size for a trawl vessel (\code{SM},\code{LG},\code{NA}) }
#'\item{SPPLIVMT}{Weight in metric tons.}
#'\item{US}{Landing from the USA vessels or foreign vessels}
#'
#'Character string:
#'
#' \item{sql}{Defines the SQL query used to pull the data}
#'
#'@section Details:
#' *For \code{useLanded} = T. Shellfish species (NESPP3 codes = 743:800) return landed weight rather than live weight (\code{useLanded} = F)
#'
#' *Mesh category (\code{MESHCAT}) is split into two categories defined as small (\code{SM} where \code{MESH} <= 3) or large (\code{LG} where \code{MESH} >3)
#'
#' *A conversion from pounds (LBS) to Metric Tons (MT) is applied internally (1 LB = 0.00045359237 MT)
#'
#' *Some fish parts are removed (NESPP4 codes = '0119', '0123', '0125', '0127', '0812', '0819', '0828', '0829', '1731', '2351',
#' '2690', '2699', '3472', "3488" "3498" "3508" "3518" "3528" "3538" "3548" "3558" "3568" "3578" "3588" "3598",
#' '3868', "4694" "4704" "4714", "4808" "4818" "4828" "4838" "4848" "4858" "4868" "4878" "4888" "4898" "4908" "4918" "4928",
#' "4938" "4948" "4958" "4968" "4978" "4988" "4998",'5018', '5039', '5261', '5265')
#'
#'@examples
#'\dontrun{
#' # connect to database
#'channel <- dbutils::connect_to_database(server ="serverName", uid="userName")
#' # Pull data for years 2001 and 2002
#'rawData <- get_comland_raw_data(channel,filterByYear = 2001:2002)
#'}
#'
#'@export

get_comland_raw_data <- function(
  channel,
  filterByYear = NA,
  filterByArea = NA,
  useLanded = T,
  removeParts = T
) {
  #If not specifying a year default to 1964 - current year
  currentYear <- as.numeric(format(Sys.Date(), "%Y"))
  if (is.na(filterByYear[1])) {
    filterByYear <- 1964:currentYear
  }
  filteryears <- sqltext(filterByYear)

  message(paste0(
    "Pulling landings data from ",
    filterByYear[1],
    " to ",
    filterByYear[length(filterByYear)],
    ". This could take a while (> 1 hour) ... "
  ))

  # #Generate vector of tables to loop through
  if (any(filterByYear < 1964)) {
    stop("Landings data start in 1964")
  }

  #Use Stockeff table

  landings.qry <- paste(
    "select year, month, negear, toncl2, nespp3, nespp4, area,
                    utilcd, mesh, market_code, sum(spplivlb) as spplivlb,
                    sum(spplndlb) as spplndlb, sum(sppvalue) as sppvalue
                    from stockeff.mv_cf_landings
                    where year in (",
    filteryears,
    ")
                    group by year, month, negear, toncl2, nespp3, nespp4, area,
                    utilcd, mesh, market_code"
  )
  comland <- data.table::as.data.table(DBI::dbGetQuery(channel, landings.qry))

  sql <- landings.qry

  #Identify small/large mesh fisheries
  comland[MESH <= 3, MESHCAT := 'SM']
  comland[MESH > 3, MESHCAT := 'LG']
  comland[, MESH := NULL]

  # Use landed weight instead of live weight for shellfish
  if (useLanded) {
    comland[NESPP3 %in% 743:800, SPPLIVLB := SPPLNDLB]
  }
  comland[, SPPLNDLB := NULL]

  # Remove fish parts so live weight is not double counted
  if (removeParts) {
    comland <- comland[
      !NESPP4 %in%
        c(
          '0119',
          '0123',
          '0125',
          '0127',
          '0812',
          '0819',
          '0828',
          '0829',
          '1731',
          '2351',
          '2690',
          '2699',
          '3472',
          paste0(348:359, 8),
          '3868',
          paste0(469:471, 4),
          paste0(480:499, 8),
          '5018',
          '5039',
          '5261',
          '5265'
        ),
    ]
  }

  #Remove NESPP4
  comland[, NESPP4 := NULL]

  #Convert number fields from chr to num
  numberCols <- c(
    'YEAR',
    'MONTH',
    'NEGEAR',
    'TONCL2',
    'NESPP3',
    'UTILCD',
    'AREA'
  )
  comland[, (numberCols) := lapply(.SD, as.numeric), .SDcols = numberCols][]

  #Adjust pounds to metric tons
  comland[, SPPLIVMT := SPPLIVLB * 0.00045359237]
  comland[, SPPLIVLB := NULL]

  # #standardize YEAR field
  # comland[YEAR < 100, YEAR := YEAR + 1900L]

  #Add Nationality Flag
  comland[, US := T]

  return(list(comland = comland[], sql = sql))
}
