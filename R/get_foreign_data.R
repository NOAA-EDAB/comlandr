#' Downloads all NAFO data
#'
#' Downloads, imports, aggregates NAFO data from 21B data base
#'
#' @param filterByYear Numeric vector. Years for which data is required
#' @param filterByArea Character vector. NAFO Areas for which data is required
#'@param removeUSA Boolean. Should USA landings be removed from data set? (Default = T, remove)
#'@param aggregateCountry Boolean. Should all catch be aggregated over country codes? (Default = T)
#'
#'@return Data frame: NAFO data
#'
#'\item{Year}{Year of catch}
#'\item{MONTH}{Month of catch}
#'\item{QY}{Quatere year of catch. Jan-Mar = 1, ..., Oct-Dec = 4}
#'\item{GearCode}{NAFO gear code}
#'\item{Tonnage}{Size class of vessel}
#'\item{DivCode}{Division code in which vessel reported catch}
#'\item{Code}{NAFO species code of landed fish}
#'\item{SPPLIVMT}{catch in Metric tons}
#'\item{Country}{Reporting country - only if \code{aggregateCounty = F}}
#'
#'@importFrom data.table ":=" "key" "setcolorder" "as.data.table"
#'
#'@seealso
#' NAFO 21B website: \url{https://www.nafo.int/Data/Catch-Statistics-STATLANT-21B}
#'
#'@examples
#'nafoData <- get_foreign_data(filterByYear = 2000)
#'head(nafoData)
#'
#' @export

get_foreign_data <- function(filterByYear=NA,filterByArea=NA,removeUSA = T, aggregateCountry = T){
  #Note - NAFO landings by division only so not available in sum.by = "stat.area"
  #Add NAFO foreign landings - Data from http://www.nafo.int/data/frames/data.html

  files <- data.frame(url = c("https://www.nafo.int/Portals/0/Stats/nafo-21b-60-69.zip",
                              "https://www.nafo.int/Portals/0/Stats/nafo-21b-70-79.zip",
                              "https://www.nafo.int/Portals/0/Stats/nafo-21b-80-89.zip",
                              "https://www.nafo.int/Portals/0/Stats/nafo-21b-90-99.zip",
                              "https://www.nafo.int/Portals/0/Stats/nafo-21b-2000-09.zip",
                              "https://www.nafo.int/Portals/0/Stats/nafo-21b-2010-19.zip",
                              "https://www.nafo.int/Portals/0/Stats/nafo-21b-2020-23.zip"),
                      filename = c("NAFO21B-60-69.txt",
                                   "NAFO21B-70-79.txt",
                                   "NAFO21B-80-89.txt",
                                   "NAFO21B-90-99.txt",
                                   "NAFO21B-2000-09.txt",
                                   "NAFO-21B-2010-2019.txt",
                                   "NAFO-21B-2020-2023.txt"),
                      startyr = c(1960,1970,1980,1990,2000,2010,2020),
                      endyr = c(1969,1979,1989,1999,2009,2019,2023),
                      stringsAsFactors = FALSE)


  # Only read in files for the years requested
  if (any(is.na(filterByYear))){
    filesToReadStart <- 1 # read in all
    filesToReadEnd <- nrow(files)
  } else {
    st <- (min(filterByYear) >= files$startyr) & (min(filterByYear) <= files$endyr)
    fin <- (max(filterByYear) >= files$startyr) & (max(filterByYear) <= files$endyr)

    if (all(st == F)) {
      filesToReadStart <- 1
    } else {
      filesToReadStart <- which(as.logical(st))
    }

    if (all(fin == F)) {
      filesToReadEnd <- nrow(files)
    } else {
      filesToReadEnd <- which(as.logical(fin))
    }
  }

  # get file, catch error for missing file
  nafo <- NULL
  for (ifile in filesToReadStart:filesToReadEnd) {
    result <- tryCatch(
      {
        stringParts <- stringr::str_split(files$url[ifile],"/")
        message("Reading file: ",tail(unlist(stringParts),1))
        temp <- base::tempfile()
        download.file(url=files$url[ifile],destfile=temp, quiet=TRUE)
        res <- TRUE
      },
      error = function(e){
        message(e)
        return(FALSE)
      } ,
      warning = function(w){
        return(FALSE)
      }
    )

    if (!result) { # failed to download file
      message(paste0("File ",files$url[ifile], " can not be downloaded. Please check the link @ https://www.nafo.int/Data/Catch-Statistics"))
      base::unlink(temp)
      next
    }

    # Read data
    dataPart <- data.table::as.data.table(read.csv(unz(temp, files$filename[ifile])))
    base::unlink(temp)

    # make all column names consistent over all years data
    # 2010 + data have different column headers.
    # Use names from 1960
    if(any(names(dataPart)=="Gear")){ # found in more recent years
      data.table::setnames(dataPart,
                         c('Gear', 'AreaCode', 'SpeciesEffort'),
                         c('GearCode', 'Divcode', 'Code'))
    }
    if(any(names(dataPart)=="Month_NK")){
      data.table::setnames(dataPart,'Month_NK','Catches')
    }

    # bind all years data into a large data frame
    nafo <- rbind(nafo,dataPart)

  }


  # Remove US landings (Country code 22)
  if (removeUSA) {
    nafo <- nafo[Country != 22, ]
  }

  #Remove effort codes (1:3)
  nafo <- nafo[Code > 3, ]

  #Deal with unknown monthly catch????? The Catches column represent catch that couldn't be assigned to a month

  #Get nafo code in a similar format to comland
  nafoland <- nafo[, list(Year, GearCode, Tonnage, Divcode, Country, Code, Catches)]
  # unknown monthly catch resides in "Catches" field. Assign as Month = 0
  nafoland[, MONTH := 0]
  data.table::setnames(nafoland, 'Catches', 'SPPLIVMT')


  month <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
  for(i in 1:12){
    nafoland.month <- nafo[, list(Year, GearCode, Tonnage, Divcode, Country, Code, get(month[i]))]
    nafoland.month[, MONTH := i]
    data.table::setnames(nafoland.month,
                         names(nafoland.month)[7],
                         'SPPLIVMT')
    nafoland <- data.table::rbindlist(list(nafoland, nafoland.month))
  }

  #aggregate nafo landings
  #Aggregate by quarter year
  nafoland[MONTH %in% 1:3,   QY := 1]
  nafoland[MONTH %in% 4:6,   QY := 2]
  nafoland[MONTH %in% 7:9,   QY := 3]
  nafoland[MONTH %in% 10:12, QY := 4]
  nafoland[MONTH == 0,       QY := 1] # Catches for Unknown MONTH

  # convert weight from character to integer.
  # Filter out nonsense values inherent in dataset
  nafoland <- nafoland %>%
    dplyr::mutate(TEMP = dplyr::case_when(grepl("[^e]-",SPPLIVMT) ~ as.integer(NA),
                                       SPPLIVMT == "" ~ as.integer(NA),
                                       is.na(SPPLIVMT) ~ as.integer(NA),
                                       TRUE ~ as.integer(1))) %>%
    dplyr::filter(!is.na(TEMP)) %>%
    dplyr::select(-TEMP) %>%
    dplyr::mutate(SPPLIVMT = as.integer(SPPLIVMT)) %>%
    data.table::as.data.table()

  # set NA's in monthly catch to zero
  nafoland[is.na(SPPLIVMT), SPPLIVMT := 0]


  # aggregate over country
  if (aggregateCountry) {
    nafoland <- nafoland %>%
      dplyr::group_by(Year,GearCode,Tonnage,Divcode,Code,MONTH,QY) %>%
      dplyr::summarise(SPPLIVMT=sum(as.numeric(SPPLIVMT)),.groups="drop") %>%
      data.table::as.data.table(.)
  }


  # Filter data pull based on user inputs, years, areas
  if (all(!is.na(filterByYear))) {
    nafoland <- nafoland %>%
      dplyr::filter(Year %in% filterByYear)
  }
  if (all(!is.na(filterByArea))) {
    nafoland <- nafoland %>%
      dplyr::filter(Divcode %in% filterByArea)
  }



  return(nafoland[])
}
