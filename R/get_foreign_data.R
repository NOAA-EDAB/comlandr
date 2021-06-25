#' Downloads all NAFO data
#'
#'Downloads and reads in all NAFO data then aggregates it
#'
#'@return Data frame: NAFO data
#'
#'\item{}{}
#'\item{}{}
#'\item{}{}
#'\item{}{}
#'\item{}{}
#'
#'@importFrom data.table ":=" "key" "setcolorder" "as.data.table"
#'
#' @noRd

comland_nafo <- function(channel){
  #Note - NAFO landings by division only so not available in sum.by = "stat.area"
  #Add NAFO foreign landings - Data from http://www.nafo.int/data/frames/data.html

  files <- data.frame(url = c("https://www.nafo.int/Portals/0/Stats/nafo-21b-60-69.zip",
                              "https://www.nafo.int/Portals/0/Stats/nafo-21b-70-79.zip",
                              "https://www.nafo.int/Portals/0/Stats/nafo-21b-80-89.zip",
                              "https://www.nafo.int/Portals/0/Stats/nafo-21b-90-99.zip",
                              "https://www.nafo.int/Portals/0/Stats/nafo-21b-2000-09.zip",
                              "https://www.nafo.int/Portals/0/Stats/nafo-21b-2010-16.zip"),
                      filename = c("NAFO21B-60-69.txt",
                                   "NAFO21B-70-79.txt",
                                   "NAFO21B-80-89.txt",
                                   "NAFO21B-90-99.txt",
                                   "NAFO21B-2000-09.txt",
                                   "nafo-21b-2010-16/NAFO-21B-2010-16.txt"))


  # get file, catch error for missing file
  nafo <- NULL
  for (ifile in 1:nrow(files)) {
    result <- tryCatch(
      {
        stringParts <- stringr::str_split(files$url[ifile],"/")
        message("Reading file: ",tail(unlist(stringParts),1))
        temp <- tempfile()
        download.file(files$url[ifile],destfile=temp,quiet=TRUE)
        res <- TRUE
      },
      error = function(e){
        message(paste0("No data for ",ay))
        return(FALSE)
      } ,
      warning = function(w) return(FALSE)
    )

    if (!result) { # failed to download file
      message(paste0("File ",files$filename[ifile], "can not be found. Please check the link @ https://www.nafo.int/Data/Catch-Statistics"))
      unlink(temp)
      next
    }

    # Read data
    dataPart <- data.table::as.data.table(read.csv(unz(temp, files$filename[ifile])))
    unlink(temp)

    # make all coumn names consistent over all years data
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

  #Remove US landings (Country code 22) and effort codes (1:3)
  nafo <- nafo[Country != 22 & Code > 3, ]

  #Deal with unknown monthly catch????? The Catches column represent catch that couldn't be assigned to a month

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

  #aggregate nafo landings
  #Aggregate by quarter year
  nafoland[MONTH %in% 1:3,   QY := 1]
  nafoland[MONTH %in% 4:6,   QY := 2]
  nafoland[MONTH %in% 7:9,   QY := 3]
  nafoland[MONTH %in% 10:12, QY := 4]
  nafoland[MONTH == 0,       QY := 1]


  ## Add NESPP3 codes
  speciesInfo <- data.table::as.data.table(comlandr::get_species(channel)$data )
  speciesInfo <- speciesInfo[,list(NESPP3,NAFOSPP)]
  speciesInfo[, NESPP3:= as.integer(NESPP3) ]
  speciesInfo[, NAFOSPP:= as.integer(NAFOSPP)]
  speciesInfo <- speciesInfo %>% dplyr::distinct()

  ###########################################################################
  # caution many to one relationship NESPP3 -> NAFO. Duplicated catch data
  # Fix this
  #############################################################################
  nafoland <- dplyr::left_join(nafoland, speciesInfo, by = c("Code"="NAFOSPP"))

  # change name of NAFO species field
  data.table::setnames(nafoland,"Code", "NAFOCode")



  return(nafoland)
}
