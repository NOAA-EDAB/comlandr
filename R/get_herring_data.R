

# new herring data pull function ----

#' Pull Herring data
#'
#' Herring Data comes from the state of Maine and replaces the herring data from StockEff (since
#' it is incomplete). Pulled from NEFSC_GARFO.maine_herring_catch
#'
#' @inheritParams get_comland_data
#' @param 
#'
#'@return Processed Herring data added to comland
#'
#'
#' @export

get_herring_data2 <- function(channel, 
                              filterByYear, 
                              filterByArea,
                              useForeign) {
  
  #Pulling data
  message("Pulling Atlantic herring data from maine_herring_catch ...")
  
  years <- dplyr::case_when(is.na(filterByYear[1]) ~ ">= 1963",
                            TRUE ~ paste0("in (", survdat:::sqltext(filterByYear), ")"))

  herr.qry <- paste0("select year, month, category, stock_area, negear, gearname,
                     keptmt, discmt
                     from NEFSC_GARFO.maine_herring_catch
                     where year ", years)
  if(!is.na(filterByArea[1])){
    herr.qry <- paste0(herr.qry, " and stock_area in (", survdat:::sqltext(filterByArea), ")
                               order by stock_area")
  }
  
  herr.catch <- tibble::as_tibble(DBI::dbGetQuery(channel, herr.qry))

  #Convert number fields from chr to num
  herr.catch <- herr.catch |>
    dplyr::mutate_at(c('YEAR', 'MONTH', 'STOCK_AREA', 'NEGEAR'), as.numeric)
  # NAs introduced in STOCK_AREA -- validated they are actually missing

  #Aggregate data
  herring <- herr.catch |>
    dplyr::group_by(YEAR, MONTH, CATEGORY, STOCK_AREA, NEGEAR) |>
    dplyr::summarise(SPPLIVMT = sum(KEPTMT, na.rm = TRUE),
                     DISCMT = sum(DISCMT, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(MARKET_CODE = "UN",
                  TONCL2 = 30,
                  MESHCAT = "LG",
                  US = ifelse(CATEGORY != "NAFO", TRUE, FALSE))
  
    if(isFALSE(useForeign)) {
    herring <- herring |>
      dplyr::filter(CATEGORY != "NAFO")
  }
  
  output <- list(data = herring,
                 sql = herr.qry)
}

# merge herring data function ----

#' Merge Herring data
#'
#' Herring Data comes from the state of Maine and replaces the herring data from StockEff (since
#' it is incomplete). 
#'
#'@param comland Data frame. master data frame containing species landings
#'@param herring Data frame. Data pull from `get_herring_data2`
#'
#'@return Processed Herring data added to comland
#'
#' @export

merge_herring_data <- function(herring, 
                               comland#, # filterByYear, filterByArea,
                             #useForeign
                             ) {
  
  new_sql <- c(comland$sql, herring$sql)
  herring <- herring$data

  herring.comland <- comland$comland |>
    dplyr::filter(NESPP3 == "168")
  
  # Impute Price from comland (month+year price per wt) ----
  message("Imputing price from comlandr...")
  herring.price <- herring.comland |>
    dplyr::group_by(YEAR, MONTH) |>
    dplyr::summarise(price = sum(SPPVALUE, na.rm = T) / sum(SPPLIVMT, na.rm = T))
  
  # this is 0 -- but keeping because it was in source code
  price_1964 <- herring.price |>
    dplyr::filter(YEAR == 1964) |>
    dplyr::summarise(price = mean(price, na.rm = TRUE)) |>
    purrr::pluck("price")
  
  herring.price <- herring.price |>
    dplyr::mutate(price = dplyr::case_when(YEAR <= 1963 ~ price_1964,
                                           TRUE ~ price))

  message("Merging in imputed prices...")
  herring <- dplyr::left_join(herring, herring.price) |>
    dplyr::mutate(SPPVALUE = round(price * SPPLIVMT))
  
  # impute utilization from comland ----
  # assign UTILCD based on year, month, cumulative probabilities
  # I don't like this -- the herring data has spatial resolution while the comlandr data does not
  # suggest to assign data to an EPU and include that in the matching criteria
  # or only assign UTILCD in years when only one UTILCD is used 
  # or leave out altogether
  
  # create imputation key based on proportions
  message("Imputing UTILCD from comlandr...")
  herring.util <- herring.comland |>
    dplyr::group_by(YEAR, MONTH, UTILCD) |>
    dplyr::summarise(SPPLIVMT = sum(SPPLIVMT)) |>
    dplyr::ungroup() |>
    dplyr::group_by(YEAR, MONTH) |>
    dplyr::mutate(SPPLIVMT.ALL = sum(SPPLIVMT),
                  Prop = SPPLIVMT/SPPLIVMT.ALL) |>
    dplyr::arrange(YEAR, MONTH, Prop) |>
    dplyr::mutate(cum.prop = cumsum(Prop)) |>
    dplyr::ungroup()
  
  # create proportions in herring data for imputation
  herring <- herring |>
    dplyr::group_by(YEAR, MONTH) |>
    dplyr::mutate(Total = sum(SPPLIVMT),
                  Prop = SPPLIVMT / Total) |>
    dplyr::arrange(YEAR, MONTH, Prop) |>
    dplyr::mutate(cum.prop = cumsum(Prop)) |>
    dplyr::ungroup()
  
  check1 <- nrow(herring)
  
  # merge and keep only the UTILCD associated with the lowest possible key_cum_prop
  message("Merging in imputed UTILCD...")
  full_herring <- dplyr::left_join(herring, 
                                   herring.util |>
                                     dplyr::select(YEAR, MONTH, UTILCD, cum.prop) |>
                                     dplyr::distinct() |>
                                     dplyr::rename(key_cum_prop = cum.prop),
                                   relationship = "many-to-many") 
  # print(nrow(full_herring))
  full_herring <- full_herring |>
    # dplyr::arrange(YEAR, MONTH, cum.prop, key_cum_prop) |>
    dplyr::mutate(cum.prop = ifelse(cum.prop > 0.99, 1, cum.prop),
                  key_cum_prop = ifelse(key_cum_prop > 0.99, 1, key_cum_prop)) |>
    # dplyr::mutate(cum.prop = round(cum.prop, digits = 5),
    #               key_cum_prop = round(key_cum_prop, digits = 5)) |>
    dplyr::filter(cum.prop <= key_cum_prop) |> 
    # this filter isn't happening correctly
    # not assessing the 1's properly
    # issues with rounding?
    tidyr::nest(data = c("UTILCD", "key_cum_prop"))
  
  # this isn't working as intended yet (2025-04-02)
  # it's because of the 0.00001 added in the previous step
  # print(nrow(full_herring))
  full_herring <- full_herring |>
    dplyr::mutate(out = purrr::map(.x = data,
                                   .f = function(.x){
                                     (.x |> dplyr::arrange(key_cum_prop))[1,]
                                   })) |>
    dplyr::select(-data) |>
    tidyr::unnest(cols = out) |>
    dplyr::mutate(NESPP3 = 168#,
                  # US = ifelse(CATEGORY == "NAFO", FALSE, TRUE) 
                  # nationality should already be assigned from the first function
    ) |>
    dplyr::select(-c('Total', 'Prop', 'cum.prop', 'price', 'DISCMT', "CATEGORY", "key_cum_prop"))
  
  check2 <- nrow(full_herring)
  if (check2 < check1) {
    message("Data seems to be getting lost")
  } else if (check2 > check1) {
    message("Data seems to be being added")
  }
  
  # merge cleaned ME herring with comland ----
  message("Merging ME data into comlandr...")
  output <- list(data = dplyr::bind_rows(full_herring, 
                                         comland$comland |>
                                           dplyr::filter(NESPP3 != 168)),
                 sql = new_sql)
  
  return(output)
}

# original function ----

#' Pull Herring data (original function)
#'
#' Herring Data comes from the state of Maine and replaces the herring data from StockEff (since
#' it is incomplete). Pulled from NEFSC_GARFO.maine_herring_catch
#'
#' @inheritParams get_comland_data
#'@param comland Data frame. master data frame containing species landings
#'
#'@return Processed Herring data added to comland
#'
#'@importFrom data.table ":=" "key"
#'
#' @export

get_herring_data_orig <- function(channel, comland, filterByYear, filterByArea,
                             useForeign) {
  
  # TODO: pull herring directly from stockeff with get_comland_data, then apply wrangling
  
  #Pulling data
  message("Pulling Atlantic herring data from maine_herring_catch ...")
  
  if(is.na(filterByYear[1])){
    years <- ">= 1963"
  }else{
    years <- paste0("in (", survdat:::sqltext(filterByYear), ")")
  }
  
  ## this query adds in data from ME
  herr.qry <- paste0("select year, month, category, stock_area, negear, gearname,
                     keptmt, discmt
                     from NEFSC_GARFO.maine_herring_catch
                     where year ", years)
  if(!is.na(filterByArea[1])){
    herr.qry <- paste0(herr.qry, " and stock_area in (", survdat:::sqltext(filterByArea), ")
                               order by stock_area")
  }
  
  sql <- c(comland$sql, herr.qry)
  
  #pull out comland data
  comland <- comland$comland
  # print(head(comland))
  
  herr.catch <- data.table::as.data.table(DBI::dbGetQuery(channel, herr.qry))
  
  #Convert number fields from chr to num
  numberCols <- c('YEAR', 'MONTH', 'STOCK_AREA', 'NEGEAR', 'GEARNAME')
  herr.catch[, (numberCols):= lapply(.SD, as.numeric), .SDcols = numberCols]
  
  #Aggregate data
  data.table::setkey(herr.catch, YEAR, MONTH, CATEGORY, STOCK_AREA, NEGEAR)
  
  herring <- herr.catch[, list(sum(KEPTMT, na.rm = T), sum(DISCMT, na.rm = T)),
                        by = key(herr.catch)]
  
  data.table::setnames(herring, c('STOCK_AREA', 'V1', 'V2'),
                       c('AREA', 'SPPLIVMT', 'DISCMT'))
  
  #Using averages from comland to fill in categories
  # TODO: are these assignments valid?
  herring[, MARKET_CODE := 'UN']
  
  herring[, TONCL2 := 30]
  
  herring[, UTILCD := 0]
  
  herring[, MESHCAT := 'LG']
  
  # message("next line references NESPP3")
  # print(head(comland))
  # TODO: move reference to comland to merge_herring function
  #compute price/utilization from CF tables
  herring.comland <- comland[NESPP3 == 168, ]
  # message("last line referenced NESPP3")
  
  #Price from comland
  herring.price <- herring.comland[, (sum(SPPVALUE, na.rm = T) / sum(SPPLIVMT, na.rm = T)),
                                   by = c('YEAR', 'MONTH')]
  
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
  
  # TODO: this could be moved earlier in function?
  herring[, NESPP3 := 168]
  
  #Add Nationality Flag
  herring[, US := T]
  herring[CATEGORY == 'NAFO', US := F]
  herring[, CATEGORY := NULL]
  
  print(head(herring))
  print(head(comland))
  order_cols <- names(herring)[which(names(herring) %in% names(comland))]
  data.table::setcolorder(herring, order_cols)
  # data.table::setcolorder(herring, names(comland))
  
  #remove herring from data pull and add in Maine numbers
  comland <- data.table::rbindlist(list(comland[NESPP3 != 168, ], herring), fill = TRUE)
  
  # TODO: move this earlier in function?
  #If not grabbing foreign data - remove from data set
  if(!useForeign) comland <- comland[US == T, ]
  
  return(list(comland = comland[],
              sql     = sql))
  
}

# garbage working code, will delete ----

#' Pull Herring data
#'
#' Herring Data comes from the state of Maine and replaces the herring data from StockEff (since
#' it is incomplete). Pulled from NEFSC_GARFO.maine_herring_catch
#'
#' @inheritParams get_comland_data
#'@param comland Data frame. master data frame containing species landings
#'
#'@return Processed Herring data added to comland
#'
#'@importFrom data.table ":=" "key"
#'
#' @export

# get_herring_data <- function(channel, comland, filterByYear, filterByArea,
#                              useForeign) {
#   
#   # TODO: pull herring directly with get_comland_data, then apply wrangling
# 
#   #Pulling data
#   message("Pulling Atlantic herring data from maine_herring_catch ...")
#   
#   years <- dplyr::case_when(is.na(filterByYear[1]) ~ ">= 1963",
#                             TRUE ~ paste0("in (", survdat:::sqltext(filterByYear), ")"))
# 
#   # if(is.na(filterByYear[1])){
#   #   years <- ">= 1963"
#   # }else{
#   #   years <- paste0("in (", survdat:::sqltext(filterByYear), ")")
#   # }
# 
#   ## this query adds in data from ME
#   herr.qry <- paste0("select year, month, category, stock_area, negear, gearname,
#                      keptmt, discmt
#                      from NEFSC_GARFO.maine_herring_catch
#                      where year ", years)
#   if(!is.na(filterByArea[1])){
#     herr.qry <- paste0(herr.qry, " and stock_area in (", survdat:::sqltext(filterByArea), ")
#                                order by stock_area")
#   }
# 
#   sql <- c(comland$sql, herr.qry)
# 
#   #pull out comland data
#   comland <- comland$comland
# 
#   herr.catch <- tibble::as_tibble(DBI::dbGetQuery(channel, herr.qry))
#   # herr.catch <- data.table::as.data.table(herr.catch$data)
# 
#   #Convert number fields from chr to num
#   # numberCols <- c('YEAR', 'MONTH', 'STOCK_AREA', 'NEGEAR', 'GEARNAME')
#   # herr.catch[, (numberCols):= lapply(.SD, as.numeric), .SDcols = numberCols]
#   # GEARNAME is not numeric
#   
#   herr.catch <- herr.catch |>
#     dplyr::mutate_at(c('YEAR', 'MONTH', 'STOCK_AREA', 'NEGEAR'), as.numeric)
#   # NAs introduced in STOCK_AREA -- validated they are actually missing
#   
#   #### stopped here 2025-03-20
# 
#   #Aggregate data
#   # data.table::setkey(herr.catch, YEAR, MONTH, CATEGORY, STOCK_AREA, NEGEAR)
#   # 
#   # herring <- herr.catch[, list(sum(KEPTMT, na.rm = T), sum(DISCMT, na.rm = T)),
#   #                       by = data.table::key(herr.catch)]
#   
# #   data.table::setnames(herring, c('STOCK_AREA', 'V1', 'V2'),
# #                        c('AREA', 'SPPLIVMT', 'DISCMT'))
# 
#   #Using averages from comland to fill in categories
#   # herring[, MARKET_CODE := 'UN']
#   # 
#   # herring[, TONCL2 := 30]
#   # 
#   # herring[, UTILCD := 0]
#   # 
#   # herring[, MESHCAT := 'LG']
#   
#   herring <- herr.catch |>
#     dplyr::group_by(YEAR, MONTH, CATEGORY, STOCK_AREA, NEGEAR) |>
#     dplyr::summarise(SPPLIVMT = sum(KEPTMT, na.rm = TRUE),
#                      DISCMT = sum(DISCMT, na.rm = TRUE)) |>
#     dplyr::ungroup() |>
#     dplyr::mutate(MARKET_CODE = "UN",
#                   TONCL2 = 30,
#                   MESHCAT = "LG")
# 
#   # TODO: move reference to comland to merge_herring function
#   #compute price/utilization from CF tables
#   # herring.comland <- comland$comland[NESPP3 == 168, ]
#   
#   herring.comland <- comland$comland |>
#     dplyr::filter(NESPP3 == "168")
# 
#   #Price from comland
#   # herring.price <- herring.comland[, (sum(SPPVALUE, na.rm = T) / sum(SPPLIVMT, na.rm = T)),
#   #                                  by = c('YEAR', 'MONTH')]
#   
#   herring.price <- herring.comland |>
#     dplyr::group_by(YEAR, MONTH) |>
#     dplyr::summarise(price = sum(SPPVALUE/SPPLIVMT, na.rm = T))
#   
#   # this is 0
#   price_1964 <- herring.price |>
#     dplyr::filter(YEAR == 1964) |>
#     dplyr::summarise(price = mean(price, na.rm = TRUE)) |>
#     purrr::pluck("price")
#   
#   herring.price <- herring.price |>
#     dplyr::mutate(price = dplyr::case_when(YEAR <= 1963 ~ price_1964,
#                                            TRUE ~ price))
#   
# 
#   # data.table::setnames(herring.price, 'V1', 'price')
# 
#   # herring <- merge(herring, herring.price, by = c('YEAR', 'MONTH'), all.x = T)
#   
#   herring <- dplyr::left_join(herring, herring.price) |>
#     dplyr::mutate(SPPVALUE = round(price * SPPLIVMT))
# 
#   #Use 1964 prices for < 1964
#   # herring[YEAR < 1964, price := mean(herring[YEAR == 1964, price])]
#   # #Calculate SPPVALUE from price
#   # herring[, SPPVALUE := round(price * SPPLIVMT)]
#   
#   #Utilization from comland
#   # herring.util <- herring.comland[, sum(SPPLIVMT), by = c('YEAR', 'MONTH', 'UTILCD')]
#   # data.table::setnames(herring.util, 'V1', 'SPPLIVMT')
#   # 
#   # herring.util[, SPPLIVMT.ALL := sum(SPPLIVMT), by = c('YEAR', 'MONTH')]
#   # 
#   # herring.util[, Prop := SPPLIVMT/SPPLIVMT.ALL]
#   # 
#   # data.table::setorder(herring.util, YEAR, MONTH, Prop)
#   # 
#   # herring.util[, cum.prop := cumsum(Prop), by = c('YEAR', 'MONTH')]
#   
#   herring.util <- herring.comland |>
#     dplyr::group_by(YEAR, MONTH, UTILCD) |>
#     dplyr::summarise(SPPLIVMT = sum(SPPLIVMT)) |>
#     dplyr::ungroup() |>
#     dplyr::group_by(YEAR, MONTH) |>
#     dplyr::mutate(SPPLIVMT.ALL = sum(SPPLIVMT),
#                   Prop = SPPLIVMT/SPPLIVMT.ALL) |>
#     dplyr::arrange(YEAR, MONTH, Prop) |>
#     dplyr::mutate(cum.prop = cumsum(Prop)) |>
#     dplyr::ungroup()
#     
# 
#   #Apply proportions to Maine data set
#   #Not pulled all the time - current through 2017
#   # herring[, Total := sum(SPPLIVMT), by = c('YEAR', 'MONTH')]
#   # 
#   # herring[, Prop := SPPLIVMT / Total]
#   # 
#   # data.table::setorder(herring, YEAR, MONTH, Prop)
#   # herring[, cum.prop := cumsum(Prop), by = c('YEAR', 'MONTH')]
#   
#   herring <- herring |>
#     dplyr::group_by(YEAR, MONTH) |>
#     dplyr::mutate(Total = sum(SPPLIVMT),
#                   Prop = SPPLIVMT / Total) |>
#     dplyr::arrange(YEAR, MONTH, Prop) |>
#     dplyr::mutate(cum.prop = cumsum(Prop)) |>
#     dplyr::ungroup()
# 
#   # assign UTILCD based on year, month, cumulative probabilities
#   # I don't like this -- the herring data has spatial resolution while the comlandr data does not
#   # suggest to assign data to an EPU and include that in the matching criteria
#   # or only assign UTILCD in years when only one UTILCD is used 
#   # or leave out altogether
#   for(iyear in unique(herring.util[, YEAR])){
#     for(imonth in unique(herring.util[YEAR == iyear, MONTH])){
#       cum.prop.low <- 0
#       for(iutil in herring.util[YEAR == iyear & MONTH == imonth, UTILCD]){
#         
#         cum.prop.high <- herring.util[YEAR == iyear & MONTH == imonth & UTILCD == iutil, 
#                                       cum.prop]
#         
#         herring[YEAR == iyear & MONTH == imonth & cum.prop <= cum.prop.high & cum.prop > cum.prop.low, 
#                 UTILCD := iutil]
#         
#         cum.prop.low <- cum.prop.high
#       }
#     }
#   }
#   
#   # test <- purrr::map(.x = herring.util |>
#   #                   dplyr::select(YEAR, MONTH, UTILCD, cum.prop) |>
#   #                   dplyr::distinct() |>
#   #              dplyr::group_split(dplyr::row_number(), .keep = FALSE),
#   #             function(.x, 
#   #                      herring_data = herring
#   #                      ){
#   #    
#   #               herring_data <- herring_data |>
#   #                 dplyr::filter(YEAR == .x$YEAR,
#   #                               MONTH == .x$MONTH) |>
#   #                 dplyr::mutate(UTILCD = NA) |>
#   #                 dplyr::mutate(UTILCD = dplyr::case_when(
#   #                   .data$cum.prop <= .x$cum.prop & is.na(.data$UTILCD) ~ .x$UTILCD,
#   #                   TRUE ~ UTILCD
#   #                 )) |>
#   #                 dplyr::filter()
#   #               
#   #               return(herring_data)
#   #               # this is creating an element for each year-month-utilcd
#   #               # which creates duplicates that have different utilcd
#   #               
#   #             })
#   # things are getting duplicated somehow?
#   # test2 <- purrr::list_rbind(test)
#   
#   # key <- herring.util |>
#   #   dplyr::select(YEAR, MONTH, UTILCD, cum.prop) |>
#   #   dplyr::distinct() |>
#   #   dplyr::rename(key_cum_prop = cum.prop)
#   
#   # empty_grid <- tidyr::expand_grid(YEAR = unique(c(herring$YEAR, herring.comland$YEAR)),
#   #                                  MONTH = 1:12)
#   
#   # nest_herring <- herring |>
#   #   dplyr::full_join(empty_grid) |>
#   #   dplyr::group_by(YEAR, MONTH) |>
#   #   tidyr::nest(.key = "herring")
#   # 
#   # nest_key <- herring.util |>
#   #   dplyr::select(YEAR, MONTH, UTILCD, cum.prop) |>
#   #   dplyr::distinct() |>
#   #   dplyr::rename(key_cum_prop = cum.prop) |>
#   #   # expand out 0s to match with ME herring data
#   #   dplyr::full_join(empty_grid) |>
#   #   dplyr::group_by(YEAR, MONTH) |>
#   #   tidyr::nest(.key = "key")
#   
#   # full_data <- dplyr::full_join(nest_herring, nest_key) |>
#   #   dplyr::mutate(joined = purrr::map2(.x = herring, 
#   #                                      .y = key,
#   #                                     .f = ~dplyr::case_when(is.null(.y)[1] ~ .x,
#   #                                           TRUE ~ dplyr::cross_join(.x, .y))
#   #                                           ))
#   # merge to impute UTILCD?
# 
# 
#   full_herring <- dplyr::full_join(herring, 
#                            herring.util |>
#                              dplyr::select(YEAR, MONTH, UTILCD, cum.prop) |>
#                              dplyr::distinct() |>
#                              dplyr::rename(key_cum_prop = cum.prop)) |>
#     # dplyr::arrange(YEAR, MONTH, cum.prop, key_cum_prop) |>
#     dplyr::filter(cum.prop <= key_cum_prop) |>
#     tidyr::nest(data = c("UTILCD", "key_cum_prop")) |>
#     dplyr::mutate(out = purrr::map(.x = data,
#                                    .f = function(.x){
#                                      .x |>
#                                        dplyr::arrange(key_cum_prop) |>
#                                        dplyr::first()
#                                    })) |>
#     dplyr::select(-data) |>
#     tidyr::unnest(cols = out)
# 
#   # test <- purrr::map2(.x = full_data$herring,
#   #                     .y = full_data$key,
#   #                     .f = function(.x, 
#   #                             .y
#   #                    ){
#   #                      
#   #                       if(is.null(.y)) {
#   #                         
#   #                         } else{
#   #                         for(i in 1:nrow(.y)){
#   #                           
#   #                           val <- .y$key_cum_prop[i]
#   #                           
#   #                           .x <- .x |>
#   #                             dplyr::mutate(cum.prop = ifelse(cum.prop <= val & !is.na(cum.prop), 
#   #                                                                    val, 
#   #                                                                    cum.prop))
#   #                           
#   #                       }
#   #                      
#   #                        
#                          # all_dat <- .x |>
#                          #   dplyr::left_join(herring_data) |>
#                          #   dplyr::left_join(key_data[i,]) |>
#                          #   dplyr::filter(cum.prop <= key_cum_prop) |>
#                          #   dplyr::mutate(flag = ifelse(length(unique(UTILCD)) > 1, "duplicates", "ok"))
#                        # }
#                       
#                        # there will still be duplicates here
#                        
#                        # test3[[50]] |>
#                        #   dplyr::left_join(herring) |>
#                        #   dplyr::left_join(key) |>
#                        #   dplyr::filter(cum.prop <= key_cum_prop) |>
#                        #   dplyr::mutate(flag = ifelse(length(unique(UTILCD)) > 1, "duplicates", "ok"))
#                        
#                        # herring_data <- herring_data |>
#                        #   dplyr::filter(YEAR == .x$YEAR,
#                        #                 MONTH == .x$MONTH) |>
#                        #   dplyr::mutate(UTILCD = NA) |>
#                        #   dplyr::mutate(UTILCD = dplyr::case_when(
#                        #     .data$cum.prop <= .x$cum.prop & is.na(.data$UTILCD) ~ .x$UTILCD,
#                        #     TRUE ~ UTILCD
#                        #   )) |>
#                        #   dplyr::filter()
#                        
#                        # return(all_dat)
#                        # this is creating an element for each year-month-utilcd
#                        # which creates duplicates that have different utilcd
#                        
#                      # })
#   # things are getting duplicated somehow?
#   # test2 <- purrr::list_rbind(test)
#   
#   # herring.comland |> 
#   #   dplyr::group_by(YEAR, MONTH, US, EPU, UTILCD) |> 
#   #   dplyr::summarise(n = dplyr::n()) |> 
#   #   dplyr::ungroup() |>
#   #   ggplot2::ggplot(
#   #     ggplot2::aes(x = YEAR, 
#   #                  y = n, 
#   #                  # color = as.factor(MONTH), 
#   #                  color = EPU)) + 
#   #   ggplot2::geom_point() + 
#   #   ggplot2::geom_line() + 
#   #   ggplot2::facet_grid(cols = ggplot2::vars(MONTH),
#   #                       rows = ggplot2::vars(UTILCD)) +
#   #   ggplot2::ylim(c(0,6))
#   # 
#   # herring.util |> 
#   #   # dplyr::group_by(YEAR, MONTH, UTILCD) |> 
#   #   # dplyr::summarise( = dplyr::n()) |> 
#   #   dplyr::ungroup() |>
#   #   ggplot2::ggplot(
#   #     ggplot2::aes(x = YEAR, 
#   #                  y = Prop, 
#   #                  color = as.factor(MONTH)#,
#   #                  # color = EPU
#   #                  )) + 
#   #   ggplot2::geom_point() + 
#   #   ggplot2::geom_line() + 
#   #   ggplot2::facet_grid(cols = ggplot2::vars(EPU),
#   #                       rows = ggplot2::vars(UTILCD)) +
#   #   ggplot2::ylim(c(0,1)) +
#   #   ggplot2::theme_bw()
# 
#   #fix column headings
#   # herring[, c('Total', 'Prop', 'cum.prop', 'price', 'DISCMT') := NULL]
#   
#   full_herring <- full_herring |>
#     dplyr::mutate(NESPP3 = 168,
#                   US = ifelse(CATEGORY == "NAFO", FALSE, TRUE)) |>
#   dplyr::select(-c('Total', 'Prop', 'cum.prop', 'price', 'DISCMT', "CATEGORY"))
#   
#   # TODO: this could be moved earlier in function?
#   # herring[, NESPP3 := 168]
# 
#   #Add Nationality Flag
#   # herring[, US := T]
#   # herring[CATEGORY == 'NAFO', US := F]
#   # herring[, CATEGORY := NULL]
# 
#   # data.table::setcolorder(herring, names(comland))
# 
#   #remove herring from data pull and add in Maine numbers
#   # comland <- data.table::rbindlist(list(comland[NESPP3 != 168, ], herring))
#   
#   output <- dplyr::bind_rows(full_herring, herring.comland)
# 
#   # TODO: move this earlier in function?
#   #If not grabbing foreign data - remove from data set
#   if(!useForeign) {
#     output <- output |>
#     dplyr::filter(US == TRUE)
#   }
#   
#   return(output)
# 
#   return(list(comland = comland[],
#               sql     = sql))
# 
# }
