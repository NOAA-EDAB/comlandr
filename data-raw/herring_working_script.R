# Metadata ----

### Project name: update get_herring_data
### Code purpose: create two functions, one for herring data pull and one for merge with comlandr

### Author: AT
### Date started: 2025-03-20

### Code reviewer:
### Date reviewed:

# Libraries & functions ----
devtools::install_github("andybeet/dbutils")
devtools::install_github("noaa-edab/comlandr")
devtools::load_all()

# Data ----
comland <- readRDS(here::here("../comlandr_data/comland.rds"))
herr.catch <- readRDS(here::here("../comlandr_data/herringAll.rds"))

source(here::here("../../Abigail.Tyrell/channel.R"))

# Analyses ----

herr.catch$data |>
  dplyr::filter(is.na(STOCK_AREA))
# these NAs are legit

herr.catch$data |>
  dplyr::filter(is.na(GEARNAME))
unique(herr.catch$data$GEARNAME) # GEARNAME is not numeric

# testing outputs ----

source(here::here("R/get_herring_data.R"))

# original method -- pull comland, then get_herring
# comlandr_data <- comlandr::get_comland_data(channel,filterByYear = 2020:2023)
# data pull not working, using data Andy provided instead
# comlandr_data <- comland$comland |>
#   dplyr::filter(YEAR %in% 2020:2023)

orig <- get_herring_data_orig(channel = channel,
                      comland = comland,
                      filterByYear = NA,
                      filterByArea = NA,
                      useForeign = TRUE)
orig2 <- orig$comland |>
  dplyr::filter(NESPP3 == 168) |>
  tibble::as_tibble()

# new method -- pull herring, pull comland, then combine

herring_pull <- get_herring_data2(channel = channel,
                                  filterByYear = NA,
                                  filterByArea = NA,
                                  useForeign = TRUE)

herring_combined <- merge_herring_data(herring = herring_pull,
                                       comland = comland)

test <- herring_combined$data |>
  dplyr::filter(NESPP3 == 168) |>
  dplyr::rename(AREA = STOCK_AREA)
str(test)
str(orig2)
str(herring_pull$data)

test[,(test |>
        colnames() |>
        sort())]

orig2[,(orig2 |>
        colnames() |>
        sort())]

dplyr::anti_join(test, 
                 orig2) |>
  dplyr::select(colnames(test)) |>
  View()

dplyr::anti_join(orig2, 
                 test) |>
  dplyr::select(colnames(test)) |>
  View()

full_herring |> 
  dplyr::filter(YEAR == 1985, MONTH == 7, STOCK_AREA == 513)
                