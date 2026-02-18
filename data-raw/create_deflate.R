#Area for all species
library(data.table)
library(usethis)
library(here)

inflate <- data.table::as.data.table(read.delim(here::here(
  'data-raw',
  'wp.data.3.ProcessedFoods'
)))

inflate[, series_id := gsub(" ", "", inflate[, series_id])]
deflate <- inflate[series_id == "WPU0223", ]
deflate[, MONTH := as.numeric(substr(period, 2, 3))]
data.table::setnames(deflate, c('year', 'value'), c('YEAR', 'PPI'))
deflate <- deflate[, list(YEAR, MONTH, PPI)]

usethis::use_data(deflate)
