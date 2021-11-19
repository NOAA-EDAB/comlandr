#Aggregate areas table

#The proportion of catch inside and outside was calculated from the MS Keyrun 
#project for Georges Bank.  
library(here); library(data.table)

mskeyAreas <- readRDS(here::here('data-raw/All_Species_Proportions.rds')) 
mskeyAreas[InOut == 'in', EPU := 'GB']
mskeyAreas[AREA %in% c(521, 522, 551, 561) & InOut == 'out', EPU := 'GOM']  
mskeyAreas[AREA %in% c(526, 537, 538)      & InOut == 'out', EPU := 'MAB']
mskeyAreas[is.na(EPU), EPU := 'Other']

#Drop InOut column
mskeyAreas[, InOut := NULL]

#Output to package
usethis::use_data(mskeyAreas, overwrite = TRUE)