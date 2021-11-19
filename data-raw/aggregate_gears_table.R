#Aggregate gears table

#Gear designations from the MS Keyrun project for Georges Bank.  

library(data.table)

#Create Gear table
mskeyGears <- data.table(NEGEAR2 = c(5, 
                                     5, 16, 32, 35, 36,
                                     1, 2, 8, 10, 50, 52, 14, 26,
                                     12, 17, 37,
                                     18, 15, 19, 20, 21, 23, 30, 33, 53,
                                     13,
                                     40,
                                     22, 25, 38, 41,
                                     3, 4, 6, 11),
                         MESHCAT = c('SM', rep('LG', 5), rep(NA, 30)),
                         Fleet = c('SM Mesh',
                                   rep('LG Mesh', 5),
                                   rep('Fixed Gear', 8),
                                   rep('Pelagic', 3),
                                   rep('Trap', 9),
                                   'Scallop Dredge',
                                   'Clam Dredge',
                                   rep('Other Dredge', 4),
                                   rep('HMS', 4)))

#Output to package
usethis::use_data(mskeyGears, overwrite = TRUE)