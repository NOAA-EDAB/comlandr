# Extracts raw commercial data from Database

Pulls raw commercial fishing data from the Stock Efficiency (STOCKEFF)
database.

## Usage

``` r
get_comland_raw_data(
  channel,
  filterByYear = NA,
  filterByArea = NA,
  useLanded = T,
  removeParts = T
)
```

## Arguments

- channel:

  an Object inherited from `ROracle::Oracle`. This object is used to
  connect to communicate with the database engine. (see
  `dbutils::connect_to_database`)

- filterByYear:

  numeric vector. Years to be retrieved (Default = NA, pull all years)

- filterByArea:

  numeric vector. Statistical Areas to be retrieved (Default = NA, pull
  all areas)

- useLanded:

  boolean. Use landed or live weight for shellfish (Default = T, landed)

- removeParts:

  boolean. Remove species parts (Heads, wings, etc), Default = T

## Value

List of 2 objects:

Data frame (data.table) (n x 12)

- comland:

  Each row of the data.table defines a group of trips fishing in the
  same YEAR, MONTH, AREA using the same NEGEAR and MESH, on the same
  sized vessel, TONCL2, catching species (NESPP3) with MARKET_CODE for
  the same purpose (UTILCD). The sum of the landings and value are
  returned for each 'group'

- YEAR:

  Year of trips

- MONTH:

  Month of trips

- NEGEAR:

  Three digit Fishing gear code used on trips

- TONCL2:

  Two digit Tonnage class code of the fishing vessel

- NESPP3:

  Three digit Species code

- AREA:

  Statistical area in which species was reportly caught

- UTILCD:

  Utilization code. Eg. The utilization code: 0=food fish or unknown;
  2=aquaculture; 3=canned pet food (1984+); 4=Biomedical (2002+);
  5=animal food (1984+); 7=bait; 8=industrial, other (2002+);
  9=industrial, reduction.

- MARKET_CODE:

  Species market code (2 characters)

- SPPVALUE:

  The value of landed catch to the nearest dollar (U.S.), paid to
  fisherman by dealer, for a given species.

- MESHCAT:

  Code to describe the mesh size for a trawl vessel (`SM`,`LG`,`NA`)

- SPPLIVMT:

  Weight in metric tons.

- US:

  Landing from the USA vessels or foreign vessels

Character string:

- sql:

  Defines the SQL query used to pull the data

## Details

\*For `useLanded` = T. Shellfish species (NESPP3 codes = 743:800) return
landed weight rather than live weight (`useLanded` = F)

\*Mesh category (`MESHCAT`) is split into two categories defined as
small (`SM` where `MESH` \<= 3) or large (`LG` where `MESH` \>3)

\*A conversion from pounds (LBS) to Metric Tons (MT) is applied
internally (1 LB = 0.00045359237 MT)

\*Some fish parts are removed (NESPP4 codes = '0119', '0123', '0125',
'0127', '0812', '0819', '0828', '0829', '1731', '2351', '2690', '2699',
'3472', "3488" "3498" "3508" "3518" "3528" "3538" "3548" "3558" "3568"
"3578" "3588" "3598", '3868', "4694" "4704" "4714", "4808" "4818" "4828"
"4838" "4848" "4858" "4868" "4878" "4888" "4898" "4908" "4918" "4928",
"4938" "4948" "4958" "4968" "4978" "4988" "4998",'5018', '5039', '5261',
'5265')

## Examples

``` r
if (FALSE) { # \dontrun{
# connect to database
channel <- dbutils::connect_to_database(server ="serverName", uid="userName")
# Pull data for years 2001 and 2002
rawData <- get_comland_raw_data(channel,filterByYear = 2001:2002)
} # }
```
