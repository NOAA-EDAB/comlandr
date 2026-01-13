# Extracts and processes commercial data from Database

Connects to Population dynamics Database STOCKEFF to pull US landings
data. Data is also pulled from NAFO (foreign landings) and the State of
Maine (Herring). These sources of data are then aggregated, species
value is adjusted to a user defined reference year, skate and hake
landings (often reported as an unclassified category) are split based on
bottom trawl survey, and missing values are imputed. For more
information regarding these methods see
[`vignette("Overview")`](https://noaa-edab.github.io/comlandr/articles/Overview.md)

## Usage

``` r
get_comland_data(
  channel,
  filterByYear = NA,
  filterByArea = NA,
  useLanded = T,
  removeParts = T,
  useHerringMaine = T,
  useForeign = T,
  refYear = NA,
  refMonth = NA,
  disagSkatesHakes = T,
  aggArea = F,
  userAreas = comlandr::mskeyAreas,
  areaDescription = "EPU",
  propDescription = "MeanProp",
  applyProp = F,
  aggGear = F,
  userGears = comlandr::mskeyGears,
  fleetDescription = "Fleet",
  unkVar = c("MONTH", "NEGEAR", "AREA"),
  knStrata = c("HY", "QY", "MONTH", "NEGEAR", "TONCL2", "AREA")
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

- useHerringMaine:

  boolean. Pull data from Maine Herring database or use herring data in
  commercial landings database (Default = T)

- useForeign:

  boolean. Pull foreign data from NAFO. (Default = T)

- refYear:

  numeric. Reference year to use when adjusting species value

- refMonth:

  numeric. Reference month to use when adjusting species value

- disagSkatesHakes:

  boolean. Partition skates and hake unclassified landings into species
  (Default = T)

- aggArea:

  boolean. Aggregate Statistical Areas into larger spatial units
  (Default = F)

- userAreas:

  data frame. Spatial units in which Statistical areas should be
  aggregated (eg.
  [`mskeyAreas`](https://noaa-edab.github.io/comlandr/reference/mskeyAreas.md))

- areaDescription:

  character string. Field name in `userAreas` denoting spatial unit.
  (Default = "EPU")

- propDescription:

  character string. Field name in `userAreas` denoting the scaling
  factor. (Default = "MeanProp")

- applyProp:

  boolean. Apply the proportions in userAreas to the landings and value
  (Default = T)

- aggGear:

  boolean. Aggregate NEGEAR codes to larger "fleets" (Default = F)

- userGears:

  data frame. Fleet designations in which NEGEAR codes should be grouped
  (eg.
  [`mskeyGears`](https://noaa-edab.github.io/comlandr/reference/mskeyGears.md))

- fleetDescription:

  character string. Field name in `userGears` denoting Fleet. (Default =
  "Fleet")

- unkVar:

  character vector. Variables in the data, with have missing values,
  that you wish to assign a value to. (unkVar = NULL skips assigning
  unknowns)

- knStrata:

  character vector. Variables in the data that you wish to use to use to
  assign values to `unkVar`

## Value

A list of 3 objects

- Data frame (data.table) (n x 12)

&nbsp;

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

- NEGEAR/Fleet:

  Fishing gear used on trips or aggregated to Fleet

- TONCL2:

  Tonnage class of the fishing vessel (2 digit value)

- NESPP3:

  Species code (3 characters)

- MARKET_CODE:

  Market code or species caught (2 characters)

- MESHCAT:

  Code to describe the mesh size for a trawl vessel

- AREA/EPU:

  Statistical area/ Ecological Production Unit in which species was
  reportly caught

- UTILCD:

  Utilization code. Eg. The utilization code: 0=food fish or unknown;
  2=aquaculture; 3=canned pet food (1984+); 4=Biomedical (2002+);
  5=animal food (1984+); 7=bait; 8=industrial, other (2002+);
  9=industrial, reduction.

- US:

  Landing from the USA vessels or foreign vessels

- SPPLIVMT:

  Weight in metric tons.

- SPPVALUE:

  The value of landed catch to the nearest dollar (U.S.), paid to
  fisherman by dealer, for a given species.

&nbsp;

- Character string

&nbsp;

- sql:

  Defines the SQL query used to pull the data

&nbsp;

- Function call

&nbsp;

- call:

  The function call used to create the data

## Argument choices

Some of the arguments rely on the choice of others.

If `aggArea = T` then the user must also supply a `userAreas` data frame
and a `areaDescription` string to denote the field in `userArea` which
maps the statistical area to the larger spatial unit.

If `aggGear = T` then the user must also supply a `userGears` data frame
and a `fleetDescription` string to denote the field in `userGears` which
maps the NEGEAR codes to the fleet designation.
