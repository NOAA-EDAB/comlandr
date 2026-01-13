# Extracts commercial discard data from Database

Connects to obdbs and pulls discard data, calculates discard to kept
ratios, and applies to landings data obtained using `get_comland_data`.

## Usage

``` r
get_comdisc_data(
  channel,
  comland,
  aggArea = F,
  areaDescription = "EPU",
  propDescription = "MeanProp",
  aggGear = F,
  fleetDescription = "Fleet",
  extendTS = T
)
```

## Arguments

- channel:

  an Object inherited from `ROracle::Oracle`. This object is used to
  connect to communicate with the database engine. (see
  `dbutils::connect_to_database`)

- comland:

  Data frame. Result of `get_comland_data`

- aggArea:

  boolean. Aggregate Statistical Areas into larger spatial units
  (Default = F)

- areaDescription:

  character string. Field name in `userAreas` denoting spatial unit.
  (Default = "EPU")

- propDescription:

  character string. Field name in `userAreas` denoting the scaling
  factor. (Default = "MeanProp")

- aggGear:

  boolean. Aggregate NEGEAR codes to larger "fleets" (Default = F)

- fleetDescription:

  character string. Field name in `userGears` denoting Fleet. (Default =
  "Fleet")

- extendTS:

  Boolean. Should the DK (Discard to kept) ratio be extended and applied
  to landings beyond observer coverage time period (Discards started in
  1989). Default = T

## Value

Data frame (data.table) (n x 10) Each row of the data.table represents a
species record for a given tow/trip

- YEAR:

  Year of trip/tow

- MONTH:

  Month of trip/tow

- NEGEAR:

  Fishing gear used on trip/tow

- TONCL1:

  Tonnage class of the fishing vessel

- NESPP3:

  Species code (3 charachters)

- NESPP4:

  Species code and market code (4 characters)

- AREA:

  Statistical area in which species was reportly caught

- UTILCD:

  Utilization code

- SPPLIVLB:

  live weight (landed = "n") or landed weight (landed="y") in lbs

- SPPVALUE:

  The value of landed catch to the nearest dollar (U.S.), paid to
  fisherman by dealer, for a given species.
