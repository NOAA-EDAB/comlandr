# Extracts observer data from Database

Connects to obdbs and pulls fields from OBSPP, OBINC, ASMSPP, and ASMINC

## Usage

``` r
get_comdisc_raw_data(channel, filterByYear)
```

## Arguments

- channel:

  an Object inherited from `ROracle::Oracle`. This object is used to
  connect to communicate with the database engine. (see
  `dbutils::connect_to_database`)

- filterByYear:

  numeric vector. Years to be retrieved (Default = NA, pull all years)

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
