# Pull Herring data

Herring Data comes from the state of Maine and replaces the herring data
from StockEff (since it is incomplete). Pulled from
NEFSC_GARFO.maine_herring_catch

## Usage

``` r
get_herring_data(channel, comland, filterByYear, filterByArea, useForeign)
```

## Arguments

- channel:

  an Object inherited from `ROracle::Oracle`. This object is used to
  connect to communicate with the database engine. (see
  `dbutils::connect_to_database`)

- comland:

  Data frame. master data frame containing species landings

- filterByYear:

  numeric vector. Years to be retrieved (Default = NA, pull all years)

- filterByArea:

  numeric vector. Statistical Areas to be retrieved (Default = NA, pull
  all areas)

- useForeign:

  boolean. Pull foreign data from NAFO. (Default = T)

## Value

Processed Herring data added to comland
