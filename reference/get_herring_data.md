# Pull Herring data

Herring Data comes from the state of Maine. The underlying oracle table
is updated periodically, often when an assessment is required. The
herring data from CAMS/STOCKEFF is incomplete. This is used in its place
Pulled from NEFSC_GARFO.maine_herring_catch

## Usage

``` r
get_herring_data(channel, filterByYear = NA, filterByArea = NA)
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

## Value

Herring data as a data frame
