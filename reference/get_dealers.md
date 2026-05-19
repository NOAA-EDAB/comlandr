# Extract Dealers location details

Extract a list of dealer names and locations from supporting tables

## Usage

``` r
get_dealers(channel, state = NA, year = NA)
```

## Arguments

- channel:

  an Object inherited from `ROracle::Oracle`. This object is used to
  connect to communicate with the database engine. (see
  `dbutils::connect_to_database`)

- state:

  Character vector. State abbreviations. Default = NA (All states)

- year:

  Numeric vector. Years in which to pull data. Defaul = NA (All years)

## Value

A list is returned:

- data:

  containing the result of the executed `sqlStatement`

- sql:

  containing the sql call

- colNames:

  a vector of the table's column names

If no `sqlStatement` is provided the default sql statement
"`select * from NEFSC_GARFO.PERMIT_DEALER`" is used

## Reference

Use the data dictionary for field name explanations

## See also

`connect_to_database`

Other get functions:
[`get_areas()`](https://noaa-edab.github.io/comlandr/reference/get_areas.md),
[`get_gears()`](https://noaa-edab.github.io/comlandr/reference/get_gears.md),
[`get_locations()`](https://noaa-edab.github.io/comlandr/reference/get_locations.md),
[`get_ports()`](https://noaa-edab.github.io/comlandr/reference/get_ports.md),
[`get_species()`](https://noaa-edab.github.io/comlandr/reference/get_species.md),
[`get_species_itis()`](https://noaa-edab.github.io/comlandr/reference/get_species_itis.md),
[`get_species_stock_area()`](https://noaa-edab.github.io/comlandr/reference/get_species_stock_area.md),
[`get_vessels()`](https://noaa-edab.github.io/comlandr/reference/get_vessels.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# extracts dealer location table
channel <- connect_to_database(server="name_of_server",uid="individuals_username")
get_dealers(channel)

# extracts dealer details for states, Massachusetts and Maine
get_dealers(channel, state = c("MA","ME"))

# extracts dealer details for years 2010 to 2020
get_dealers(channel, year = 2010:2020)

# extracts dealer details for years 2010 to 2020 for MA & ME
get_dealers(channel, state = c("MA","ME"), year = 2010:2020)
} # }
```
