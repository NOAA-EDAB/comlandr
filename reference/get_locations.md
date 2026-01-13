# Extract LOCATION information from CFDBS

Extract a list of lat, long, ten minute square, etc from the NEFSC "loc"
supporting table

## Usage

``` r
get_locations(channel)
```

## Arguments

- channel:

  an Object inherited from `ROracle::Oracle`. This object is used to
  connect to communicate with the database engine. (see
  `dbutils::connect_to_database`)

## Value

A list is returned:

- data:

  containing the result of the executed `sqlStatement`

- sql:

  containing the `sqlStatement` itself

- colNames:

  a vector of the table's column names

## Reference

Use the data dictionary for field name explanations

## See also

`connect_to_database`

Other get functions:
[`get_areas()`](https://noaa-edab.github.io/comlandr/reference/get_areas.md),
[`get_gears()`](https://noaa-edab.github.io/comlandr/reference/get_gears.md),
[`get_ports()`](https://noaa-edab.github.io/comlandr/reference/get_ports.md),
[`get_species()`](https://noaa-edab.github.io/comlandr/reference/get_species.md),
[`get_species_itis()`](https://noaa-edab.github.io/comlandr/reference/get_species_itis.md),
[`get_species_stock_area()`](https://noaa-edab.github.io/comlandr/reference/get_species_stock_area.md),
[`get_vessels()`](https://noaa-edab.github.io/comlandr/reference/get_vessels.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# extracts complete locations table based on default sql statement
channel <- connect_to_database(server="name_of_server",uid="individuals_username")
get_locations(channel)
} # }
```
