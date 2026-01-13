# Extract VESSEL information from CFDBS

Extract a list of vessell ID's, tonnage, crew size, home port, etc from
the NEFSC "Mstrvess" supporting table

## Usage

``` r
get_vessels(channel)
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

If no `sqlStatement` is provided the default sql statement
"`select * from NEFSC_GARFO.cfdbs_mstrvess`" is used

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
[`get_species_stock_area()`](https://noaa-edab.github.io/comlandr/reference/get_species_stock_area.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# extracts complete vessel table based on custom sql statement
channel <- connect_to_database(server="name_of_server",uid="individuals_username")
get_vessels(channel)

} # }
```
