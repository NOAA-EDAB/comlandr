# Extract fishing GEAR types from CFDBS

Extract a list of gear types in the NEFSC "GEAR" supporting table

## Usage

``` r
get_gears(channel, gears = "all")
```

## Arguments

- channel:

  an Object inherited from `ROracle::Oracle`. This object is used to
  connect to communicate with the database engine. (see
  `dbutils::connect_to_database`)

- gears:

  specific gear code or set of codes. Either numeric or character
  vector. Defaults to "all" gears. Numeric codes are converted to
  VARCHAR2(2 BYTE) when creating the sql statement. Character codes are
  short character strings referencing GEARNM field.

## Value

A list is returned:

- data:

  containing the result of the executed `sqlStatement`

- sql:

  containing the sql call

- colNames:

  a vector of the table's column names

If no `sqlStatement` is provided the default sql statement
"`select * from NEFSC_GARFO.cfdbs_gear`" is used

## Reference

Use the data dictionary for field name explanations

## See also

`connect_to_database`

Other get functions:
[`get_areas()`](https://noaa-edab.github.io/comlandr/reference/get_areas.md),
[`get_locations()`](https://noaa-edab.github.io/comlandr/reference/get_locations.md),
[`get_ports()`](https://noaa-edab.github.io/comlandr/reference/get_ports.md),
[`get_species()`](https://noaa-edab.github.io/comlandr/reference/get_species.md),
[`get_species_itis()`](https://noaa-edab.github.io/comlandr/reference/get_species_itis.md),
[`get_species_stock_area()`](https://noaa-edab.github.io/comlandr/reference/get_species_stock_area.md),
[`get_vessels()`](https://noaa-edab.github.io/comlandr/reference/get_vessels.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# extracts gear data from cfdbs.gear table based on the default \code{sqlStatement}
channel <- connect_to_database(server="name_of_server",uid="individuals_username")
get_gears(channel)

# extracts info based on gear types (5,6) (numeric)
get_gears(channel,gears=c(5,6))

# extracts info based on gear types (5,6) (character)
get_gears(channel,gears=c("05","06"))

# extracts info for "Seines"
get_gears(channel,"seines")

} # }
```
