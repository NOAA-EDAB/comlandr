# Extract Statistical Areas that comprise species stock definition

Extract a list of statistical areas from STOCKEFF supporting table

## Usage

``` r
get_species_stock_area(channel, species = "all", stock_name = NULL)
```

## Arguments

- channel:

  an Object inherited from `ROracle::Oracle`. This object is used to
  connect to communicate with the database engine. (see
  `dbutils::connect_to_database`)

- species:

  A specific species code or set of codes. Either numeric or character
  vector. Defaults to "all" species. Numeric codes (SPECIES_ITIS) are
  converted to VARCHAR2 (6) when creating the sql statement.

- stock_name:

  Character string. Upper or lower case. The abbreviated name of the
  stock (default = NULL, pulls all stocks). For Example "GBK", "EGOM"

## Value

A list is returned:

- data:

  containing the result of the executed `sqlStatement`

- sql:

  containing the sql call

- colNames:

  a vector of the table's column names

The default sql statement
"`select * from STOCKEFF.V_CF_STOCK_RECENT_STAT_AREA_O`" is used

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
[`get_vessels()`](https://noaa-edab.github.io/comlandr/reference/get_vessels.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# extracts complete area table based on default sql statement
channel <- connect_to_database(server="name_of_server",uid="individuals_username")
get_species_stock_area(channel)

# extracts info for cod (164712)
get_species_stock_area(channel,species=164712)

# extracts info for cod ("COD"). All stocks
get_species_stock_area(channel,"cod")
get_species_stock_area(channel,"co")
get_species_stock_area(channel,"COD")

# extracts info for cod (GBK stock)
get_species_stock_area(channel,"COD", stock_name = "GBK")
get_species_stock_area(channel,"CO", stock_name = "gbk")
get_species_stock_area(channel,"164712", stock_name = "EGOM")
get_species_stock_area(channel,164712, stock_name = "egom")

# extracts info for cod (164712)  and bluefish (168559)
sqlStatement <- "select * from cfdbs.species_itis_ne"
get_species_stock_area(channel,species= c("164712","168559"))

} # }
```
