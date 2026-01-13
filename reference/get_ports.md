# Extract PORT location information from CFDBS

Extract a list of port names, and location info for vessel landings from
the NEFSC "Port" supporting table

## Usage

``` r
get_ports(channel, ports = "all")
```

## Arguments

- channel:

  an Object inherited from `ROracle::Oracle`. This object is used to
  connect to communicate with the database engine. (see
  `dbutils::connect_to_database`)

- ports:

  a specific port code or set of codes. Either numeric or character
  vector. Defaults to "all" ports. Numeric codes are converted to
  VARCHAR2(6 BYTE) when creating the sql statement. Character codes are
  short character strings referencing PORTNM field.

## Value

A list is returned:

- data:

  containing the result of the executed `sqlStatement`

- sql:

  containing the sql call

- colNames:

  a vector of the table's column names

If no `sqlStatement` is provided the default sql statement
"`select * from NEFSC_GARFO.cfdbs_port`" is used

## Reference

Use the data dictionary for field name explanations

## See also

`connect_to_database`

Other get functions:
[`get_areas()`](https://noaa-edab.github.io/comlandr/reference/get_areas.md),
[`get_gears()`](https://noaa-edab.github.io/comlandr/reference/get_gears.md),
[`get_locations()`](https://noaa-edab.github.io/comlandr/reference/get_locations.md),
[`get_species()`](https://noaa-edab.github.io/comlandr/reference/get_species.md),
[`get_species_itis()`](https://noaa-edab.github.io/comlandr/reference/get_species_itis.md),
[`get_species_stock_area()`](https://noaa-edab.github.io/comlandr/reference/get_species_stock_area.md),
[`get_vessels()`](https://noaa-edab.github.io/comlandr/reference/get_vessels.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# extracts complete port table based on default sql statement
channel <- connect_to_database(server="name_of_server",uid="individuals_username")
get_ports(channel)

# extracts port details based on ports 224309 , 224409 (numeric)
get_ports(channel,ports = c(224309,224409))

# extracts port details based on ports 224309 , 224409 (character)
get_ports(channel,ports = c("224309","224409"))

# extracts port details based on port name (character)
get_ports(channel,ports = "Fairfield")
get_ports(channel,ports = "Fair")

} # }
```
