# Extract AREA information from CFDBS

Extract a list of statistical areas, region, NAFO codes, etc from the
NEFSC "Area" supporting table

## Usage

``` r
get_areas(channel, areas = "all")
```

## Arguments

- channel:

  an Object inherited from `ROracle::Oracle`. This object is used to
  connect to communicate with the database engine. (see
  `dbutils::connect_to_database`)

- areas:

  a specific area code or set of codes. Either numeric or character
  vector. Defaults to "all" areas Numeric codes are converted to
  VARCHAR2(3 BYTE) when creating the sql statement. Character codes are
  short character strings to reference the AREANM field.

## Value

A list is returned:

- data:

  containing the result of the executed `sqlStatement`

- sql:

  containing the sql call

- colNames:

  a vector of the table's column names

The default sql statement "`select * from NEFSC_GARFO.cfdbs_area`" is
used

## Reference

Use the data dictionary for field name explanations

## See also

`connect_to_database`

Other get functions:
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
# extracts complete area table based on default sql statement
channel <- connect_to_database(server="name_of_server",uid="individuals_username")
get_areas(channel)

# extracts a subset of area data based on selected areas 100,500 (numeric)
get_areas(channel,areas=c(100,500))

# extracts a subset of area data based on selected areas 100,500 (character)
get_areas(channel,areas=c("100","500"))

# extracts a subset of area data based on areanm's containing "GG" (Androscoggin River etc)
get_areas(channel,"GG")

} # }
```
