# Extract SPECIES information from CFDBS

Extract a list of speices names, code, market category, etc from the
NEFSC cfspp table

## Usage

``` r
get_species(channel, species = "all")
```

## Arguments

- channel:

  an Object inherited from `ROracle::Oracle`. This object is used to
  connect to communicate with the database engine. (see
  `dbutils::connect_to_database`)

- species:

  a specific species code or set of codes. Either numeric or character
  vector. (NESPP3 codes) Numeric codes are converted to VARCHAR2(3 BYTE)
  when creating the sql statement. A Species common name can also be
  supplied. The character string is used to pull from SPPNM field.
  Defaults to "all" species.

## Value

A list is returned:

- data:

  containing the result of the executed `$sql` statement

- sql:

  containing the sql call

- colNames:

  a vector of the table's column names

The default sql statement "`select * from NEFSC_GARFO.cfdbs_cfspp`" is
used

## Reference

Use the data dictionary for field name explanations. Note: species codes
(NESPP3) are stored in the database as VARCHAR2(3 BYTE)

## See also

`connect_to_database`

Other get functions:
[`get_areas()`](https://noaa-edab.github.io/comlandr/reference/get_areas.md),
[`get_gears()`](https://noaa-edab.github.io/comlandr/reference/get_gears.md),
[`get_locations()`](https://noaa-edab.github.io/comlandr/reference/get_locations.md),
[`get_ports()`](https://noaa-edab.github.io/comlandr/reference/get_ports.md),
[`get_species_itis()`](https://noaa-edab.github.io/comlandr/reference/get_species_itis.md),
[`get_species_stock_area()`](https://noaa-edab.github.io/comlandr/reference/get_species_stock_area.md),
[`get_vessels()`](https://noaa-edab.github.io/comlandr/reference/get_vessels.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# extracts complete species table based on custom sql statement
channel <- connect_to_database(server="name_of_server",uid="individuals_username")
get_species(channel)

# extracts info for cod (081)
get_species(channel,species=81)

# extracts info for cod ("COD")
get_species(channel,"cod")
get_species(channel,"co")
get_species(channel,"COD")


# extracts info for cod (081)  and bluefish (023)
sqlStatement <- "select * from cfdbs.cfspp"
get_species(channel,species= c("081","023"))
} # }
```
