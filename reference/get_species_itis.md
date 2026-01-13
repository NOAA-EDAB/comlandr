# Extract SPECIES information from CFDBS (SPECIES_ITIS_NE, table)

Extract a list of species names, code, market category, etc from the
NEFSC_GARFO CFDBS_SPECIES_ITIS_NE table

## Usage

``` r
get_species_itis(channel, species = "all", nameType = "common_name")
```

## Arguments

- channel:

  an Object inherited from `ROracle::Oracle`. This object is used to
  connect to communicate with the database engine. (see
  `dbutils::connect_to_database`)

- species:

  A specific species code or set of codes. Either numeric or character
  vector. Defaults to "all" species. Numeric codes (SPECIES_ITIS,
  NESPP4) are converted to VARCHAR2 (6 and 4 characters respectively)
  when creating the sql statement.

- nameType:

  Character string. Upper or lower case. Either "common_name" (default),
  "scientific_name" or "nespp4". Determines which type of name to search
  under.

## Value

A list is returned:

- data:

  containing the result of the executed `$sql` statement

- sql:

  containing the sql call

- colNames:

  a vector of the table's column names

The default sql statement
"`select * from NEFSC_GARFO.cfdbs_SPECIES_ITIS_NE`" is used

## Reference

Use the data dictionary for field name explanations.

## See also

`connect_to_database`

Other get functions:
[`get_areas()`](https://noaa-edab.github.io/comlandr/reference/get_areas.md),
[`get_gears()`](https://noaa-edab.github.io/comlandr/reference/get_gears.md),
[`get_locations()`](https://noaa-edab.github.io/comlandr/reference/get_locations.md),
[`get_ports()`](https://noaa-edab.github.io/comlandr/reference/get_ports.md),
[`get_species()`](https://noaa-edab.github.io/comlandr/reference/get_species.md),
[`get_species_stock_area()`](https://noaa-edab.github.io/comlandr/reference/get_species_stock_area.md),
[`get_vessels()`](https://noaa-edab.github.io/comlandr/reference/get_vessels.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# extracts complete species table based on custom sql statement
channel <- connect_to_database(server="name_of_server",uid="individuals_username")
get_species_itis(channel)

# extracts info for cod (164712)
get_species_itis(channel,species=164712)

# extracts info for cod ("COD")
get_species_itis(channel,"cod") #o r
get_species_itis(channel,"co") # or (note also return cockles, calico scallop etc.)
get_species_itis(channel,"COD")

# extracts info for cod ("gadus")
get_species_itis(channel,"gadus",nameType="scientific_name") #o r
get_species_itis(channel,"morh",nameType="scientific_name") #o r
get_species_itis(channel,"GADUS",nameType="scientific_name") #o r

#' # extracts info for cod ("0814") market category 4
get_species_itis(channel,"0814",nameType="NESPP4") #o r
get_species_itis(channel,814,nameType="NESPP4")


# extracts info for cod (164712)  and bluefish (168559)
sqlStatement <- "select * from cfdbs.species_itis_ne"
get_species_itis(channel,species= c("164712","168559"))
} # }
```
