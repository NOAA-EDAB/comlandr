# comlandr 1.0.0

A significant overhaul to the layout and structure of the package.
Many changes are internal and will not be apparent to a user

## Major changes

* Reworked algorithm to assign unknowns. This operation now takes place immediately following data pull
* Changed names and reorganzied many internal functions
* Added functions to estimate discards
* Added lazy loaded datasets from mskeyrun project and price data
* Switched underlying data source. From WODETS, WOLANDS, CFDETS to STOCKEFF (which pulls from these earlier data sources along with CAMS)

# comlandr 0.3.0

Standalone functionality for pulling NAFO data

`get_foreign_data` - pulls data from NAFOs STATLANT 21B database

# comlandr 0.2.0

Added supporting `get` functions to pull information from supporting oracle tables

`get_areas` - pull stat area, region, NAFO codes

`get_gears` - pull gear type, codes, description

`get_vessels` - pull tonnage, crew size

`get_locations` - pull lat, lon, 10 min sq.

`get_ports` - pull port names and locations

`get_species` - pull species name, market code information

`get_species_itis` - pull species itis information


# comlandr 0.1.0

Initial release 

