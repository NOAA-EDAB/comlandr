# Package index

## Get Data

Functions that pull data from the STOCKEFF/CAMS/CFDBS/NAFO databases

- [`get_comland_data()`](https://noaa-edab.github.io/comlandr/reference/get_comland_data.md)
  : Extracts and processes commercial data from Database
- [`get_comland_raw_data()`](https://noaa-edab.github.io/comlandr/reference/get_comland_raw_data.md)
  : Extracts raw commercial data from Database
- [`get_foreign_data()`](https://noaa-edab.github.io/comlandr/reference/get_foreign_data.md)
  : Downloads all NAFO data
- [`get_comdisc_data()`](https://noaa-edab.github.io/comlandr/reference/get_comdisc_data.md)
  : Extracts commercial discard data from Database
- [`get_comdisc_raw_data()`](https://noaa-edab.github.io/comlandr/reference/get_comdisc_raw_data.md)
  : Extracts observer data from Database
- [`get_herring_data()`](https://noaa-edab.github.io/comlandr/reference/get_herring_data.md)
  : Pull Herring data

## Supporting Functions

Functions that pull lookup tables from CFDBS

- [`get_species()`](https://noaa-edab.github.io/comlandr/reference/get_species.md)
  : Extract SPECIES information from CFDBS
- [`get_species_itis()`](https://noaa-edab.github.io/comlandr/reference/get_species_itis.md)
  : Extract SPECIES information from CFDBS (SPECIES_ITIS_NE, table)
- [`get_species_stock_area()`](https://noaa-edab.github.io/comlandr/reference/get_species_stock_area.md)
  : Extract Statistical Areas that comprise species stock definition
- [`get_areas()`](https://noaa-edab.github.io/comlandr/reference/get_areas.md)
  : Extract AREA information from CFDBS
- [`get_gears()`](https://noaa-edab.github.io/comlandr/reference/get_gears.md)
  : Extract fishing GEAR types from CFDBS
- [`get_locations()`](https://noaa-edab.github.io/comlandr/reference/get_locations.md)
  : Extract LOCATION information from CFDBS
- [`get_ports()`](https://noaa-edab.github.io/comlandr/reference/get_ports.md)
  : Extract PORT location information from CFDBS
- [`get_vessels()`](https://noaa-edab.github.io/comlandr/reference/get_vessels.md)
  : Extract VESSEL information from CFDBS

## Data

Lazy loaded data

- [`soeAreas`](https://noaa-edab.github.io/comlandr/reference/soeAreas.md)
  : soeAreas: Area Species,EPU designations
- [`mskeyAreas`](https://noaa-edab.github.io/comlandr/reference/mskeyAreas.md)
  : mskeyAreas: Area Species, EPU designations
- [`mskeyGears`](https://noaa-edab.github.io/comlandr/reference/mskeyGears.md)
  : mskeyGears: Gear Fleet mapping from mskeyrun project
- [`EPUs`](https://noaa-edab.github.io/comlandr/reference/EPUs.md) :
  EPUs: Properties of Ecological Production Units (EPUs)

## Helper Functions

- [`process_foreign_data()`](https://noaa-edab.github.io/comlandr/reference/process_foreign_data.md)
  : Processes NAFO data for comlandr use

## Package Overview

- [`comlandr`](https://noaa-edab.github.io/comlandr/reference/comlandr-package.md)
  [`comlandr-package`](https://noaa-edab.github.io/comlandr/reference/comlandr-package.md)
  : comlandr: Pull and process commercial fisheries data
