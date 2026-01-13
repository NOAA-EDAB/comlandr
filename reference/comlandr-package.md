# comlandr: Pull and process commercial fisheries data

Suite of functions to pull and process landings and discard data in
addition to helper function to pull lookup tables

## Details

- Pulls data from StockEff

- Pulls Herring data from state of Maine database

- Defines fleet structure and assigns fleets to gear types

- Define geographic areas and assigns statistical areas to geographic
  areas

- Assigns landings to unknown gear, area, quarter/half year, size based
  on similar trips (based on M Palmer, 2008 Working paper)

- Pulls and processes NAFO (Northwest Atlantic Fisheries Organization)
  data

- Uses survey data to apportion hake complex into species

- Uses survey data to apportion skate complex into species

- Uses observer data to estimate discards

- Adjusts species value to specified date

## References

Palmer, M (2008). **A method to apportion landings with unknown area,
month and unspecified market categories among landings with similar
region and fleet characteristics** Working paper

NAFO website: <https://www.nafo.int>

## See also

Useful links:

- <https://github.com/NOAA-EDAB/comlandr>

- <https://noaa-edab.github.io/comlandr/>

- Report bugs at <https://github.com/NOAA-EDAB/comlandr/issues>

## Author

**Maintainer**: Andy Beet <andrew.beet@noaa.gov>
([ORCID](https://orcid.org/0000-0001-8270-7090))

Authors:

- Sean Lucey
