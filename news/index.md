# Changelog

## comlandr 1.1.1

### Patch fixes

- Change to `get_foreign_Data()` to include
  [NAFO](https://noaa-edab.github.io/comlandr/news/nafo.int) 21B data
  updates
- Update to bundled 21A data

## comlandr 1.1.0

Previous version of package pulled from a deprecated time series. It was
then hard coded in the package as a lazyloaded dataset, with a manual
updating process. This has been replaced with a workflow to pull the
data stream via API. To ensure even if API fails there is economic data
available

### Minor changes

- Use the Federal reserve bank of st louis price deflator.
  <https://fred.stlouisfed.org/series/WPU0223>. This is a monthly index.
- `adjust_inflation.r` edited to use this data stream.
- Add argument checks in `check_argument_validation.r` to throw an error
  if reference values for Month and/or Year fall outside the data set.
- Workflow (`getFred.yaml`) sends email every month to report if the
  economic data has changed, runs on a schedule 20th of the month since
  data is updated on 16th of month
  - compares data in repo and new pulled data (`compare_fred_data` &
    `get_fred`)
  - sends email report (`sendAsEmail.rmd`)
- Add rmd to document schemas used

### Bug fixes

- Many of the data tables used in the supporting functions have been
  moved from NEFSC to GARFO (Schema changes). Changes to reflect this
  have been made in several of the `get` functions
- `refYear` and `refMonth` allowed to both be `NA` or both not `NA` PR
- Can not have either `refYear` and `refMonth` to be `NA` if other is
  not.
- A change in `data.table` resulted in a string names to be converted to
  a vector for use in join statements.

### New functions

- `get_species_stock_area.r` - Extrats statistical areas that comprise
  species stock definition

## comlandr 1.0.0

A significant overhaul to the layout and structure of the package. Many
changes are internal and will not be apparent to a user

### Major changes

- Reworked algorithm to assign unknowns. This operation now takes place
  immediately following data pull
- Changed names and reorganzied many internal functions
- Added functions to estimate discards
- Added lazy loaded datasets from mskeyrun project and price data
- Switched underlying data source. From WODETS, WOLANDS, CFDETS to
  STOCKEFF (which pulls from these earlier data sources along with CAMS)

## comlandr 0.3.0

Standalone functionality for pulling NAFO data

`get_foreign_data` - pulls data from NAFOs STATLANT 21B database

## comlandr 0.2.0

Added supporting `get` functions to pull information from supporting
oracle tables

`get_areas` - pull stat area, region, NAFO codes

`get_gears` - pull gear type, codes, description

`get_vessels` - pull tonnage, crew size

`get_locations` - pull lat, lon, 10 min sq.

`get_ports` - pull port names and locations

`get_species` - pull species name, market code information

`get_species_itis` - pull species itis information

## comlandr 0.1.0

Initial release
