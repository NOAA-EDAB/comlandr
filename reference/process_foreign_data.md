# Processes NAFO data for comlandr use

Assigns NAFO Gear codes to NEGEAR codes and NEGEAR codes to fleets
Assigns NAFO Species to NESPP3 species Assigns NAFO Divcodes to EPUs
Assigns NAFO tonnage to tonnage classes

## Usage

``` r
process_foreign_data(channel, nafoland, useLanded = T, useHerringMaine = T)
```

## Arguments

- channel:

  an Object inherited from `ROracle::Oracle`. This object is used to
  connect to communicate with the database engine. (see
  `dbutils::connect_to_database`)

- nafoland:

  Data frame. output from
  [`get_foreign_data`](https://noaa-edab.github.io/comlandr/reference/get_foreign_data.md)

- useLanded:

  boolean. Use landed or live weight for shellfish (Default = T, landed)

- useHerringMaine:

  boolean. Pull data from Maine Herring database or use herring data in
  commercial landings database (Default = T)

## Value

Data frame: NAFO data

## Details

Fills in missing data by including from 21A (SS EPU only)
