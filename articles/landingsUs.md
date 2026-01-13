# Pull US landings data from STOCKEFF

``` r
library(comlandr)
```

US landings by trip are pulled from STOCKEFF via
[`get_comland_raw_data()`](https://noaa-edab.github.io/comlandr/reference/get_comland_raw_data.md)
and grouped by year, month, negear, toncl2, nespp3, nespp4, area,
utilcd, mesh, market_code from which spplivlb, sppvalue, and spplndlb
are summed.

- Mesh category (`MESHCAT`) is then defined as either `SM` (\<= 3
  inches) or `LG` (\> 3 inches) depending on whether a trip reported a
  mesh size

- Depending on the value of the argument `useLanded`, meat weight or
  live weight is used for shellfish species (NESPP3 codes for shellfish
  are 743:800). Use
  [`get_species()`](https://noaa-edab.github.io/comlandr/reference/get_species.md)
  or
  [`get_species_itis()`](https://noaa-edab.github.io/comlandr/reference/get_species_itis.md)
  to associate common or scientific names to these shellfish codes

- Fish parts are removed. For a list of removed fish part codes, see
  [`vignette("fishPartsRemoved")`](https://noaa-edab.github.io/comlandr/articles/fishPartsRemoved.md)

- All weight is converted from lbs to metric tons
