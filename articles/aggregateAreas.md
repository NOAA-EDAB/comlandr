# Aggregate landings to another footprint

``` r
library(comlandr)
```

Specifying the arguments `aggArea = T` (in
[`get_comland_data()`](https://noaa-edab.github.io/comlandr/reference/get_comland_data.md))
and supplying a data frame, `userAreas`, the footprint of the returned
landings can be changed by aggregating the landings over [statistical
areas](https://www.fisheries.noaa.gov/resource/map/greater-atlantic-region-statistical-areas)
(the units of reporting).

- Any statistical areas not defined in `userAreas` are assigned to an
  `OTHER` category
- Landings and value are aggregated to areas defined in `userAreas`
- To aggregate NAFO data is more complicated since the reporting
  footprint differs from that of STOCKEFF
  - A proportion of the landings in each NAFO division is assigned to
    `userAreas` based on the overlap of statistical areas in `userAreas`
    with the NAFO division

For a list of Statistical Areas see
[`get_areas()`](https://noaa-edab.github.io/comlandr/reference/get_areas.md)
