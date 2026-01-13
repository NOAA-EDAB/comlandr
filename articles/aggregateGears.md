# Aggregate gears codes to fleets

``` r
library(comlandr)
```

Specifying the arguments `aggGear = T` (in
[`get_comland_data()`](https://noaa-edab.github.io/comlandr/reference/get_comland_data.md))
and supplying a data frame, `userGears`, fishing fleets are returned by
aggregating the landings over gear types. \* `NEGEAR2` gear codes are
aggregated to fleets as defined in `userGears`

For a list of `NEGEAR2` codes see
[`get_gears()`](https://noaa-edab.github.io/comlandr/reference/get_gears.md)
