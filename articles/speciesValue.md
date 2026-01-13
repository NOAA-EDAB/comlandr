# Calculating present day value

``` r
library(comlandr)
```

The `SPPVALUE` field in the STOCKEFF database is the value of the catch
on the date landed. To compare `SPPVALUE` over time, these values need
to be converted to present day dollar amounts. This is achieved using
the [Unprocessed and Prepared Seafood Index,
WPU0223](https://fred.stlouisfed.org/series/WPU0223) from the Federal
Reserve Bank of St Louis. This index is calculated on a monthly basis.

- `SPPVALUE` in STOCKEFF is only available from 1981 to present day.
- `comlandr` allows the user to adjust the value to any reference date
  required. See arguments `refMonth`, `refYear` in
  [`get_comland_data()`](https://noaa-edab.github.io/comlandr/reference/get_comland_data.md)
