# Downloads all NAFO data

Downloads, imports, aggregates NAFO data from 21B data base

## Usage

``` r
get_foreign_data(
  filterByYear = NA,
  filterByArea = NA,
  removeUSA = T,
  aggregateCountry = T
)
```

## Arguments

- filterByYear:

  Numeric vector. Years for which data is required

- filterByArea:

  Character vector. NAFO Areas for which data is required

- removeUSA:

  Boolean. Should USA landings be removed from data set? (Default = T,
  remove)

- aggregateCountry:

  Boolean. Should all catch be aggregated over country codes? (Default =
  T)

## Value

Data frame: NAFO data

- Year:

  Year of catch

- MONTH:

  Month of catch

- QY:

  Quatere year of catch. Jan-Mar = 1, ..., Oct-Dec = 4

- GearCode:

  NAFO gear code

- Tonnage:

  Size class of vessel

- DivCode:

  Division code in which vessel reported catch

- Code:

  NAFO species code of landed fish

- SPPLIVMT:

  catch in Metric tons

- Country:

  Reporting country - only if `aggregateCounty = F`

## See also

NAFO 21B website:
<https://www.nafo.int/Data/Catch-Statistics-STATLANT-21B>

## Examples

``` r
nafoData <- get_foreign_data(filterByYear = 2000)
#> Reading file: nafo-21b-2000-09.zip
head(nafoData)
#>     Year GearCode Tonnage Divcode  Code MONTH    QY SPPLIVMT
#>    <int>    <int>   <int>   <int> <int> <num> <num>    <num>
#> 1:  2000        8       5      32   632     0     1        0
#> 2:  2000        8       5      32   632     1     1        0
#> 3:  2000        8       5      32   632     2     1        0
#> 4:  2000        8       5      32   632     3     1        0
#> 5:  2000        8       5      32   632     4     2        0
#> 6:  2000        8       5      32   632     5     2        0
```
