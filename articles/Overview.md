# Overview

We will attempt to summarise the data sources, methods, assumptions
made, and the caveats of note in the `comlandr` package.

## Data sources

Data are used from various sources to compile the resulting data pull

- Commercial landings are pulled from the Stock Efficiency Initiative
  [(STOCKEFF)](https://apps-st.fisheries.noaa.gov/confluence/spaces/STOCKEFFDOC/pages/246776079/StockEff+for+NEFSC+Users)
  commercial landings module. A product created and maintained by the
  NEFSC population dynamics branch.
- Herring data comes from the State of Maine. The data is updated
  periodically by the stock assessment scientist
- Catch by international (non US) vessels are reported to Northwest
  Atlantic Fisheries Organization [(NAFO)](https://www.nafo.int/). This
  data is pulled directly from their website.
- Observer data from the NEFSC is used to aid in the estimation of
  discards

### Commercial landings

Commercial landings data resides in the Catch Accounting and Monitoring
System
[(CAMS)](https://www.greateratlantic.fisheries.noaa.gov/ro/fso/reports/cams/cams_documentation/)
a database maintained by the Greater Atlantic Regional Fisheries Office
[(GARFO)](https://www.fisheries.noaa.gov/about/greater-atlantic-regional-fisheries-office).
This system was officially launched in 2022. Prior to this time landings
data was more fragmented and “snapshots” of the data (housed at GARFO)
was stored at the NEFSC. This was the source data for all stock
assessments.

The Stock Efficiency Initiative was created (prior to the adoption of
CAMS) with the goal of consolidating the landings data into a more
digestible form for use in stock assessments (amongst many other
things). STOCKEFF now incorporates CAMS data (post 2022) and legacy data
to serve up a product for stock assessments.

`comlandr` uses the STOCKEFF product exclusively for all landings data.

The reason for this is to align the scientific products created by the
Ecosystem Dynamics and Assessment Branch (EDAB) with stock assessments
undertaken by Population Dynamics Branch by using the same underlying
data source.

### Herring landings

Herring data comes from the State of Maine and is not fully integrated
into CAMS (and therefore STOCKEFF). In `comlandr`, the Herring data is
pulled from a separate source managed independently from STOCKEFF.

### Foreign landings

Landings by non US fishing vessels (foreign landings) within the
Northwest Atlantic waters are managed by the Northwest Atlantic
Fisheries Organization [(NAFO)](https://www.nafo.int/). `comlandr` pulls
this data from the NAFO website and merges it with US landings data

For detailed information regarding NAFO data please review the
[`vignette("ForeignCatch")`](https://noaa-edab.github.io/comlandr/articles/ForeignCatch.md)
and
[`vignette("ForeignCatchCompare21A21B")`](https://noaa-edab.github.io/comlandr/articles/ForeignCatchCompare21A21B.md)

## Processing steps

- Pull US landings from commercial database
  ([`vignette("landingsUs")`](https://noaa-edab.github.io/comlandr/articles/landingsUs.md))
- Identify and assign values to missing data
  ([`vignette("missingData")`](https://noaa-edab.github.io/comlandr/articles/missingData.md))
- Pull Herring data from State of Maine
  ([`vignette("landingsHerring")`](https://noaa-edab.github.io/comlandr/articles/landingsHerring.md))
- Pull Foreign vessel landings from NAFO
  ([`vignette("landingsNafo")`](https://noaa-edab.github.io/comlandr/articles/landingsNafo.md))
- Calculate species value
  ([`vignette("speciesValue")`](https://noaa-edab.github.io/comlandr/articles/speciesValue.md))
- Differentiate species reported as a group (skates, hakes)
  ([`vignette("disaggregate")`](https://noaa-edab.github.io/comlandr/articles/disaggregate.md))
- Aggregate statistical areas to a defined spatial region
  ([`vignette("aggregateAreas")`](https://noaa-edab.github.io/comlandr/articles/aggregateAreas.md))
- Aggregate fishing gears to defined fleets
  ([`vignette("aggregateGears")`](https://noaa-edab.github.io/comlandr/articles/aggregateGears.md))

## Discards

The reporting of discarding is not legally required. However estimates
are required for management. `comlandr` uses data from the [Observer
program](https://www.fisheries.noaa.gov/new-england-mid-atlantic/fisheries-observers/northeast-fisheries-observer-program)
to help estimate discards.

See
[`vignette("discarding")`](https://noaa-edab.github.io/comlandr/articles/discarding.md)

## Caveats

- Although US landings data (SPPLIVLB) pulled from STOCKEFF can be
  retrieved from as early as 1964, species value (SPPVALUE) is only
  available from 1982. Prior to this date the value can not be reliably
  validated.

- Menhaden landings are not complete in the commercial database.
