# Tease apart unclassified groups of species

``` r
library(comlandr)
```

Little and winter skate species as well as silver and offshore hake
species are often reported as unclassified in the commercial landings.
The NEFSC bottom trawl survey is used to tease out the contribution of
each species to the total of unclassified landings. The bottom trawl
survey identifies and weighs all species caught. Estimates of the
proportion of each species are estimated through time and space. These
proportions are then applied to the landings data to result in estimates
of species landed.

## Skates

- Skate species in the bottom trawl survey are identified with SVSPP
  codes = 22:28
- The location of bottom trawl tows (both in SPRING and FALL) are
  assigned to statistical areas in which landings are reported.
- The proportion caught (by biomass) by season is then applied to the
  commercial landings (SPPLIVWT) and value (SPPVALUE) for the
  unclassified skates (NESPP3 = 365) and assigned to little skate
  (NESPP3 = 366) or winter skate (NESPP3 = 367). The remained is left as
  unclassified.

## Hakes

The same process is applied to the hake species

- Offshore hake (SVSPP = 69) and silver hake (SVSPP = 72)
- Commercial data base code for unclassified hakes (NESPP3 = 507).
  Resulting landings are assigned to offshore hake (NESPP3 = 508) and
  silver hake (NESPP3 = 509)
