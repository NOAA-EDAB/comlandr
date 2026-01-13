# Compare NAFO 21A & 21B databases

## Northwest Atlantic Fisheries Organization (NAFO)

Data stored on the [NAFO](https://www.nafo.int/Data/Catch-Statistics)’s
site is stored in the STATLANT 21 database and is served to the user in
one of two formats. The differences are explained
[here](https://noaa-edab.github.io/comlandr/articles/ForeignCatch.html)
package.

## Spatial Domain

The NAFO spatial domain “covers a large portion of the Atlantic Ocean,
including the 200-mile zones of Coastal States — Canada, Greenland,
St. Pierre et Miquelon and USA.”. The R package
[`comlandr`](https://noaa-edab.github.io/comlandr) focuses on the
Northeast United States (NEUS) shown by the fisheries statistcal areas
as defined by the Greater Atlantic Regional Fisheries Office
([GARFO](https://www.fisheries.noaa.gov/about/greater-atlantic-regional-fisheries-office)).
The NAFO divisions that map to the NEUS domain are shown in the second
panel.

![Spatial
domain](ForeignCatchCompare21A21B_files/figure-html/plotspatial-1.png)

The remainder of this document and all following figures will be based
on this smaller area.

## Catch by Division

Catch by the USA is reported and stored in the NEFSC databases. Prior to
NAFO database comparisons all catch by the USA is removed. Catch from
divisions 4X, 5Y, 5Z, 6A, 6B, 6C (NEUS equivalent) area used. Aggregated
over Country and Species

- 21A
- 21B

![Catch by Division
21A](ForeignCatchCompare21A21B_files/figure-html/plot21adiv-1.png)

![Catch by Division
21B](ForeignCatchCompare21A21B_files/figure-html/plot21bdiv-1.png)

## Catch by Country

Aggregated over species and division

### 21A

![Catch by Country
21A](ForeignCatchCompare21A21B_files/figure-html/plot21acountry-1.png)

### 21B

![Catch by Country
21B](ForeignCatchCompare21A21B_files/figure-html/plot21bcountry-1.png)

## Catch by Species

Aggregate over division, country

### 21A

There are 112 species represented.

Species in 21A not in 21B:

| x                       |
|:------------------------|
| WINTER SKATE - RJT      |
| BARNDOOR SKATE - RJL    |
| LITTLE SKATE - RJD      |
| GREEN CRAB - CRG        |
| BASKING SHARK - BSK     |
| NORTHERN WOLFFISH - CAB |
| SPOTTED WOLFFISH - CAS  |
| DUSKY SHARK - DUS       |
| SCALLOPS (NS) - SCX     |

![Catch by Species
21A](ForeignCatchCompare21A21B_files/figure-html/plot21aspecies-1.png)![Catch
by Species
21A](ForeignCatchCompare21A21B_files/figure-html/plot21aspecies-2.png)![Catch
by Species
21A](ForeignCatchCompare21A21B_files/figure-html/plot21aspecies-3.png)![Catch
by Species
21A](ForeignCatchCompare21A21B_files/figure-html/plot21aspecies-4.png)![Catch
by Species
21A](ForeignCatchCompare21A21B_files/figure-html/plot21aspecies-5.png)![Catch
by Species
21A](ForeignCatchCompare21A21B_files/figure-html/plot21aspecies-6.png)![Catch
by Species
21A](ForeignCatchCompare21A21B_files/figure-html/plot21aspecies-7.png)

### 21B

There are 107 species represented

Species in 21B not in 21A

| x                  |
|:-------------------|
| BLUE LING - BLI    |
| MULLETS (NS) - MUL |
| ARCTIC SKATE - RJG |
| CUNNER - CUN       |

![Catch by Species
21B](ForeignCatchCompare21A21B_files/figure-html/plot21bspecies-1.png)![Catch
by Species
21B](ForeignCatchCompare21A21B_files/figure-html/plot21bspecies-2.png)![Catch
by Species
21B](ForeignCatchCompare21A21B_files/figure-html/plot21bspecies-3.png)![Catch
by Species
21B](ForeignCatchCompare21A21B_files/figure-html/plot21bspecies-4.png)![Catch
by Species
21B](ForeignCatchCompare21A21B_files/figure-html/plot21bspecies-5.png)![Catch
by Species
21B](ForeignCatchCompare21A21B_files/figure-html/plot21bspecies-6.png)![Catch
by Species
21B](ForeignCatchCompare21A21B_files/figure-html/plot21bspecies-7.png)

## Catch over Time

Aggregated catch over division, country, and species

### 21A

Years with missing data:

![Catch by Time
21A](ForeignCatchCompare21A21B_files/figure-html/plot21atime-1.png)

### 21B

Years with missing data: 2003, 2008, 2009, 2013, 2020, 2021, 2022

![Catch by Time
21B](ForeignCatchCompare21A21B_files/figure-html/plot21btime-1.png)

## Catch assigned to Unknown Divisions

Displayed are the percentage of the total catch that are reported in
unknown Divisions

### For 21A

![Catch Unknowns
21A](ForeignCatchCompare21A21B_files/figure-html/unknown21a-1.png)

### For 21B

![Catch Unknowns
21B](ForeignCatchCompare21A21B_files/figure-html/unknown21b-1.png)
