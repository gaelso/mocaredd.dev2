# Check that the app's the input data has matching land uses and correct carbon pools and associated factors.

The expected input carbon stock table for the tool contains carbon
values of different carbon pools coded: - AGB for aboveground biomass, -
BGB or RS (Root-to-Shoot ratio) for the belowground biomass, - DW for
deadwood, - LI for litter and - SOC for soil organic carbon. All pools
can be expressed in tons of carbon (C), except AGB and BGB which can
also be expressed in ton of dry matter (DM) if a carbon fraction 'CF' is
provided.

## Usage

``` r
fct_make_formula(.c_el, .c_unit, .version = 1)
```

## Arguments

- .c_el:

  Vector of carbon elements, inc. "AGB", "BGB", "RS", "DW", "LI", "SOC",
  "ALL".

- .c_unit:

  Carbon unit. For `.version = 1` a single value ("DM" or "C") that
  applies to the whole land use. For `.version = 2` a vector aligned
  with `.c_el` giving the unit of each carbon element.

- .version:

  Template version, 1 (default) or 2. In v1 a single carbon fraction
  (CF) wraps the biomass pools when the land use is expressed as dry
  matter (DM). In v2 CF is applied per carbon element, only to those
  expressed as DM.

## Value

A character value with the formula for calculating total carbon stock.

## Examples

``` r
library(mocaredd)
#> Error in library(mocaredd): there is no package called ‘mocaredd’

c_el <- c("AGB", "RS", "DW")

fct_make_formula(.c_el = c_el, .c_unit = "DM")
#> [1] "(AGB + AGB * RS) * CF + DW"

## Template v2, one unit per element
fct_make_formula(.c_el = c_el, .c_unit = c("DM", NA, "C"), .version = 2)
#> [1] "AGB * CF + AGB * RS * CF + DW"
```
