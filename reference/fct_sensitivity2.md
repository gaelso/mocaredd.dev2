# Sensitivity analysis of REDD+ emissions and emission reductions

Global sensitivity analysis of the emission / emission reduction
estimates with respect to the model inputs, using the same inputs as the
simulation. The variance of each output is decomposed into first-order
contributions of the independent inputs (activity data, carbon stocks,
carbon fraction, degradation ratios) with the delta method: for
independent inputs and a first-order expansion, \\Var(Y) \approx \sum_i
(\partial Y / \partial X_i)^2 \sigma_i^2\\, and the normalised
contribution \\(\partial Y / \partial X_i \cdot \sigma_i)^2 / Var(Y)\\
is the first-order Sobol index of input \\i\\. Contributions are summed
within REDD+ input groups. Emissions are additionally split between
deforestation (DF) and degradation (DG) when both are reported, each
with its own uncertainty.

See the "Sensitivity analysis" vignette for the rationale and the
relationship with variance-based (Sobol) sensitivity analysis.

## Usage

``` r
fct_sensitivity2(.checked_data)
```

## Arguments

- .checked_data:

  The list returned by
  [`fct_checkinput()`](https://gaelso.github.io/mocaredd.dev2/reference/fct_checkinput.md).

## Value

A list with `variance` (first-order variance contributions of each input
group to each output, in \\ REDD+ activity with uncertainty),
`gg_variance` (contribution bar chart) and `gg_split` (DF vs DG
emissions bar chart).

## Examples

``` r
library(mocaredd)
#> Error in library(mocaredd): there is no package called ‘mocaredd’

path <- system.file("extdata/mocaredd-templatev2-simple.xlsx", package = "mocaredd.dev2")
checked <- fct_checkinput(.path = path)
#> Loading data... - progress: 0%.
#> ✓ Tables loaded successfully from template v2
#> Checking column names... - progress: 14%.
#> ✓ Column names: all required columns present
#> Checking table dimensions... - progress: 29%.
#> ✓ Table sizes: all tables have sufficient rows
#> Checking column data types... - progress: 43%.
#> ✓ Data types: all columns have correct data types
#> Checking category values... - progress: 57%.
#> ✓ Category values: all categories are valid
#> Checking unique IDs... - progress: 71%.
#> ✓ Unique IDs: no duplicates or missing IDs found
#> Checking cross-table and intra_table consistency... - progress: 86%.
#> ✓ Cross-table consistency: all references match
#> -- All checks passed.

sa <- fct_sensitivity2(.checked_data = checked)
sa$variance
#> # A tibble: 12 × 3
#>    output  group             contribution_pct
#>    <chr>   <chr>                        <dbl>
#>  1 REF     Carbon stock                  43.3
#>  2 REF     Degradation ratio             48.9
#>  3 REF     Activity data                  7.2
#>  4 REF     Carbon fraction                0.6
#>  5 E-MON1  Carbon stock                  38.2
#>  6 E-MON1  Degradation ratio             56.3
#>  7 E-MON1  Activity data                  4.8
#>  8 E-MON1  Carbon fraction                0.7
#>  9 ER-MON1 Carbon stock                  42.3
#> 10 ER-MON1 Degradation ratio             40.8
#> 11 ER-MON1 Activity data                 16.4
#> 12 ER-MON1 Carbon fraction                0.5
```
