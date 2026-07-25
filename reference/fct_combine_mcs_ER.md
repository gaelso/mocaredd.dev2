# Combine MCS of emissions from Reference and monitoring period into emission reductions

Depending on how the period is defined and if the data are annualized or
not, calculate the Emission Level for a reference or monitoring period
for each simulation.

## Usage

``` r
fct_combine_mcs_ER(.sim_ref, .sim_mon, .ad_annual)
```

## Arguments

- .sim_ref:

  simulation aggregated to reference period, output from
  fct_combine_mcs_P()

- .sim_mon:

  simulation aggregated to monitoring period(s), output from
  fct_combine_mcs_P()

- .ad_annual:

  TRUE or FALSE, is the activity data annualized or not.

## Value

A tibble with simulations at the final estimate per type of period.

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

sim_trans <- fct_combine_mcs_E(.checked_data = checked)

sim_REF <- fct_combine_mcs_P(
  .data = sim_trans,
  .time = checked$data$time,
  .period_type = "REF",
  .ad_annual = checked$data$setup$ad_annual
)
#> Error in dplyr::select(dplyr::filter(.time, !is.na(.data$period_type),     stringr::str_detect(.data$period_type, pattern = .period_type)),     "period_no", "period_type", "nb_years"): Can't select columns that don't exist.
#> ✖ Column `nb_years` doesn't exist.

sim_MON <- fct_combine_mcs_P(
  .data = sim_trans,
  .time = checked$data$time,
  .period_type = "MON",
  .ad_annual = checked$data$setup$ad_annual
)
#> Error in dplyr::select(dplyr::filter(.time, !is.na(.data$period_type),     stringr::str_detect(.data$period_type, pattern = .period_type)),     "period_no", "period_type", "nb_years"): Can't select columns that don't exist.
#> ✖ Column `nb_years` doesn't exist.

## !!! SIM MON and ER to be done
```
