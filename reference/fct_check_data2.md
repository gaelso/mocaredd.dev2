# Check that the app's input data meet the template criteria.

Tests 6 types of conformity with the app template to avoid code break
during analysis: (1) column names, (2) tables' size, (3) data types, (4)
categories names, (5) unique IDs and (6) matching key variables between
tables.

## Usage

``` r
fct_check_data2(.usr, .time, .ad, .cs)
```

## Arguments

- .usr:

  User inputs' table for the shiny app (user_inputs)

- .time:

  the 'time' table from the tool input file (see template)

- .ad:

  Activity Data input table for the shiny app (AD_lu_transitions)

- .cs:

  Carbon Stock input table for the shiny app (c_stocks)

## Value

A dataframe with TRUE or FALSE (TRUE if each check passes), and broad
error locations if FALSE.

## Examples

``` r

path <- system.file("extdata/example1-4pools.xlsx", package = "mocaredd.dev2")
path <- system.file("extdata/mocaredd-template-v2-4pools.xlsx", package = "mocaredd.dev2")

tabs <- c("user", "time", "area", "carbon")
dat <- purrr::map(tabs , function(x){
  readxl::read_xlsx(path = path, sheet = x, na = "NA")
})
#> Error in purrr::map(tabs, function(x) {    readxl::read_xlsx(path = path, sheet = x, na = "NA")}): ℹ In index: 1.
#> Caused by error:
#> ! `path` does not exist: ‘’
names(dat) <- tabs
#> Error: object 'dat' not found

fct_check_data2(.ad = ad, .cs = cs, .usr = usr, .time = time)
#> Error: object 'usr' not found
```
