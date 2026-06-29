# Date -\> calyearmonth (character)

Date -\> calyearmonth (character)

## Usage

``` r
date_to_calyearmonth_c(x = lubridate::today())

# Default S3 method
date_to_calyearmonth_c(x)

# S3 method for class 'character'
date_to_calyearmonth_c(x = lubridate::today())

# S3 method for class 'Date'
date_to_calyearmonth_c(x = lubridate::today())
```

## Arguments

- x:

  a Date object or string, in the form of 'yyyy-mm-dd'

## Value

calyearmonth ("YYYY-MXX")

## Examples

``` r
date_to_calyearmonth_c("2021-08-11")
#> [1] "2021-M08"
date_to_calyearmonth_c(lubridate::today())
#> [1] "2026-M06"
```
