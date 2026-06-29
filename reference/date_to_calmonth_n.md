# Date -\> calmonth (numeric)

Date -\> calmonth (numeric)

## Usage

``` r
date_to_calmonth_n(x = lubridate::today())

# Default S3 method
date_to_calmonth_n(x)

# S3 method for class 'character'
date_to_calmonth_n(x = lubridate::today())

# S3 method for class 'Date'
date_to_calmonth_n(x = lubridate::today())
```

## Arguments

- x:

  a Date object or string, in the form of 'yyyy-mm-dd'

## Value

calmonth

## Examples

``` r
date_to_calmonth_n("2021-08-11")
#> [1] 8
date_to_calmonth_n(lubridate::today())
#> [1] 6
```
