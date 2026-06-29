# ISO week (numeric) from Date object

ISO week (numeric) from Date object

## Usage

``` r
date_to_isoweek_n(x = lubridate::today())

# Default S3 method
date_to_isoweek_n(x)

# S3 method for class 'character'
date_to_isoweek_n(x = lubridate::today())

# S3 method for class 'Date'
date_to_isoweek_n(x = lubridate::today())
```

## Arguments

- x:

  a Date object or string, in the form of 'yyyy-mm-dd'

## Value

ISO week in numeric

## Examples

``` r
date_to_isoweek_n("2021-08-11")
#> [1] 32
date_to_isoweek_n(lubridate::today())
#> [1] 27
```
