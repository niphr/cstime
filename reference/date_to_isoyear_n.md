# ISO year (numeric) from Date object

ISO year (numeric) from Date object

## Usage

``` r
date_to_isoyear_n(x = lubridate::today())

# Default S3 method
date_to_isoyear_n(x)

# S3 method for class 'character'
date_to_isoyear_n(x = lubridate::today())

# S3 method for class 'Date'
date_to_isoyear_n(x = lubridate::today())
```

## Arguments

- x:

  a Date object or string, in the form of 'yyyy-mm-dd'

## Value

ISO year in numeric

## Examples

``` r
date_to_isoyear_n("2021-08-11")
#> [1] 2021
date_to_isoyear_n(lubridate::today())
#> [1] 2026
```
