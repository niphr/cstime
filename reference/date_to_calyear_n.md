# Date -\> calyear (numeric)

Date -\> calyear (numeric)

## Usage

``` r
date_to_calyear_n(x = lubridate::today())

# Default S3 method
date_to_calyear_n(x)

# S3 method for class 'character'
date_to_calyear_n(x = lubridate::today())

# S3 method for class 'Date'
date_to_calyear_n(x = lubridate::today())
```

## Arguments

- x:

  a Date object or string, in the form of 'yyyy-mm-dd'

## Value

ISO year in character

## Examples

``` r
date_to_calyear_n("2021-08-11")
#> [1] 2021
date_to_calyear_n(lubridate::today())
#> [1] 2026
```
