# Date to ISO week (numeric)

Converts a date to its ISO 8601 week number, returned as a number.

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

  A Date object, or a character string in the format 'yyyy-mm-dd'.

## Value

ISO week as an integer vector (1 to 53).

## Details

ISO weeks run Monday to Sunday and are numbered 1 to 52 or 53. Week 1 is
the week containing the first Thursday of the ISO year.

## Examples

``` r
date_to_isoweek_n(as.Date("2021-08-11"))
#> [1] 32
date_to_isoweek_n("2021-01-01")
#> [1] 53
```
