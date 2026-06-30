# Date to ISO week (character)

Converts a date to its ISO 8601 week number, returned as a zero-padded
character string.

## Usage

``` r
date_to_isoweek_c(x = lubridate::today())

# Default S3 method
date_to_isoweek_c(x)

# S3 method for class 'character'
date_to_isoweek_c(x = lubridate::today())

# S3 method for class 'Date'
date_to_isoweek_c(x = lubridate::today())
```

## Arguments

- x:

  A Date object, or a character string in the format 'yyyy-mm-dd'.

## Value

ISO week as a character vector (e.g. "32").

## Details

ISO weeks run Monday to Sunday and are numbered 01 to 52 or 53. Week 01
is the week containing the first Thursday of the ISO year. The week is
returned as two digits, e.g. "01" or "53".

## Examples

``` r
date_to_isoweek_c(as.Date("2021-08-11"))
#> [1] "32"
date_to_isoweek_c("2021-01-01")
#> [1] "53"
```
