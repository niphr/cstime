# Date to calendar year (numeric)

Converts a date to its calendar (Gregorian) year, returned as a number.

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

  A Date object, or a character string in the format 'yyyy-mm-dd'.

## Value

Calendar year as an integer vector (e.g. 2021).

## Details

Unlike the ISO year (see
[`date_to_isoyear_n()`](https://niphr.github.io/cstime/reference/date_to_isoyear_n.md)),
the calendar year is taken directly from the date, so 2021-01-01 is
calendar year 2021.

## Examples

``` r
date_to_calyear_n(as.Date("2021-08-11"))
#> [1] 2021
date_to_calyear_n("2021-01-01")
#> [1] 2021
```
