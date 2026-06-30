# Date to calendar yearmonth (character)

Converts a date to a combined calendar year and month string of the form
"yyyy-Mmm".

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

  A Date object, or a character string in the format 'yyyy-mm-dd'.

## Value

Calendar yearmonth as a character vector (e.g. "2021-M08").

## Details

The output combines the calendar year and the zero-padded calendar
month, separated by "-M", for example "2021-M08".

## Examples

``` r
date_to_calyearmonth_c(as.Date("2021-08-11"))
#> [1] "2021-M08"
date_to_calyearmonth_c("2021-01-01")
#> [1] "2021-M01"
```
