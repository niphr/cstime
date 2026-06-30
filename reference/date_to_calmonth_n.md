# Date to calendar month (numeric)

Converts a date to its calendar month number, returned as a number.

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

  A Date object, or a character string in the format 'yyyy-mm-dd'.

## Value

Calendar month as an integer vector (1 for January to 12 for December).

## Examples

``` r
date_to_calmonth_n(as.Date("2021-08-11"))
#> [1] 8
date_to_calmonth_n("2021-01-01")
#> [1] 1
```
