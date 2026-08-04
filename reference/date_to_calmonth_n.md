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

## See also

[`vignette("date_conversion", package = "cstime")`](https://niphr.github.io/cstime/articles/date_conversion.md)
for worked date, ISO year and ISO week conversions. No vignette runs
this function.

Other date-to-number converters:
[`date_to_calyear_n()`](https://niphr.github.io/cstime/reference/date_to_calyear_n.md),
[`date_to_isoquarter_n()`](https://niphr.github.io/cstime/reference/date_to_isoquarter_n.md),
[`date_to_isoweek_n()`](https://niphr.github.io/cstime/reference/date_to_isoweek_n.md),
[`date_to_isoyear_n()`](https://niphr.github.io/cstime/reference/date_to_isoyear_n.md)

## Examples

``` r
date_to_calmonth_n(as.Date("2021-08-11"))
#> [1] 8
date_to_calmonth_n("2021-01-01")
#> [1] 1
```
