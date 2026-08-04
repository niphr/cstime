# Date to ISO quarter (numeric)

Converts a date to an ISO-week-based quarter (1 to 4), returned as a
number.

## Usage

``` r
date_to_isoquarter_n(x = lubridate::today())

# Default S3 method
date_to_isoquarter_n(x)

# S3 method for class 'character'
date_to_isoquarter_n(x = lubridate::today())

# S3 method for class 'Date'
date_to_isoquarter_n(x = lubridate::today())
```

## Arguments

- x:

  A Date object, or a character string in the format 'yyyy-mm-dd'.

## Value

ISO quarter as a numeric vector (1 to 4).

## Details

The quarter is derived from the ISO week rather than the calendar month:
weeks 1 to 13 are quarter 1, weeks 14 to 26 are quarter 2, weeks 27 to
39 are quarter 3, and weeks 40 onwards (including week 53 in long ISO
years) are quarter 4.

## See also

[`vignette("date_conversion", package = "cstime")`](https://niphr.github.io/cstime/articles/date_conversion.md)
for worked date, ISO year and ISO week conversions. No vignette runs
this function.

Other date-to-number converters:
[`date_to_calmonth_n()`](https://niphr.github.io/cstime/reference/date_to_calmonth_n.md),
[`date_to_calyear_n()`](https://niphr.github.io/cstime/reference/date_to_calyear_n.md),
[`date_to_isoweek_n()`](https://niphr.github.io/cstime/reference/date_to_isoweek_n.md),
[`date_to_isoyear_n()`](https://niphr.github.io/cstime/reference/date_to_isoyear_n.md)

## Examples

``` r
date_to_isoquarter_n(as.Date("2021-08-11"))
#> [1] 3
date_to_isoquarter_n("2021-01-01")
#> [1] 4
```
