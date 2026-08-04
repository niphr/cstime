# Date to ISO year (character)

Converts a date to its ISO 8601 week-based year, returned as a character
string.

## Usage

``` r
date_to_isoyear_c(x = lubridate::today())

# Default S3 method
date_to_isoyear_c(x)

# S3 method for class 'character'
date_to_isoyear_c(x = lubridate::today())

# S3 method for class 'Date'
date_to_isoyear_c(x = lubridate::today())
```

## Arguments

- x:

  A Date object, or a character string in the format 'yyyy-mm-dd'.

## Value

ISO year as a character vector (e.g. "2021").

## Details

The ISO 8601 week-based year is not always the same as the calendar
year. ISO weeks run Monday to Sunday, and week 1 is the week containing
the year's first Thursday. As a result the first days of January can
belong to the last ISO week of the previous year, and the last days of
December can belong to ISO week 1 of the following year. For example
2021-01-01 is a Friday that falls in ISO week 53 of ISO year 2020.

## See also

[`vignette("cstime", package = "cstime")`](https://niphr.github.io/cstime/articles/cstime.md)
and
[`vignette("date_conversion", package = "cstime")`](https://niphr.github.io/cstime/articles/date_conversion.md),
which both run this function.

Other date-to-character converters:
[`date_to_calmonth_c()`](https://niphr.github.io/cstime/reference/date_to_calmonth_c.md),
[`date_to_calyear_c()`](https://niphr.github.io/cstime/reference/date_to_calyear_c.md),
[`date_to_calyearmonth_c()`](https://niphr.github.io/cstime/reference/date_to_calyearmonth_c.md),
[`date_to_isoquarter_c()`](https://niphr.github.io/cstime/reference/date_to_isoquarter_c.md),
[`date_to_isoweek_c()`](https://niphr.github.io/cstime/reference/date_to_isoweek_c.md),
[`date_to_isoyearquarter_c()`](https://niphr.github.io/cstime/reference/date_to_isoyearquarter_c.md),
[`date_to_isoyearweek_c()`](https://niphr.github.io/cstime/reference/date_to_isoyearweek_c.md)

## Examples

``` r
date_to_isoyear_c(as.Date("2021-08-11"))
#> [1] "2021"
date_to_isoyear_c("2021-01-01")
#> [1] "2020"
```
