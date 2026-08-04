# Date to calendar year (character)

Converts a date to its calendar (Gregorian) year, returned as a
character string.

## Usage

``` r
date_to_calyear_c(x = lubridate::today())

# Default S3 method
date_to_calyear_c(x)

# S3 method for class 'character'
date_to_calyear_c(x = lubridate::today())

# S3 method for class 'Date'
date_to_calyear_c(x = lubridate::today())
```

## Arguments

- x:

  A Date object, or a character string in the format 'yyyy-mm-dd'.

## Value

Calendar year as a character vector (e.g. "2021").

## Details

Unlike the ISO year (see
[`date_to_isoyear_c()`](https://niphr.github.io/cstime/reference/date_to_isoyear_c.md)),
the calendar year is taken directly from the date, so 2021-01-01 is
calendar year "2021".

## See also

[`vignette("date_conversion", package = "cstime")`](https://niphr.github.io/cstime/articles/date_conversion.md)
for worked date, ISO year and ISO week conversions. No vignette runs
this function.

Other date-to-character converters:
[`date_to_calmonth_c()`](https://niphr.github.io/cstime/reference/date_to_calmonth_c.md),
[`date_to_calyearmonth_c()`](https://niphr.github.io/cstime/reference/date_to_calyearmonth_c.md),
[`date_to_isoquarter_c()`](https://niphr.github.io/cstime/reference/date_to_isoquarter_c.md),
[`date_to_isoweek_c()`](https://niphr.github.io/cstime/reference/date_to_isoweek_c.md),
[`date_to_isoyear_c()`](https://niphr.github.io/cstime/reference/date_to_isoyear_c.md),
[`date_to_isoyearquarter_c()`](https://niphr.github.io/cstime/reference/date_to_isoyearquarter_c.md),
[`date_to_isoyearweek_c()`](https://niphr.github.io/cstime/reference/date_to_isoyearweek_c.md)

## Examples

``` r
date_to_calyear_c(as.Date("2021-08-11"))
#> [1] "2021"
date_to_calyear_c("2021-01-01")
#> [1] "2021"
```
