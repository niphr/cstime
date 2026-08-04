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

## See also

[`vignette("date_conversion", package = "cstime")`](https://niphr.github.io/cstime/articles/date_conversion.md),
which runs this function.

Other date-to-character converters:
[`date_to_calmonth_c()`](https://niphr.github.io/cstime/reference/date_to_calmonth_c.md),
[`date_to_calyear_c()`](https://niphr.github.io/cstime/reference/date_to_calyear_c.md),
[`date_to_calyearmonth_c()`](https://niphr.github.io/cstime/reference/date_to_calyearmonth_c.md),
[`date_to_isoquarter_c()`](https://niphr.github.io/cstime/reference/date_to_isoquarter_c.md),
[`date_to_isoyear_c()`](https://niphr.github.io/cstime/reference/date_to_isoyear_c.md),
[`date_to_isoyearquarter_c()`](https://niphr.github.io/cstime/reference/date_to_isoyearquarter_c.md),
[`date_to_isoyearweek_c()`](https://niphr.github.io/cstime/reference/date_to_isoyearweek_c.md)

## Examples

``` r
date_to_isoweek_c(as.Date("2021-08-11"))
#> [1] "32"
date_to_isoweek_c("2021-01-01")
#> [1] "53"
```
