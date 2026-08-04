# Date to calendar month (character)

Converts a date to its calendar month number, returned as a zero-padded
character string.

## Usage

``` r
date_to_calmonth_c(x = lubridate::today())

# Default S3 method
date_to_calmonth_c(x)

# S3 method for class 'character'
date_to_calmonth_c(x = lubridate::today())

# S3 method for class 'Date'
date_to_calmonth_c(x = lubridate::today())
```

## Arguments

- x:

  A Date object, or a character string in the format 'yyyy-mm-dd'.

## Value

Calendar month as a character vector ("01" to "12").

## Details

The month is returned as two digits, "01" for January through "12" for
December.

## See also

[`vignette("date_conversion", package = "cstime")`](https://niphr.github.io/cstime/articles/date_conversion.md)
for worked date, ISO year and ISO week conversions. No vignette runs
this function.

Other date-to-character converters:
[`date_to_calyear_c()`](https://niphr.github.io/cstime/reference/date_to_calyear_c.md),
[`date_to_calyearmonth_c()`](https://niphr.github.io/cstime/reference/date_to_calyearmonth_c.md),
[`date_to_isoquarter_c()`](https://niphr.github.io/cstime/reference/date_to_isoquarter_c.md),
[`date_to_isoweek_c()`](https://niphr.github.io/cstime/reference/date_to_isoweek_c.md),
[`date_to_isoyear_c()`](https://niphr.github.io/cstime/reference/date_to_isoyear_c.md),
[`date_to_isoyearquarter_c()`](https://niphr.github.io/cstime/reference/date_to_isoyearquarter_c.md),
[`date_to_isoyearweek_c()`](https://niphr.github.io/cstime/reference/date_to_isoyearweek_c.md)

## Examples

``` r
date_to_calmonth_c(as.Date("2021-08-11"))
#> [1] "08"
date_to_calmonth_c("2021-01-01")
#> [1] "01"
```
