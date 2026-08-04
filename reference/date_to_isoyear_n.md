# Date to ISO year (numeric)

Converts a date to its ISO 8601 week-based year, returned as a number.

## Usage

``` r
date_to_isoyear_n(x = lubridate::today())

# Default S3 method
date_to_isoyear_n(x)

# S3 method for class 'character'
date_to_isoyear_n(x = lubridate::today())

# S3 method for class 'Date'
date_to_isoyear_n(x = lubridate::today())
```

## Arguments

- x:

  A Date object, or a character string in the format 'yyyy-mm-dd'.

## Value

ISO year as an integer vector (e.g. 2021).

## Details

The ISO 8601 week-based year can differ from the calendar year near the
start and end of January and December. See
[`date_to_isoyear_c()`](https://niphr.github.io/cstime/reference/date_to_isoyear_c.md)
for the rules used to assign weeks and years.

## See also

[`vignette("date_conversion", package = "cstime")`](https://niphr.github.io/cstime/articles/date_conversion.md),
which runs this function.

Other date-to-number converters:
[`date_to_calmonth_n()`](https://niphr.github.io/cstime/reference/date_to_calmonth_n.md),
[`date_to_calyear_n()`](https://niphr.github.io/cstime/reference/date_to_calyear_n.md),
[`date_to_isoquarter_n()`](https://niphr.github.io/cstime/reference/date_to_isoquarter_n.md),
[`date_to_isoweek_n()`](https://niphr.github.io/cstime/reference/date_to_isoweek_n.md)

## Examples

``` r
date_to_isoyear_n(as.Date("2021-08-11"))
#> [1] 2021
date_to_isoyear_n("2021-01-01")
#> [1] 2020
```
