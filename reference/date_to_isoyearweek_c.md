# Date to ISO yearweek (character)

Converts a date to a combined ISO 8601 year and week string of the form
"yyyy-ww".

## Usage

``` r
date_to_isoyearweek_c(x = lubridate::today())

# Default S3 method
date_to_isoyearweek_c(x)

# S3 method for class 'character'
date_to_isoyearweek_c(x = lubridate::today())

# S3 method for class 'Date'
date_to_isoyearweek_c(x = lubridate::today())
```

## Arguments

- x:

  A Date object, or a character string in the format 'yyyy-mm-dd'.

## Value

ISO yearweek as a character vector (e.g. "2021-32").

## Details

The output combines the ISO year (see
[`date_to_isoyear_c()`](https://niphr.github.io/cstime/reference/date_to_isoyear_c.md))
and the zero-padded ISO week (see
[`date_to_isoweek_c()`](https://niphr.github.io/cstime/reference/date_to_isoweek_c.md)),
separated by a hyphen, for example "2021-32". Because the ISO year can
differ from the calendar year, 2021-01-01 maps to "2020-53".

## Examples

``` r
date_to_isoyearweek_c(as.Date("2021-08-11"))
#> [1] "2021-32"
date_to_isoyearweek_c("2021-01-01")
#> [1] "2020-53"
```
