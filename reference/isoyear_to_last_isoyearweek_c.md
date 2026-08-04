# ISO year to last ISO yearweek (character)

Returns the last ISO yearweek of a given ISO year as a "yyyy-ww" string.

## Usage

``` r
isoyear_to_last_isoyearweek_c(x)

# Default S3 method
isoyear_to_last_isoyearweek_c(x)

# S3 method for class 'character'
isoyear_to_last_isoyearweek_c(x)

# S3 method for class 'numeric'
isoyear_to_last_isoyearweek_c(x)
```

## Arguments

- x:

  ISO year as a number or character string, e.g. 2020 or "2020".

## Value

Last ISO yearweek of the year as a character vector (e.g. "2020-53").

## Details

Most ISO years have 52 weeks, so the result is usually "yyyy-52". ISO
years that contain 53 weeks (such as 2020) instead return "yyyy-53". The
year is accepted as either a number or a character string.

## See also

[`isoyear_to_last_isoweek_n()`](https://niphr.github.io/cstime/reference/isoyear_to_last_isoweek_n.md)
and
[`isoyear_to_last_date()`](https://niphr.github.io/cstime/reference/isoyear_to_last_date.md)
answer the same question as a week number and as a date.
[`vignette("date_conversion", package = "cstime")`](https://niphr.github.io/cstime/articles/date_conversion.md)
for worked date, ISO year and ISO week conversions. No vignette runs
this function.

## Examples

``` r
isoyear_to_last_isoyearweek_c(c(2019, 2020, 2021))
#> [1] "2019-52" "2020-53" "2021-52"
isoyear_to_last_isoyearweek_c("2020")
#> [1] "2020-53"
```
