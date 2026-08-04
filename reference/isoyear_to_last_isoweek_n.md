# ISO year to last ISO week (numeric)

Returns the number of the last ISO week in a given ISO year, that is,
the count of ISO weeks in that year.

## Usage

``` r
isoyear_to_last_isoweek_n(x)

# Default S3 method
isoyear_to_last_isoweek_n(x)

# S3 method for class 'character'
isoyear_to_last_isoweek_n(x)

# S3 method for class 'numeric'
isoyear_to_last_isoweek_n(x)
```

## Arguments

- x:

  ISO year as a number or character string, e.g. 2020 or "2020".

## Value

Last ISO week of the year as an integer vector (52 or 53).

## Details

This is 52 for most years and 53 for long ISO years such as 2020. The
year is accepted as either a number or a character string.

## See also

[`isoyear_to_last_isoyearweek_c()`](https://niphr.github.io/cstime/reference/isoyear_to_last_isoyearweek_c.md)
and
[`isoyear_to_last_date()`](https://niphr.github.io/cstime/reference/isoyear_to_last_date.md)
answer the same question as a yearweek string and as a date.
[`vignette("date_conversion", package = "cstime")`](https://niphr.github.io/cstime/articles/date_conversion.md)
for worked date, ISO year and ISO week conversions. No vignette runs
this function.

## Examples

``` r
isoyear_to_last_isoweek_n(c(2019, 2020, 2021))
#> [1] 52 53 52
isoyear_to_last_isoweek_n("2020")
#> [1] 53
```
