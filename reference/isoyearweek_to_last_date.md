# ISO yearweek to last date (Sunday)

Returns the date of the Sunday that ends a given ISO yearweek.

## Usage

``` r
isoyearweek_to_last_date(x)

# Default S3 method
isoyearweek_to_last_date(x)

# S3 method for class 'character'
isoyearweek_to_last_date(x)
```

## Arguments

- x:

  ISO yearweek as a character string of the form "yyyy-ww", e.g.
  "2020-19" for the 19th ISO week of 2020.

## Value

A [base::Date](https://rdrr.io/r/base/Dates.html) vector giving the
Sunday of each ISO yearweek.

## Details

ISO weeks run Monday to Sunday, so the returned date is the Sunday of
the supplied yearweek.

## See also

[`isoyear_to_last_date()`](https://niphr.github.io/cstime/reference/isoyear_to_last_date.md)
and
[`season_to_last_date()`](https://niphr.github.io/cstime/reference/season_to_last_date.md)
do the same for an ISO year and for a season. Each takes a different
input grammar, so they are not interchangeable.
[`vignette("date_conversion", package = "cstime")`](https://niphr.github.io/cstime/articles/date_conversion.md)
for worked date, ISO year and ISO week conversions. No vignette runs
this function.

## Examples

``` r
isoyearweek_to_last_date(c("2019-19", "2020-01"))
#> [1] "2019-05-12" "2020-01-05"
```
