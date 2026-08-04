# Season to last date (Sunday)

Returns the date of the Sunday that ends a given season.

## Usage

``` r
season_to_last_date(x)

# Default S3 method
season_to_last_date(x)

# S3 method for class 'character'
season_to_last_date(x)
```

## Arguments

- x:

  Season as a character string of the form "yyyy/yyyy", e.g.
  "2019/2020".

## Value

A [base::Date](https://rdrr.io/r/base/Dates.html) vector giving the last
Sunday of each season.

## Details

A season is written "yyyy/yyyy" where the two years are consecutive (for
example "2019/2020"). Seasons are aligned to ISO weeks, with season week
1 starting at ISO week 35; the season therefore ends in late summer of
the second year. The returned date is the Sunday of the final week of
the season.

## See also

[`isoyearweek_to_season_c()`](https://niphr.github.io/cstime/reference/isoyearweek_to_season_c.md)
finds the season an ISO yearweek belongs to.
[`isoyear_to_last_date()`](https://niphr.github.io/cstime/reference/isoyear_to_last_date.md)
and
[`isoyearweek_to_last_date()`](https://niphr.github.io/cstime/reference/isoyearweek_to_last_date.md)
do the same for an ISO year and for an ISO yearweek.
[`vignette("season", package = "cstime")`](https://niphr.github.io/cstime/articles/season.md)
for worked season week conversions. No vignette runs this function.

## Examples

``` r
season_to_last_date(c("2019/2020", "2020/2021"))
#> [1] "2020-08-23" "2021-08-29"
```
