# ISO yearweek to season (character)

Maps an ISO yearweek to the surveillance season it belongs to, written
as "yyyy/yyyy".

## Usage

``` r
isoyearweek_to_season_c(x)

# Default S3 method
isoyearweek_to_season_c(x)

# S3 method for class 'character'
isoyearweek_to_season_c(x)
```

## Arguments

- x:

  ISO yearweek as a character string of the form "yyyy-ww", e.g.
  "2021-01".

## Value

Season as a character vector (e.g. "2020/2021").

## Details

Seasons start at ISO week 35 (season week 1). ISO weeks 35 and later
belong to the season beginning in that calendar year, while earlier
weeks belong to the season that began the previous calendar year. For
example "2021-01" falls in season "2020/2021" and "2021-50" falls in
season "2021/2022".

## See also

[`isoyearweek_to_seasonweek_n()`](https://niphr.github.io/cstime/reference/isoyearweek_to_seasonweek_n.md)
for the week within that season, and
[`season_to_last_date()`](https://niphr.github.io/cstime/reference/season_to_last_date.md)
for the date the season ends.
[`vignette("season", package = "cstime")`](https://niphr.github.io/cstime/articles/season.md)
for worked season week conversions. No vignette runs this function.

## Examples

``` r
isoyearweek_to_season_c(c("2021-01", "2021-50"))
#> [1] "2020/2021" "2021/2022"
```
