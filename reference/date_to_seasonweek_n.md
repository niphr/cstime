# Date to season week (numeric)

Maps a date to its position within the surveillance season, where season
week 1 corresponds to ISO week 35.

## Usage

``` r
date_to_seasonweek_n(x)
```

## Arguments

- x:

  A Date object, or a character string in the format 'yyyy-mm-dd'.

## Value

Season week as a numeric vector.

## Details

The date is first converted to an ISO yearweek with
[`date_to_isoyearweek_c()`](https://niphr.github.io/cstime/reference/date_to_isoyearweek_c.md)
and then to a season week with
[`isoyearweek_to_seasonweek_n()`](https://niphr.github.io/cstime/reference/isoyearweek_to_seasonweek_n.md).
As with
[`isoweek_to_seasonweek_n()`](https://niphr.github.io/cstime/reference/isoweek_to_seasonweek_n.md),
ISO week 53 maps to the half-step season week 18.5.

## See also

[`date_to_season_c()`](https://niphr.github.io/cstime/reference/date_to_season_c.md)
for the season itself, and
[`isoyearweek_to_seasonweek_n()`](https://niphr.github.io/cstime/reference/isoyearweek_to_seasonweek_n.md)
to start from an ISO yearweek instead.
[`vignette("season", package = "cstime")`](https://niphr.github.io/cstime/articles/season.md)
for worked season week conversions. No vignette runs this function.

## Examples

``` r
date_to_seasonweek_n(c("2021-01-01", "2021-12-01"))
#> [1] 18.5 14.0
date_to_seasonweek_n(as.Date("2021-09-01"))
#> [1] 1
```
