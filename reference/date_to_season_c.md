# Date to season (character)

Maps a date to the surveillance season it belongs to, written as
"yyyy/yyyy".

## Usage

``` r
date_to_season_c(x)
```

## Arguments

- x:

  A Date object, or a character string in the format 'yyyy-mm-dd'.

## Value

Season as a character vector (e.g. "2020/2021").

## Details

The date is first converted to an ISO yearweek with
[`date_to_isoyearweek_c()`](https://niphr.github.io/cstime/reference/date_to_isoyearweek_c.md)
and then to a season with
[`isoyearweek_to_season_c()`](https://niphr.github.io/cstime/reference/isoyearweek_to_season_c.md).
Seasons start at ISO week 35, so dates in early January belong to the
season that began the previous calendar year.

## Examples

``` r
date_to_season_c(c("2021-01-01", "2021-12-01"))
#> [1] "2020/2021" "2021/2022"
date_to_season_c(as.Date("2021-09-01"))
#> [1] "2021/2022"
```
