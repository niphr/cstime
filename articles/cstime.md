# Intro to cstime

`cstime` provides date and time functions for public health
surveillance. Its main purpose is consistent conversion between:

- Date.
- ISO year, ISO week, ISO yearweek.
- Season week.

`cstime` is part of the [csverse](https://niphr.github.io/packages.html)
package suite.

``` r
library(cstime)
#> cstime 2026.8.21
#> https://niphr.github.io/cstime/
```

## What cstime is for

Public health surveillance counts cases by week. The week is the unit of
analysis, not the calendar month. `cstime` converts a date into the ISO
week, ISO year, season or quarter that contains it, and converts back
the other way.

## How the function names work

Almost every name is built the same way:

    <from>_to_<to>_<suffix>

The suffix says what type comes back. `_c` returns a character string,
`_n` returns a number, `_date` returns a `Date`.

``` r
date_to_isoyearweek_c("2021-01-01")
#> [1] "2020-53"
isoyearweek_to_isoyear_n("2021-01")
#> [1] 2021
isoyearweek_to_last_date("2021-01")
#> [1] "2021-01-10"
```

cstime exports 33 functions. All 33 end in `_c` (17 of them), `_n` (13)
or `_date` (3). All but one also follow `<from>_to_<to>`. The exception
is [`now_c()`](https://niphr.github.io/cstime/reference/now_c.md), which
reads the clock instead of converting. Once you know the pattern, you
can guess a function name instead of searching for it.

## Three things that are easy to get wrong

**An ISO year is not a calendar year.** 2020-12-31 and 2021-01-01 sit in
different calendar years, but both sit in ISO yearweek 2020-53.

``` r
d <- as.Date(c("2020-12-31", "2021-01-01"))
data.frame(
  date = d,
  calyear = date_to_calyear_n(d),
  isoyear = date_to_isoyear_n(d),
  isoyearweek = date_to_isoyearweek_c(d)
)
#>         date calyear isoyear isoyearweek
#> 1 2020-12-31    2020    2020     2020-53
#> 2 2021-01-01    2021    2020     2020-53
```

**An ISO year has 52 or 53 weeks.** Do not assume 52. 2015, 2020 and
2026 each have 53.

``` r
data.frame(
  isoyear = 2015:2026,
  weeks_in_isoyear = isoyear_to_last_isoweek_n(2015:2026)
)
#>    isoyear weeks_in_isoyear
#> 1     2015               53
#> 2     2016               52
#> 3     2017               52
#> 4     2018               52
#> 5     2019               52
#> 6     2020               53
#> 7     2021               52
#> 8     2022               52
#> 9     2023               52
#> 10    2024               52
#> 11    2025               52
#> 12    2026               53
```

**A season week is not an ISO week.** A season starts in ISO week 35 and
runs past New Year, so one season covers two ISO years. A season also
numbers its own weeks from 1, so ISO week 35 is season week 1.

``` r
d <- as.Date(c("2020-08-24", "2020-12-28", "2021-01-04", "2021-08-29"))
data.frame(
  date = d,
  isoyearweek = date_to_isoyearweek_c(d),
  season = date_to_season_c(d)
)
#>         date isoyearweek    season
#> 1 2020-08-24     2020-35 2020/2021
#> 2 2020-12-28     2020-53 2020/2021
#> 3 2021-01-04     2021-01 2020/2021
#> 4 2021-08-29     2021-34 2020/2021

isoweek_to_seasonweek_n(c(35, 52, 1, 34))
#> [1]  1 18 19 52
```

## cstime and the other cs packages

`cstime` needs no other cs package to run. `cstidy` needs cstime: when
cstidy fills the time columns of a surveillance table – isoyear,
isoweek, season, quarter – those values come from cstime.

## Where to go next

- [Date, year, week
  conversion](https://niphr.github.io/cstime/articles/date_conversion.md)
  covers dates, ISO years and ISO weeks.
- [Season week](https://niphr.github.io/cstime/articles/season.md)
  covers season week numbering, including what happens in ISO week 53.

## Date to ISO year

``` r
date_to_isoyear_c('2021-01-01')
#> [1] "2020"
```

## ISO yearweek string to ISO year and ISO week

``` r
isoyearweek_to_isoyear_c("2021-02")
#> [1] "2021"
isoyearweek_to_isoweek_c("2021-02")
#> [1] "02"
```

## Season week and ISO week

``` r
seasonweek_to_isoweek_n(10)
#> [1] 44
isoweek_to_seasonweek_n(1)
#> [1] 19
```
