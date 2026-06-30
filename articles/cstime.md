# Intro to cstime

`cstime` provides date and time functions for public health
surveillance. Its main purpose is consistent conversion between:

- Date.
- Isoyear, isoweek, isoyearweek.
- Season week.

It also provides functions to generate rolling date ranges for computing
weekly, bi-weekly, and 4-week averages.

`cstime` is part of the [csverse](https://niphr.github.io/packages.html)
package suite.

``` r
library(cstime)
#> cstime 2025.10.13
#> https://niphr.github.io/cstime/
library(magrittr)
```

## Date to isoyear

``` r
date_to_isoyear_c('2021-01-01')
#> [1] "2020"
```

## Isoyearweek string to isoyear and isoweek

``` r
isoyearweek_to_isoyear_c("2021-02")
#> [1] "2021"
isoyearweek_to_isoweek_c("2021-02")
#> [1] "02"
```

## Season week and isoweek

``` r
seasonweek_to_isoweek_n(10)
#> [1] 44
isoweek_to_seasonweek_n(1)
#> [1] 19
```
