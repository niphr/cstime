# Intro to cstime

`cstime` provides date time functions for public health purposes.

The core functionality is consistent time conversion between :

- Date.
- Isoyear, isoweek, isoyearweek.
- Season week.

It also provides functions to generate rolling dates for computing the
weekly, bi-weekly, 4 week averages.

`cstime` is part of the [csverse](https://www.csids.no/packages.html)
package suite.

``` r
library(cstime)
#> cstime 2025.10.13
#> https://niphr.github.io/cstime/
library(magrittr)
```

To convert a date to isoyear:

``` r
date_to_isoyear_c('2021-01-01')
#> [1] "2020"
```

To convert a isoyearweek string to isoyear/isoweek:

``` r
isoyearweek_to_isoyear_c("2021-02")
#> [1] "2021"
isoyearweek_to_isoweek_c("2021-02")
#> [1] "02"
```

To convert a season week to isoweek (and reverse):

``` r
seasonweek_to_isoweek_n(10)
#> [1] 44
isoweek_to_seasonweek_n(1)  
#> [1] 19
```
