# Date, year, week conversion

These functions simplify working with dates and times across different
formats.

Inputs can be numeric (e.g. `3`) or character (e.g. `"03"`,
`"2020-03"`). The output type is controlled by the function suffix:
functions ending in `_c` return a character, those ending in `_n` return
a double.

``` r
library(cstime)
#> cstime 2026.8.6
#> https://niphr.github.io/cstime/
library(magrittr)
```

## Which year and week is this date?

When called with no arguments, the functions return the isoyear or
isoweek of today.

``` r
date_to_isoyear_c()
#> [1] "2026"
date_to_isoyear_n()
#> [1] 2026
date_to_isoweek_c()
#> [1] "32"
date_to_isoweek_n()
#> [1] 32
# provide a date
date_to_isoyear_c('2021-01-01')
#> [1] "2020"
date_to_isoyear_n('2021-01-01')
#> [1] 2020
date_to_isoweek_c('2021-01-01')
#> [1] "53"
date_to_isoweek_n('2021-01-01')
#> [1] 53
date_to_isoyearweek_c('2021-08-11')
#> [1] "2021-32"
```

## Get isoyear and isoweek from an isoyearweek string

``` r
isoyearweek_to_isoyear_c("2021-02")
#> [1] "2021"
isoyearweek_to_isoyear_n("2021-02")
#> [1] 2021
isoyearweek_to_isoweek_c("2021-02")
#> [1] "02"
isoyearweek_to_isoweek_n("2021-02")
#> [1] 2
```

The built-in dataset `dates_by_isoyearweek` provides a reference table.
Here is how to subset it to specific years:

``` r
yrwk_19_20 <- dates_by_isoyearweek[isoyear %in% c(2019, 2020)]
head(yrwk_19_20)
#> Key: <isoyear, isoyearweek, mon, tue, wed, thu, fri, sat, sun>
#>    isoyear isoyearweek        mon        tue        wed        thu        fri
#>      <int>      <char>     <Date>     <Date>     <Date>     <Date>     <Date>
#> 1:    2019     2019-01 2018-12-31 2019-01-01 2019-01-02 2019-01-03 2019-01-04
#> 2:    2019     2019-02 2019-01-07 2019-01-08 2019-01-09 2019-01-10 2019-01-11
#> 3:    2019     2019-03 2019-01-14 2019-01-15 2019-01-16 2019-01-17 2019-01-18
#> 4:    2019     2019-04 2019-01-21 2019-01-22 2019-01-23 2019-01-24 2019-01-25
#> 5:    2019     2019-05 2019-01-28 2019-01-29 2019-01-30 2019-01-31 2019-02-01
#> 6:    2019     2019-06 2019-02-04 2019-02-05 2019-02-06 2019-02-07 2019-02-08
#>           sat        sun                                               weekdays
#>        <Date>     <Date>                                                 <list>
#> 1: 2019-01-05 2019-01-06 2018-12-31,2019-01-01,2019-01-02,2019-01-03,2019-01-04
#> 2: 2019-01-12 2019-01-13 2019-01-07,2019-01-08,2019-01-09,2019-01-10,2019-01-11
#> 3: 2019-01-19 2019-01-20 2019-01-14,2019-01-15,2019-01-16,2019-01-17,2019-01-18
#> 4: 2019-01-26 2019-01-27 2019-01-21,2019-01-22,2019-01-23,2019-01-24,2019-01-25
#> 5: 2019-02-02 2019-02-03 2019-01-28,2019-01-29,2019-01-30,2019-01-31,2019-02-01
#> 6: 2019-02-09 2019-02-10 2019-02-04,2019-02-05,2019-02-06,2019-02-07,2019-02-08
#>                  weekend
#>                   <list>
#> 1: 2019-01-05,2019-01-06
#> 2: 2019-01-12,2019-01-13
#> 3: 2019-01-19,2019-01-20
#> 4: 2019-01-26,2019-01-27
#> 5: 2019-02-02,2019-02-03
#> 6: 2019-02-09,2019-02-10
#>                                                                        days
#>                                                                      <list>
#> 1: 2018-12-31,2019-01-01,2019-01-02,2019-01-03,2019-01-04,2019-01-05,...[7]
#> 2: 2019-01-07,2019-01-08,2019-01-09,2019-01-10,2019-01-11,2019-01-12,...[7]
#> 3: 2019-01-14,2019-01-15,2019-01-16,2019-01-17,2019-01-18,2019-01-19,...[7]
#> 4: 2019-01-21,2019-01-22,2019-01-23,2019-01-24,2019-01-25,2019-01-26,...[7]
#> 5: 2019-01-28,2019-01-29,2019-01-30,2019-01-31,2019-02-01,2019-02-02,...[7]
#> 6: 2019-02-04,2019-02-05,2019-02-06,2019-02-07,2019-02-08,2019-02-09,...[7]
```
