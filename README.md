# cstime <a href="https://niphr.github.io/cstime/"><img src="man/figures/logo.png" align="right" width="120" /></a>

[![CRAN status](https://www.r-pkg.org/badges/version/cstime)](https://cran.r-project.org/package=cstime)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/cstime)](https://cran.r-project.org/package=cstime)

## Overview

[cstime](https://niphr.github.io/cstime/) provides date and time functions for public health purposes.

Public health surveillance counts events by ISO week, not by calendar month. It aligns seasonal
outcomes such as influenza to a season rather than to a year. cstime converts between date, ISO
week, ISO yearweek, ISO year, ISO quarter, calendar month and year, season and season week.
Every conversion is named `<from>_to_<to>`, and the suffix says what comes back: `_c` a character
string, `_n` a number, `_date` a `Date`.

cstime also ships three reference datasets.

## Installation

```r
install.packages("cstime")

# development version
# remotes::install_github("niphr/cstime")
```

## Quick start

```r
library(cstime)

date_to_isoyearweek_c(as.Date("2021-01-01"))
#> [1] "2020-53"

isoyearweek_to_season_c("2020-53")
#> [1] "2020/2021"

isoyear_to_last_isoweek_n(c(2020, 2021))
#> [1] 53 52
```

Those three results are the reason this package exists. 2021-01-01 is a Friday, and it falls in
ISO week 53 of ISO year 2020. Its ISO year and its calendar year are therefore not the same. That
yearweek sits in the 2020/2021 season. ISO year 2020 runs to 53 weeks where ISO year 2021 stops
at 52, so week-of-year arithmetic MUST NOT assume a fixed 52.

## Which function do I want?

| I have | I want | Use |
|---|---|---|
| A date | Calendar month or year | `date_to_calmonth_c()`, `date_to_calyear_c()`, `date_to_calyearmonth_c()` |
| A date | ISO week, year or quarter | `date_to_isoweek_c()`, `date_to_isoyear_c()`, `date_to_isoyearweek_c()`, `date_to_isoquarter_c()`, `date_to_isoyearquarter_c()` |
| A date | Season or season week | `date_to_season_c()`, `date_to_seasonweek_n()` |
| An ISO yearweek, `"2021-01"` | Its year, week, quarter or season | `isoyearweek_to_isoyear_c()`, `isoyearweek_to_isoweek_c()`, `isoyearweek_to_isoquarter_c()`, `isoyearweek_to_isoyearquarter_c()`, `isoyearweek_to_season_c()`, `isoyearweek_to_seasonweek_n()` |
| An ISO year | Its last week or last date | `isoyear_to_last_isoweek_n()`, `isoyear_to_last_isoyearweek_c()`, `isoyear_to_last_date()` |
| An ISO yearweek or a season | The Sunday that ends it | `isoyearweek_to_last_date()`, `season_to_last_date()` |
| An ISO week or a season week | The other one | `isoweek_to_seasonweek_n()`, `seasonweek_to_isoweek_n()`, `seasonweek_to_isoweek_c()` |
| A reference calendar | Weekday dates or Norwegian workdays | `dates_by_isoyearweek`, `nor_workdays_by_date`, `nor_workdays_by_isoyearweek` |

`now_c()` formats the current system time and is the one function that does not convert.

## Documentation

Full reference and articles are on the package website:
<https://niphr.github.io/cstime/>.

- `vignette("cstime")` — introduction.
- `vignette("date_conversion")` — date, ISO year and ISO week.
- `vignette("season")` — season week.
