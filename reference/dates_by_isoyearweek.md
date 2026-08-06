# Dates of each weekday within ISO yearweeks

A lookup table with one row per ISO yearweek, for ISO years 1900 to
2099. Each row holds the date of every day of that week, Monday to
Sunday, plus list-columns that group those dates. The 10436 rows run
from "1900-01" to "2099-53", so the dates covered are 1900-01-01 to
2100-01-03.

## Usage

``` r
dates_by_isoyearweek
```

## Format

A
[data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html)
with one row per ISO yearweek and the following columns:

- isoyear:

  Integer. ISO year.

- isoyearweek:

  Character. ISO yearweek, "yyyy-ww".

- mon:

  Date of Monday.

- tue:

  Date of Tuesday.

- wed:

  Date of Wednesday.

- thu:

  Date of Thursday.

- fri:

  Date of Friday.

- sat:

  Date of Saturday.

- sun:

  Date of Sunday.

- weekdays:

  List column. The dates Monday to Friday.

- weekend:

  List column. The dates Saturday and Sunday.

- days:

  List column. The dates Monday to Sunday.

## Source

Generated from a daily date sequence using ISO 8601 week rules. See
`data-raw/1_gen-data.R` in the package source.

## Examples

``` r
library(data.table)
#> 
#> Attaching package: ‘data.table’
#> The following object is masked from ‘package:base’:
#> 
#>     %notin%
# Look up the Monday and Sunday of two ISO yearweeks
dates_by_isoyearweek[isoyearweek %in% c("2021-01", "2021-02"), .(isoyearweek, mon, sun)]
#>    isoyearweek        mon        sun
#>         <char>     <Date>     <Date>
#> 1:     2021-01 2021-01-04 2021-01-10
#> 2:     2021-02 2021-01-11 2021-01-17

# Constructing a vector of dates without removing the Date class
do.call("c", dates_by_isoyearweek[isoyearweek %in% c("2021-01", "2021-02")]$weekdays)
#>  [1] "2021-01-04" "2021-01-05" "2021-01-06" "2021-01-07" "2021-01-08"
#>  [6] "2021-01-11" "2021-01-12" "2021-01-13" "2021-01-14" "2021-01-15"
```
