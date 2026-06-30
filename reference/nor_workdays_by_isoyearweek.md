# Norwegian workdays and holidays by ISO yearweek

For each complete (7-day) ISO yearweek, the proportion of days that are
public holidays, free days and workdays in Norway.

## Usage

``` r
nor_workdays_by_isoyearweek
```

## Format

A
[data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html)
with one row per ISO yearweek and the following columns:

- isoyearweek:

  Character. ISO yearweek, "yyyy-ww".

- public_holiday:

  The proportion of days in the ISO yearweek that are public holidays.

- freeday:

  The proportion of days in the ISO yearweek that are public holidays or
  Saturday/Sunday.

- workday:

  1 minus `freeday`.

## Source

Aggregated from
[nor_workdays_by_date](https://niphr.github.io/cstime/reference/nor_workdays_by_date.md).
See `data-raw/1_gen-data.R` in the package source.

## Examples

``` r
head(nor_workdays_by_isoyearweek)
#> Key: <isoyearweek>
#>    isoyearweek public_holiday freeday workday
#>         <char>          <num>   <num>   <num>
#> 1:     2000-01              0    0.29    0.71
#> 2:     2000-02              0    0.29    0.71
#> 3:     2000-03              0    0.29    0.71
#> 4:     2000-04              0    0.29    0.71
#> 5:     2000-05              0    0.29    0.71
#> 6:     2000-06              0    0.29    0.71

# ISO yearweeks that contain at least one public holiday
head(nor_workdays_by_isoyearweek[nor_workdays_by_isoyearweek$public_holiday > 0, ])
#> Key: <isoyearweek>
#>    isoyearweek public_holiday freeday workday
#>         <char>          <num>   <num>   <num>
#> 1:     2000-15           0.14    0.29    0.71
#> 2:     2000-16           0.43    0.57    0.43
#> 3:     2000-17           0.14    0.43    0.57
#> 4:     2000-18           0.14    0.43    0.57
#> 5:     2000-20           0.14    0.43    0.57
#> 6:     2000-22           0.14    0.43    0.57
```
