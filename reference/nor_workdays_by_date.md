# Norwegian workdays and holidays by date

A daily calendar flagging Norwegian public holidays, weekends and
workdays for each date in the covered range.

## Usage

``` r
nor_workdays_by_date
```

## Format

A
[data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html)
with one row per date and the following columns:

- date:

  Date.

- day_of_week:

  Integer. 1 = Monday, 7 = Sunday.

- mon_to_fri:

  Integer. 1 if Monday to Friday, 0 if Saturday or Sunday.

- sat_to_sun:

  Integer. 1 if Saturday or Sunday, 0 if Monday to Friday.

- public_holiday:

  Integer. 1 if a public holiday (helligdag), 0 otherwise.

- freeday:

  Integer. 1 if a public holiday or Saturday/Sunday, 0 otherwise.

- workday:

  Integer. 1 if `freeday` is 0, 0 if `freeday` is 1.

## Source

Derived from the Norwegian public-holiday list in
`inst/rawdata/norway_holidays.xlsx`. See `data-raw/1_gen-data.R` in the
package source.

## Examples

``` r
head(nor_workdays_by_date)
#>          date day_of_week mon_to_fri sat_to_sun public_holiday freeday workday
#>        <Date>       <num>      <num>      <num>          <num>   <num>   <num>
#> 1: 2000-01-01           6          0          1              0       1       0
#> 2: 2000-01-02           7          0          1              0       1       0
#> 3: 2000-01-03           1          1          0              0       0       1
#> 4: 2000-01-04           2          1          0              0       0       1
#> 5: 2000-01-05           3          1          0              0       0       1
#> 6: 2000-01-06           4          1          0              0       0       1

# Count the workdays in the dataset
sum(nor_workdays_by_date$workday)
#> [1] 7571
```
