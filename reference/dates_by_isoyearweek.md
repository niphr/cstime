# Dates of different days within isoyearweeks

Dates of different days within isoyearweeks

## Usage

``` r
dates_by_isoyearweek
```

## Format

- isoyear:

  Isoyear.

- isoyearweek:

  Isoweek-isoyear.

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

  List of dates from Mon-Fri

- weekend:

  List of dates from Sat-Sun

- days:

  List of dates from Mon-Sun

## Examples

``` r
# Constructing a vector of dates without removing the Date class
do.call("c", dates_by_isoyearweek[isoyearweek %in% c("2021-01", "2021-02")]$weekdays)
#>  [1] "2021-01-04" "2021-01-05" "2021-01-06" "2021-01-07" "2021-01-08"
#>  [6] "2021-01-11" "2021-01-12" "2021-01-13" "2021-01-14" "2021-01-15"
```
