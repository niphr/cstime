# ISO year to last date (Sunday)

Returns the date of the Sunday that ends the last ISO week of a given
ISO year.

## Usage

``` r
isoyear_to_last_date(x)

# Default S3 method
isoyear_to_last_date(x)

# S3 method for class 'character'
isoyear_to_last_date(x)

# S3 method for class 'numeric'
isoyear_to_last_date(x)
```

## Arguments

- x:

  ISO year as a number or character string, e.g. 2020 or "2020".

## Value

A [base::Date](https://rdrr.io/r/base/Dates.html) vector giving the last
Sunday of each ISO year.

## Details

ISO weeks end on Sunday, so the returned date is the Sunday of the final
ISO week. Because ISO years and calendar years are not aligned, this
date can fall in early January of the following calendar year (for
example the last date of ISO year 2020 is 2021-01-03). The year is
accepted as either a number or a character string.

## Examples

``` r
isoyear_to_last_date(c(2019, 2020, 2021))
#> [1] "2019-12-29" "2021-01-03" "2022-01-02"
isoyear_to_last_date("2020")
#> [1] "2021-01-03"
```
