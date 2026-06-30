# ISO year to last ISO week (numeric)

Returns the number of the last ISO week in a given ISO year, that is,
the count of ISO weeks in that year.

## Usage

``` r
isoyear_to_last_isoweek_n(x)

# Default S3 method
isoyear_to_last_isoweek_n(x)

# S3 method for class 'character'
isoyear_to_last_isoweek_n(x)

# S3 method for class 'numeric'
isoyear_to_last_isoweek_n(x)
```

## Arguments

- x:

  ISO year as a number or character string, e.g. 2020 or "2020".

## Value

Last ISO week of the year as an integer vector (52 or 53).

## Details

This is 52 for most years and 53 for long ISO years such as 2020. The
year is accepted as either a number or a character string.

## Examples

``` r
isoyear_to_last_isoweek_n(c(2019, 2020, 2021))
#> [1] 52 53 52
isoyear_to_last_isoweek_n("2020")
#> [1] 53
```
