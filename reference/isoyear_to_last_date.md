# Last Sunday in ISO year

Returns the last Sunday in the isoyear

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

  ISO year, e.g. 2020

## Value

Date of the Sunday, for the last week in the isoyear

## Examples

``` r
isoyear_to_last_date(c(2019, 2019, 2020, 2021))
#> [1] "2019-12-29" "2019-12-29" "2021-01-03" "2022-01-02"
```
