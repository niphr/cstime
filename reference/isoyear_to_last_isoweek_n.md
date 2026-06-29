# Last ISO week (numeric) in ISO year

Returns the last week in the isoyear

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

  ISO year, e.g. 2020

## Value

ISO week in numeric

## Examples

``` r
isoyear_to_last_isoweek_n(c(2019, 2019, 2020, 2021))
#> [1] 52 52 53 52
```
