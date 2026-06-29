# Season week to ISO week (numeric). Season week 1 is ISO week 35.

Season week to ISO week (numeric). Season week 1 is ISO week 35.

## Usage

``` r
seasonweek_to_isoweek_n(x)

# Default S3 method
seasonweek_to_isoweek_n(x)

# S3 method for class 'numeric'
seasonweek_to_isoweek_n(x)
```

## Arguments

- x:

  Season week in a year, between 1 and 52

## Value

ISO week in numeric

## Examples

``` r
seasonweek_to_isoweek_n(31)
#> [1] 13
```
