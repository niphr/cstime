# ISO week to season week (numeric). Season week 1 is natural week 35.

ISO week to season week (numeric). Season week 1 is natural week 35.

## Usage

``` r
isoweek_to_seasonweek_n(x)

# Default S3 method
isoweek_to_seasonweek_n(x)

# S3 method for class 'character'
isoweek_to_seasonweek_n(x)

# S3 method for class 'numeric'
isoweek_to_seasonweek_n(x)
```

## Arguments

- x:

  ISO week in a year (numeric), between 1 and 53. ISO week 53 is season
  week 18.5

## Value

Season week in numeric

## Examples

``` r
isoweek_to_seasonweek_n(31)
#> [1] 49
```
