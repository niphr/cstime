# ISO yearweek to week (numeric)

This function breaks the string connected with '-' into year/week

## Usage

``` r
isoyearweek_to_isoweek_n(x)

# Default S3 method
isoyearweek_to_isoweek_n(x)

# S3 method for class 'character'
isoyearweek_to_isoweek_n(x)
```

## Arguments

- x:

  Year-week, e.g. "2020-19" for 19th week in 2020

## Value

ISO week in numeric

## Examples

``` r
isoyearweek_to_isoweek_n('2020-19')
#> [1] 19
```
