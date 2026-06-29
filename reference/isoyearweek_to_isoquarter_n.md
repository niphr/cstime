# ISO yearweek to quarter (numeric)

This function breaks the string connected with '-' into year/quarter

## Usage

``` r
isoyearweek_to_isoquarter_n(x)

# Default S3 method
isoyearweek_to_isoquarter_n(x)

# S3 method for class 'character'
isoyearweek_to_isoquarter_n(x)
```

## Arguments

- x:

  Year-week, e.g. "2020-19" for 19th week in 2020

## Value

ISO quarter in numeric

## Examples

``` r
isoyearweek_to_isoquarter_n('2020-19')
#> [1] 2
```
