# ISO yearweek to quarter (character)

This function breaks the string connected with '-' into year/quarter

## Usage

``` r
isoyearweek_to_isoquarter_c(x)

# Default S3 method
isoyearweek_to_isoquarter_c(x)

# S3 method for class 'character'
isoyearweek_to_isoquarter_c(x)
```

## Arguments

- x:

  Year-week, e.g. "2020-19" for 19th week in 2020

## Value

ISO quarter in character

## Examples

``` r
isoyearweek_to_isoquarter_c('2020-19')
#> [1] "2"
```
