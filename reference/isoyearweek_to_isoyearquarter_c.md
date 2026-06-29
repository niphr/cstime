# ISO yearweek to ISO yearquarter (character)

ISO yearweek to ISO yearquarter (character)

## Usage

``` r
isoyearweek_to_isoyearquarter_c(x)

# Default S3 method
isoyearweek_to_isoyearquarter_c(x)

# S3 method for class 'character'
isoyearweek_to_isoyearquarter_c(x)
```

## Arguments

- x:

  Year-week, e.g. "2020-19" for 19th week in 2020

## Value

ISO yearquarter in character

## Examples

``` r
isoyearweek_to_isoyearquarter_c('2020-19')
#> [1] "2020-Q2"
```
