# ISO yearweek to year (character)

This function breaks the string connected with '-' into year/week

## Usage

``` r
isoyearweek_to_isoyear_c(x)

# Default S3 method
isoyearweek_to_isoyear_c(x)

# S3 method for class 'character'
isoyearweek_to_isoyear_c(x)
```

## Arguments

- x:

  Year-week, e.g. "2020-19" for 19th week in 2020

## Value

ISO year in character

## Examples

``` r
isoyearweek_to_isoyear_c('2020-10')
#> [1] "2020"
```
