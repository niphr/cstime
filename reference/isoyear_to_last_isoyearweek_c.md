# Last ISO yearweek (character) in ISO year

Returns the last isoyearweek in the isoyear

## Usage

``` r
isoyear_to_last_isoyearweek_c(x)

# Default S3 method
isoyear_to_last_isoyearweek_c(x)

# S3 method for class 'character'
isoyear_to_last_isoyearweek_c(x)

# S3 method for class 'numeric'
isoyear_to_last_isoyearweek_c(x)
```

## Arguments

- x:

  ISO year, e.g. 2020

## Value

ISO year-week in character, of the last ISO year

## Examples

``` r
isoyear_to_last_isoyearweek_c(c(2019, 2019, 2020, 2021))
#> [1] "2019-52" "2019-52" "2020-53" "2021-52"
```
