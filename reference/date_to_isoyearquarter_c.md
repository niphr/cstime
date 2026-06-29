# ISO year and quarter (character) from Date object

ISO year and quarter (character) from Date object

## Usage

``` r
date_to_isoyearquarter_c(x = lubridate::today())

# Default S3 method
date_to_isoyearquarter_c(x)

# S3 method for class 'character'
date_to_isoyearquarter_c(x = lubridate::today())

# S3 method for class 'Date'
date_to_isoyearquarter_c(x = lubridate::today())
```

## Arguments

- x:

  a Date object or string, in the form of 'yyyy-mm-dd'

## Value

ISO year and quarter in character

## Examples

``` r
date_to_isoyearquarter_c("2021-08-11")
#> [1] "2021-Q3"
date_to_isoyearquarter_c(lubridate::today())
#> [1] "2026-Q3"
```
