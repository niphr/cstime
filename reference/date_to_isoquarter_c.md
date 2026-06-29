# ISO quarter (character) from Date object

ISO quarter (character) from Date object

## Usage

``` r
date_to_isoquarter_c(x = lubridate::today())

# Default S3 method
date_to_isoquarter_c(x)

# S3 method for class 'character'
date_to_isoquarter_c(x = lubridate::today())

# S3 method for class 'Date'
date_to_isoquarter_c(x = lubridate::today())
```

## Arguments

- x:

  a Date object or string, in the form of 'yyyy-mm-dd'

## Value

ISO quarter in character

## Examples

``` r
date_to_isoquarter_c("2021-08-11")
#> [1] "3"
date_to_isoquarter_c(lubridate::today())
#> [1] "3"
```
