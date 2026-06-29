# Last date in ISO yearweek

Returns the Sunday in the isoyearweek

## Usage

``` r
isoyearweek_to_last_date(x)

# Default S3 method
isoyearweek_to_last_date(x)

# S3 method for class 'character'
isoyearweek_to_last_date(x)
```

## Arguments

- x:

  ISO yearweek, e.g. "2020-19" for 19th week in 2020

## Value

Date of Sunday of that isoyearweek

## Examples

``` r
isoyearweek_to_last_date(c("2019-19", "2020-01"))
#> [1] "2019-05-12" "2020-01-05"
```
