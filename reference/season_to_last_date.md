# Last date in season

Returns the last Sunday in the season

## Usage

``` r
season_to_last_date(x)

# Default S3 method
season_to_last_date(x)

# S3 method for class 'character'
season_to_last_date(x)
```

## Arguments

- x:

  Season, e.g. "2019/2020"

## Value

Date of last Sunday of that season

## Examples

``` r
isoyearweek_to_last_date(c("2019-19", "2020-01"))
#> [1] "2019-05-12" "2020-01-05"
```
