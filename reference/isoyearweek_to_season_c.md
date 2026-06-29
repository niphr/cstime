# ISO yearweek to season.

ISO yearweek to season.

## Usage

``` r
isoyearweek_to_season_c(x)

# Default S3 method
isoyearweek_to_season_c(x)

# S3 method for class 'character'
isoyearweek_to_season_c(x)
```

## Arguments

- x:

  isoyearweek, connected with '-'

## Value

Season, e.g. 2020/2021

## Examples

``` r
isoyearweek_to_season_c(c("2021-01", "2021-50"))
#> [1] "2020/2021" "2021/2022"
```
