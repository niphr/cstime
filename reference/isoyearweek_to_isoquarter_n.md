# ISO yearweek to ISO quarter (numeric)

Maps an ISO yearweek to its ISO-week-based quarter (1 to 4), returned as
a number.

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

  ISO yearweek as a character string of the form "yyyy-ww", e.g.
  "2020-19" for the 19th ISO week of 2020.

## Value

ISO quarter as an integer vector (1 to 4).

## Details

The quarter is derived from the ISO week part of the input. Weeks 1 to
13 are quarter 1, weeks 14 to 26 are quarter 2, weeks 27 to 39 are
quarter 3, and weeks 40 onwards (including week 53) are quarter 4.

## Examples

``` r
isoyearweek_to_isoquarter_n("2020-19")
#> [1] 2
```
