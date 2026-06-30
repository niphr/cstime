# ISO yearweek to ISO quarter (character)

Maps an ISO yearweek to its ISO-week-based quarter (1 to 4), returned as
a character string.

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

  ISO yearweek as a character string of the form "yyyy-ww", e.g.
  "2020-19" for the 19th ISO week of 2020.

## Value

ISO quarter as a character vector (e.g. "2").

## Details

The quarter is derived from the ISO week part of the input. See
[`isoyearweek_to_isoquarter_n()`](https://niphr.github.io/cstime/reference/isoyearweek_to_isoquarter_n.md)
for the week-to-quarter boundaries.

## Examples

``` r
isoyearweek_to_isoquarter_c("2020-19")
#> [1] "2"
```
