# ISO yearweek to ISO week (numeric)

Extracts the ISO week from an ISO yearweek string and returns it as a
number.

## Usage

``` r
isoyearweek_to_isoweek_n(x)

# Default S3 method
isoyearweek_to_isoweek_n(x)

# S3 method for class 'character'
isoyearweek_to_isoweek_n(x)
```

## Arguments

- x:

  ISO yearweek as a character string of the form "yyyy-ww", e.g.
  "2020-19" for the 19th ISO week of 2020.

## Value

ISO week as an integer vector (1 to 53).

## Details

The input is split on the hyphen into year and week, and the week part
is returned. The year part is ignored.

## Examples

``` r
isoyearweek_to_isoweek_n("2020-19")
#> [1] 19
```
