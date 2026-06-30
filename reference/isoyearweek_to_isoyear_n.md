# ISO yearweek to ISO year (numeric)

Extracts the ISO year from an ISO yearweek string and returns it as a
number.

## Usage

``` r
isoyearweek_to_isoyear_n(x)

# Default S3 method
isoyearweek_to_isoyear_n(x)

# S3 method for class 'character'
isoyearweek_to_isoyear_n(x)
```

## Arguments

- x:

  ISO yearweek as a character string of the form "yyyy-ww", e.g.
  "2020-19" for the 19th ISO week of 2020.

## Value

ISO year as an integer vector (e.g. 2020).

## Details

The input is split on the hyphen into year and week, and the year part
is returned. The week part is ignored.

## Examples

``` r
isoyearweek_to_isoyear_n("2020-10")
#> [1] 2020
```
