# ISO yearweek to ISO yearquarter (character)

Maps an ISO yearweek to a combined ISO year and quarter string of the
form "yyyy-Qn".

## Usage

``` r
isoyearweek_to_isoyearquarter_c(x)

# Default S3 method
isoyearweek_to_isoyearquarter_c(x)

# S3 method for class 'character'
isoyearweek_to_isoyearquarter_c(x)
```

## Arguments

- x:

  ISO yearweek as a character string of the form "yyyy-ww", e.g.
  "2020-19" for the 19th ISO week of 2020.

## Value

ISO yearquarter as a character vector (e.g. "2020-Q2").

## Details

The output keeps the year part of the input and appends the
ISO-week-based quarter (see
[`isoyearweek_to_isoquarter_c()`](https://niphr.github.io/cstime/reference/isoyearweek_to_isoquarter_c.md)),
for example "2020-Q2".

## Examples

``` r
isoyearweek_to_isoyearquarter_c("2020-19")
#> [1] "2020-Q2"
```
