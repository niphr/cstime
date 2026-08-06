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

ISO quarter as a numeric vector (1 to 4).

## Details

The quarter comes from the ISO week part of the input:

- Weeks 1 to 13 are quarter 1.

- Weeks 14 to 26 are quarter 2.

- Weeks 27 to 39 are quarter 3.

- Weeks 40 and later are quarter 4. This includes week 53.

## See also

[`vignette("date_conversion", package = "cstime")`](https://niphr.github.io/cstime/articles/date_conversion.md)
for worked date, ISO year and ISO week conversions. No vignette runs
this function.

Other ISO yearweek-to-number converters:
[`isoyearweek_to_isoweek_n()`](https://niphr.github.io/cstime/reference/isoyearweek_to_isoweek_n.md),
[`isoyearweek_to_isoyear_n()`](https://niphr.github.io/cstime/reference/isoyearweek_to_isoyear_n.md),
[`isoyearweek_to_seasonweek_n()`](https://niphr.github.io/cstime/reference/isoyearweek_to_seasonweek_n.md)

## Examples

``` r
isoyearweek_to_isoquarter_n("2020-19")
#> [1] 2
```
