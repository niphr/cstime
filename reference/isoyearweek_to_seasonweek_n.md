# ISO yearweek to season week (numeric)

Maps an ISO yearweek to its position within the surveillance season,
where season week 1 corresponds to ISO week 35.

## Usage

``` r
isoyearweek_to_seasonweek_n(x)
```

## Arguments

- x:

  ISO yearweek as a character string of the form "yyyy-ww", e.g.
  "2021-01".

## Value

Season week as a numeric vector.

## Details

This function takes the ISO week from the yearweek string and converts
it with
[`isoweek_to_seasonweek_n()`](https://niphr.github.io/cstime/reference/isoweek_to_seasonweek_n.md).
The same season-week numbering applies, including the 18.5 half-step for
ISO week 53.

## See also

[`vignette("season", package = "cstime")`](https://niphr.github.io/cstime/articles/season.md)
for worked season week conversions. No vignette runs this function.

Other ISO yearweek-to-number converters:
[`isoyearweek_to_isoquarter_n()`](https://niphr.github.io/cstime/reference/isoyearweek_to_isoquarter_n.md),
[`isoyearweek_to_isoweek_n()`](https://niphr.github.io/cstime/reference/isoyearweek_to_isoweek_n.md),
[`isoyearweek_to_isoyear_n()`](https://niphr.github.io/cstime/reference/isoyearweek_to_isoyear_n.md)

## Examples

``` r
isoyearweek_to_seasonweek_n(c("2021-01", "2021-35"))
#> [1] 19  1
```
