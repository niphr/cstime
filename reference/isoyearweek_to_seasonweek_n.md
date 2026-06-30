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

The ISO week is extracted from the yearweek string and then converted
with
[`isoweek_to_seasonweek_n()`](https://niphr.github.io/cstime/reference/isoweek_to_seasonweek_n.md),
so the same season-week numbering and the 18.5 half-step for ISO week 53
apply.

## Examples

``` r
isoyearweek_to_seasonweek_n(c("2021-01", "2021-35"))
#> [1] 19  1
```
