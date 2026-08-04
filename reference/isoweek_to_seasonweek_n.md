# ISO week to season week (numeric)

Maps an ISO week number to its position within the surveillance season,
where season week 1 corresponds to ISO week 35.

## Usage

``` r
isoweek_to_seasonweek_n(x)

# Default S3 method
isoweek_to_seasonweek_n(x)

# S3 method for class 'character'
isoweek_to_seasonweek_n(x)

# S3 method for class 'numeric'
isoweek_to_seasonweek_n(x)
```

## Arguments

- x:

  ISO week as a number between 1 and 53.

## Value

Season week as a numeric vector (ISO week 53 returns 18.5).

## Details

Surveillance seasons start at ISO week 35, so ISO week 35 is season week
1, ISO week 36 is season week 2, and so on, wrapping around the new
year. ISO week 53 (which only occurs in long ISO years) maps to the
half-step season week 18.5 so that the surrounding weeks keep consistent
numbering.

## See also

[`seasonweek_to_isoweek_n()`](https://niphr.github.io/cstime/reference/seasonweek_to_isoweek_n.md)
and
[`seasonweek_to_isoweek_c()`](https://niphr.github.io/cstime/reference/seasonweek_to_isoweek_c.md)
convert back. They return different classes from each other, so they are
not a family.
[`vignette("cstime", package = "cstime")`](https://niphr.github.io/cstime/articles/cstime.md)
and
[`vignette("season", package = "cstime")`](https://niphr.github.io/cstime/articles/season.md),
which both run this function.

## Examples

``` r
isoweek_to_seasonweek_n(35)
#> [1] 1
isoweek_to_seasonweek_n(c(31, 53))
#> [1] 49.0 18.5
```
