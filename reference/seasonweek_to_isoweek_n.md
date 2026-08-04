# Season week to ISO week (numeric)

Maps a season week number back to its ISO week, returned as a number.
This is the inverse of
[`isoweek_to_seasonweek_n()`](https://niphr.github.io/cstime/reference/isoweek_to_seasonweek_n.md).

## Usage

``` r
seasonweek_to_isoweek_n(x)

# Default S3 method
seasonweek_to_isoweek_n(x)

# S3 method for class 'numeric'
seasonweek_to_isoweek_n(x)
```

## Arguments

- x:

  Season week as a number between 1 and 52.

## Value

ISO week as an integer vector (1 to 53).

## Details

Season week 1 corresponds to ISO week 35, season week 2 to ISO week 36,
and so on, wrapping around the new year.

## See also

[`seasonweek_to_isoweek_c()`](https://niphr.github.io/cstime/reference/seasonweek_to_isoweek_c.md)
returns the same week as a zero-padded string, and
[`isoweek_to_seasonweek_n()`](https://niphr.github.io/cstime/reference/isoweek_to_seasonweek_n.md)
converts back.
[`vignette("cstime", package = "cstime")`](https://niphr.github.io/cstime/articles/cstime.md)
and
[`vignette("season", package = "cstime")`](https://niphr.github.io/cstime/articles/season.md),
which both run this function.

## Examples

``` r
seasonweek_to_isoweek_n(1)
#> [1] 35
seasonweek_to_isoweek_n(c(31, 52))
#> [1] 13 34
```
