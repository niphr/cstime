# Season week to ISO week (character)

Maps a season week number back to its ISO week, returned as a
zero-padded character string. This is the inverse of
[`isoweek_to_seasonweek_n()`](https://niphr.github.io/cstime/reference/isoweek_to_seasonweek_n.md).

## Usage

``` r
seasonweek_to_isoweek_c(x)

# Default S3 method
seasonweek_to_isoweek_c(x)

# S3 method for class 'numeric'
seasonweek_to_isoweek_c(x)
```

## Arguments

- x:

  Season week as a number between 1 and 52.

## Value

ISO week as a character vector (e.g. "35").

## Details

Season week 1 corresponds to ISO week 35, season week 2 to ISO week 36,
and so on, wrapping around the new year. The ISO week is returned as two
digits, e.g. "35" or "01".

## See also

[`seasonweek_to_isoweek_n()`](https://niphr.github.io/cstime/reference/seasonweek_to_isoweek_n.md)
returns the same week as a number, and
[`isoweek_to_seasonweek_n()`](https://niphr.github.io/cstime/reference/isoweek_to_seasonweek_n.md)
converts back.
[`vignette("season", package = "cstime")`](https://niphr.github.io/cstime/articles/season.md),
which runs this function.

## Examples

``` r
seasonweek_to_isoweek_c(1)
#> [1] "35"
seasonweek_to_isoweek_c(c(31, 52))
#> [1] "13" "34"
```
