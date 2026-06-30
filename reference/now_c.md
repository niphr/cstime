# Current time as character

Returns the current system time formatted as a character string.

## Usage

``` r
now_c(format = "%Y-%m-%d %H:%M:%S")
```

## Arguments

- format:

  A format string passed to
  [`base::format()`](https://rdrr.io/r/base/format.html). Defaults to
  "%Y-%m-%d %H:%M:%S".

## Value

The current time as a single character string.

## Details

The current time is taken from
[`base::Sys.time()`](https://rdrr.io/r/base/Sys.time.html) and formatted
with the supplied `format` string, which uses the conversion codes
documented in
[`base::strptime()`](https://rdrr.io/r/base/strptime.html).

## Examples

``` r
now_c()
#> [1] "2026-06-30 11:55:06"
now_c(format = "%Y-%m-%d")
#> [1] "2026-06-30"
```
