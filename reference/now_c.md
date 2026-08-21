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

The result depends on the time zone of the session. At one instant, a
session in Pacific/Auckland and a session in America/Los_Angeles can
report different calendar dates, not only different clock times.

## See also

[`date_to_isoyearweek_c()`](https://niphr.github.io/cstime/reference/date_to_isoyearweek_c.md)
to convert a date rather than to read the clock. No vignette runs this
function.

## Examples

``` r
now_c()
#> [1] "2026-08-21 04:58:05"
now_c(format = "%Y-%m-%d")
#> [1] "2026-08-21"
```
