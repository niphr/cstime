# Date to ISO yearquarter (character)

Converts a date to a combined ISO year and quarter string of the form
"yyyy-Qn".

## Usage

``` r
date_to_isoyearquarter_c(x = lubridate::today())

# Default S3 method
date_to_isoyearquarter_c(x)

# S3 method for class 'character'
date_to_isoyearquarter_c(x = lubridate::today())

# S3 method for class 'Date'
date_to_isoyearquarter_c(x = lubridate::today())
```

## Arguments

- x:

  A Date object, or a character string in the format 'yyyy-mm-dd'.

## Value

ISO yearquarter as a character vector (e.g. "2021-Q3").

## Details

The output combines the ISO year (see
[`date_to_isoyear_c()`](https://niphr.github.io/cstime/reference/date_to_isoyear_c.md))
and the ISO-week-based quarter (see
[`date_to_isoquarter_c()`](https://niphr.github.io/cstime/reference/date_to_isoquarter_c.md)),
for example "2021-Q3".

## Examples

``` r
date_to_isoyearquarter_c(as.Date("2021-08-11"))
#> [1] "2021-Q3"
date_to_isoyearquarter_c("2021-01-01")
#> [1] "2020-Q4"
```
