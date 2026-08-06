# Changelog

## Version 2026.8.6

- Documentation only. No function changes behaviour.
- [`vignette("cstime")`](https://niphr.github.io/cstime/articles/cstime.md),
  which pkgdown promotes to “Get started”, gained an overview at the
  front. It states what the package is for, the
  `<from>_to_<to>_<suffix>` naming pattern that 32 of the 33 exports
  follow, three worked examples of what surprises people (an ISO year is
  not a calendar year, an ISO year has 52 or 53 weeks, a season week is
  not an ISO week), and where cstime sits relative to cstidy.
- The existing sections of
  [`vignette("cstime")`](https://niphr.github.io/cstime/articles/cstime.md)
  are unchanged.

## Version 2026.8.4

- Documentation only. No function changes behaviour.
- `README.md` gained an installation section, a quick start and a
  which-function-do-I-want table.
- Every exported function gained an `@seealso` that names the vignette
  covering it, or states plainly that no vignette runs it.
- Added four `@family` groups for the date-to-character, date-to-number,
  yearweek-to-character and yearweek-to-number converters.
- Corrected three documentation claims. `dates_by_isoyearweek` covers
  ISO years 1900 to 2099, not 1900 to 2100.
  [`date_to_isoquarter_n()`](https://niphr.github.io/cstime/reference/date_to_isoquarter_n.md)
  and
  [`isoyearweek_to_isoquarter_n()`](https://niphr.github.io/cstime/reference/isoyearweek_to_isoquarter_n.md)
  return a numeric vector, not an integer vector.
  `nor_workdays_by_isoyearweek` stores its proportions rounded to two
  decimal places.
- [`now_c()`](https://niphr.github.io/cstime/reference/now_c.md) now
  documents that its result depends on the session time zone.
- `index.md` and `pkgdown` are no longer shipped in the source tarball.

## Version 2025.10.13

CRAN release: 2025-10-14

- Seasons now start on week 35.

## Version 2024.5.13

- Adding season_to_last_date

## Version 2023.12.28

- Adding support for isoquarter and isoyearquarter

## Version 2023.5.3

CRAN release: 2023-05-15

- Inclusion of `now_c` that gives the current time in character format.

## Version 2023.4.26

- Uses binary searches to improve speed on date conversions.

## Version 2023.4.25

- Implements `csutils::apply_fn_via_hash_table` to speed up the
  conversion functions.
