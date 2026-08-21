# Changelog

## Version 2026.8.21

- The package drops `magrittr`. Every `%>%` is now the base pipe `|>`,
  and `magrittr` is gone from `DESCRIPTION`.
- The rewrite is a relocation, not an edit. Each `%>%` call was
  transformed the way R’s parser transforms `|>`, and the resulting tree
  was required to match the tree parsed from the rewritten file. A file
  whose trees disagreed was left untouched and converted by hand
  instead.
- [`utils::globalVariables()`](https://rdrr.io/r/utils/globalVariables.html)
  no longer declares `%>%` or `:=`. Both are imported, so neither entry
  ever suppressed anything. Only `.` is declared now.

## Version 2026.8.20

- `R/.DS_Store` is removed. A macOS Finder artefact was committed inside
  `R/`, so it shipped in the tarball. `.gitignore` now covers it.
- The version is bumped because the tarball changed. `2026.8.6` was
  already published from an earlier tree, and one version number must
  not name two.

## Version 2026.8.6

### Licensing

- The copyright holder is now **Folkehelseinstituttet**. It read “Core
  Surveillance”, which names the package family rather than a legal
  entity.

- `DESCRIPTION` `Authors@R` now declares that holder with
  `role = "cph"`. It declared no copyright holder at all, and neither
  did any other package in the fleet. Nothing in `R CMD check` reports
  that.

- The copyright year is now 2026. It read 2023.

- `CLAUDE.md` now carries a Licensing section, so the year gets checked
  rather than silently ageing.

- Documentation only. No function changes behaviour.

- [`vignette("cstime")`](https://niphr.github.io/cstime/articles/cstime.md),
  which pkgdown promotes to “Get started”, gained an overview at the
  front. The overview states what the package is for and describes the
  `<from>_to_<to>_<suffix>` naming pattern that 32 of the 33 exports
  follow. It then works three examples of what surprises people:

  - An ISO year is not a calendar year.
  - An ISO year has 52 or 53 weeks.
  - A season week is not an ISO week.

  The overview closes with where cstime sits relative to cstidy.

- The existing sections of
  [`vignette("cstime")`](https://niphr.github.io/cstime/articles/cstime.md)
  are unchanged.

- Removed a false sentence from
  [`vignette("cstime")`](https://niphr.github.io/cstime/articles/cstime.md).
  It claimed that cstime provides functions to generate rolling date
  ranges for weekly, bi-weekly and 4-week averages. No such function
  exists. None of the 33 exports returns a range, and `R/`, `man/` and
  `NAMESPACE` hold zero matches for “rolling”, “bi-week”, “4-week” or
  “average”.

- Rewrote the roxygen prose, the three vignettes, `README.md` and
  `index.md` to the house technical-prose standard (ASD-STE100,
  Simplified Technical English). Sentences over 25 words are now zero in
  `R/`, zero in the vignette body text, zero in `README.md` and zero in
  `index.md`. No claim changed apart from the removal described above.

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
