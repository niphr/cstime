# Cross-cutting INPUT CONTRACT for the conversion functions.
#
# These functions are not computations. They are keyed lookups into the
# precomputed data.tables in R/sysdata.rda (conversions_date_to,
# conversions_date_c_to, conversions_isoyearweek_to, conversions_isoyear_n_to,
# conversions_isoyear_c_to), dispatched by S3 with a .default method that
# returns NA. Every expectation below is the value the shipped table returns,
# not a value computed independently.
#
# Three properties are pinned:
#   P1 NA-CONTRACT      out-of-range date, wrong type (hits .default) and NA
#                       all return NA, and raise neither error nor warning.
#   P2 VECTORISATION    a length-n input returns a length-n result, elementwise
#                       correct, including when some elements are unmatched.
#   P3 METHOD AGREEMENT the Date method and the character method return the same
#                       value for the same date.
#
# THREE VALUES BELOW LOOK LIKE BUGS AND ARE NOT. They are pinned deliberately:
#   1. date_to_seasonweek_n("2021-01-01") is 18.5, not an integer. ISO week 53
#      is a half-step season week.
#   2. date_to_isoquarter_n("2021-01-01") is 4. Calendar January, but ISO year
#      2020 week 53, and week 53 is quarter 4.
#   3. Invalid input returns NA rather than erroring. Out-of-range dates fall
#      outside the 1900-01-01..9999-09-09 lookup table; wrong types hit the
#      .default method. Do not add validation, stop() or warnings.
#
# tests/testthat.R attaches cstime for R CMD check. Running this file on its own
# with testthat::test_file() does not, so load the source tree in that case.
if (!isNamespaceLoaded("cstime")) {
  suppressPackageStartupMessages(pkgload::load_all(quiet = TRUE))
}

# 2019-12-30 and 2024-12-30 are late December belonging to the NEXT ISO year.
# 2021-01-01 is early January belonging to the PREVIOUS ISO year.
# 2021-01-04 is an ordinary date where ISO year and calendar year agree.
# 2020 is a 53-week ISO year.
d_c <- c("2019-12-30", "2020-12-31", "2021-01-01", "2021-01-04", "2024-12-30")
d_d <- as.Date(d_c)

# 1800-01-01 is BEFORE the first row of the lookup table (1900-01-01), so it is
# a valid date that the table cannot resolve.
oob_c <- "1800-01-01"
oob_d <- as.Date("1800-01-01")

# P1 -- NA-CONTRACT ====

test_that("P1 NA-CONTRACT: ISO date conversions return NA for out-of-range, wrong-type and NA input", {
  # Out of range: valid date, no row in the table. Hits the real method.
  expect_equal(date_to_isoyear_n(oob_c), NA_integer_)
  expect_equal(date_to_isoyear_n(oob_d), NA_integer_)
  expect_equal(date_to_isoyearweek_c(oob_c), NA_character_)
  expect_equal(date_to_isoyearweek_c(oob_d), NA_character_)
  expect_equal(date_to_isoquarter_n(oob_c), NA_integer_)

  # Wrong type: numeric is neither Date nor character, so this reaches the
  # .default method. This is the assertion that proves .default is live.
  expect_equal(date_to_isoyear_n(42), NA_integer_)
  expect_equal(date_to_isoyearweek_c(42), NA_character_)
  expect_equal(date_to_isoquarter_n(42), NA_integer_)

  # NA is logical, so it also reaches .default.
  expect_equal(date_to_isoyear_n(NA), NA_integer_)
  expect_equal(date_to_isoyearweek_c(NA), NA_character_)
  expect_equal(date_to_isoquarter_n(NA), NA_integer_)

  # None of this errors and none of it warns.
  expect_no_error(date_to_isoyear_n(oob_c))
  expect_no_warning(date_to_isoyear_n(oob_c))
  expect_no_error(date_to_isoyear_n(42))
  expect_no_warning(date_to_isoyear_n(42))
  expect_no_error(date_to_isoyearweek_c(NA))
  expect_no_warning(date_to_isoyearweek_c(NA))
})

test_that("P1 NA-CONTRACT: calendar and season date conversions return NA for out-of-range, wrong-type and NA input", {
  expect_equal(date_to_calyear_n(oob_c), NA_integer_)
  expect_equal(date_to_calyear_n(oob_d), NA_integer_)
  expect_equal(date_to_calmonth_n(oob_c), NA_integer_)
  expect_equal(date_to_calyearmonth_c(oob_c), NA_character_)
  expect_equal(date_to_season_c(oob_c), NA_character_)
  expect_equal(date_to_seasonweek_n(oob_c), NA_real_)

  expect_equal(date_to_calyear_n(42), NA_integer_)
  expect_equal(date_to_calmonth_n(42), NA_integer_)
  expect_equal(date_to_calyearmonth_c(42), NA_character_)
  expect_equal(date_to_season_c(42), NA_character_)
  expect_equal(date_to_seasonweek_n(42), NA_real_)

  expect_equal(date_to_calyear_n(NA), NA_integer_)
  expect_equal(date_to_calmonth_n(NA), NA_integer_)
  expect_equal(date_to_calyearmonth_c(NA), NA_character_)
  expect_equal(date_to_season_c(NA), NA_character_)
  expect_equal(date_to_seasonweek_n(NA), NA_real_)

  expect_no_error(date_to_calyear_n(42))
  expect_no_warning(date_to_calyear_n(42))
  expect_no_error(date_to_season_c(oob_c))
  expect_no_warning(date_to_season_c(oob_c))
})

test_that("P1 NA-CONTRACT: isoyearweek and isoyear conversions return NA for out-of-range, wrong-type and NA input", {
  # "1800-01" is well formed but has no row in conversions_isoyearweek_to.
  expect_equal(isoyearweek_to_last_date("1800-01"), as.Date(NA))
  expect_equal(isoyearweek_to_isoyear_n("1800-01"), NA_integer_)
  # 1800 is well formed but has no row in conversions_isoyear_n_to.
  expect_equal(isoyear_to_last_isoweek_n(1800), NA_integer_)
  expect_equal(isoyear_to_last_isoweek_n("1800"), NA_integer_)

  # Wrong type. isoyearweek_to_* accepts character only, so a number hits
  # .default. isoyear_to_* accepts numeric and character, so a logical does.
  expect_equal(isoyearweek_to_last_date(42), as.Date(NA))
  expect_equal(isoyearweek_to_isoyear_n(42), NA_integer_)
  expect_equal(isoyear_to_last_isoweek_n(TRUE), NA_integer_)

  expect_equal(isoyearweek_to_last_date(NA), as.Date(NA))
  expect_equal(isoyearweek_to_isoyear_n(NA), NA_integer_)
  expect_equal(isoyear_to_last_isoweek_n(NA), NA_integer_)

  expect_no_error(isoyearweek_to_last_date(42))
  expect_no_warning(isoyearweek_to_last_date(42))
  expect_no_error(isoyear_to_last_isoweek_n(TRUE))
  expect_no_warning(isoyear_to_last_isoweek_n(TRUE))
})

# P2 -- VECTORISATION ====

test_that("P2 VECTORISATION: ISO date conversions map a length-5 input elementwise", {
  expect_length(date_to_isoyear_n(d_c), 5L)
  expect_equal(date_to_isoyear_n(d_c), c(2020L, 2020L, 2020L, 2021L, 2025L))
  expect_length(date_to_isoyear_n(d_d), 5L)
  expect_equal(date_to_isoyear_n(d_d), c(2020L, 2020L, 2020L, 2021L, 2025L))

  expect_length(date_to_isoweek_n(d_c), 5L)
  expect_equal(date_to_isoweek_n(d_c), c(1L, 53L, 53L, 1L, 1L))

  expect_length(date_to_isoyearweek_c(d_c), 5L)
  expect_equal(
    date_to_isoyearweek_c(d_c),
    c("2020-01", "2020-53", "2020-53", "2021-01", "2025-01")
  )

  # PINNED: 2021-01-01 is ISO week 53, which is quarter 4, not quarter 1.
  expect_length(date_to_isoquarter_n(d_c), 5L)
  expect_equal(date_to_isoquarter_n(d_c), c(1, 4, 4, 1, 1))

  # Each element is resolved independently: an unmatched element becomes NA and
  # its neighbours are unaffected.
  expect_equal(
    date_to_isoyear_n(c("2021-01-01", oob_c, "2021-01-04")),
    c(2020L, NA_integer_, 2021L)
  )
  # Length is preserved even when nothing matches.
  expect_length(date_to_isoyear_n(c(oob_c, oob_c)), 2L)
  expect_length(date_to_isoyear_c(c(42, 43)), 2L)
})

test_that("P2 VECTORISATION: calendar and season date conversions map a length-5 input elementwise", {
  expect_length(date_to_calyear_n(d_c), 5L)
  expect_equal(date_to_calyear_n(d_c), c(2019L, 2020L, 2021L, 2021L, 2024L))

  expect_length(date_to_calmonth_n(d_c), 5L)
  expect_equal(date_to_calmonth_n(d_c), c(12L, 12L, 1L, 1L, 12L))

  expect_length(date_to_calyearmonth_c(d_c), 5L)
  expect_equal(
    date_to_calyearmonth_c(d_c),
    c("2019-M12", "2020-M12", "2021-M01", "2021-M01", "2024-M12")
  )

  expect_length(date_to_season_c(d_c), 5L)
  expect_equal(
    date_to_season_c(d_c),
    c("2019/2020", "2020/2021", "2020/2021", "2020/2021", "2024/2025")
  )

  # PINNED: ISO week 53 maps to the half-step season week 18.5, not an integer.
  expect_length(date_to_seasonweek_n(d_c), 5L)
  expect_equal(date_to_seasonweek_n(d_c), c(19, 18.5, 18.5, 19, 19))

  expect_equal(
    date_to_calyear_n(c("2021-01-01", oob_c, "2021-01-04")),
    c(2021L, NA_integer_, 2021L)
  )
  expect_equal(
    date_to_seasonweek_n(c("2021-01-01", oob_c, "2021-01-04")),
    c(18.5, NA_real_, 19)
  )
})

test_that("P2 VECTORISATION: isoyearweek and isoyear conversions map a length-n input elementwise", {
  expect_length(isoyear_to_last_isoweek_n(c(2019, 2020, 2021, 2024)), 4L)
  expect_equal(
    isoyear_to_last_isoweek_n(c(2019, 2020, 2021, 2024)),
    c(52L, 53L, 52L, 52L)
  )
  expect_equal(
    isoyear_to_last_isoweek_n(c("2019", "2020", "2021", "2024")),
    c(52L, 53L, 52L, 52L)
  )

  expect_length(isoyearweek_to_last_date(c("2020-53", "2021-01")), 2L)
  expect_equal(
    isoyearweek_to_last_date(c("2020-53", "2021-01")),
    as.Date(c("2021-01-03", "2021-01-10"))
  )

  expect_equal(
    isoyear_to_last_isoweek_n(c(2020, 1800, 2021)),
    c(53L, NA_integer_, 52L)
  )
  expect_equal(
    isoyearweek_to_last_date(c("2020-53", "1800-01", "2021-01")),
    as.Date(c("2021-01-03", NA, "2021-01-10"))
  )
})

# P3 -- METHOD AGREEMENT ====

test_that("P3 METHOD AGREEMENT: ISO date conversions give the same answer for Date and character input", {
  expect_equal(date_to_isoyear_n(d_d), date_to_isoyear_n(d_c))
  expect_equal(date_to_isoyear_c(d_d), date_to_isoyear_c(d_c))
  expect_equal(date_to_isoweek_n(d_d), date_to_isoweek_n(d_c))
  expect_equal(date_to_isoyearweek_c(d_d), date_to_isoyearweek_c(d_c))
  expect_equal(date_to_isoquarter_n(d_d), date_to_isoquarter_n(d_c))

  # Stated as absolute values too, so a mutation that changes BOTH methods in
  # the same way cannot keep this test green.
  expect_equal(
    date_to_isoyearweek_c(as.Date("2021-01-01")),
    date_to_isoyearweek_c("2021-01-01")
  )
  expect_equal(date_to_isoyearweek_c(as.Date("2021-01-01")), "2020-53")
  expect_equal(date_to_isoyear_n(as.Date("2021-01-01")), 2020L)
  expect_equal(date_to_isoyear_n("2021-01-01"), 2020L)

  # Agreement holds for unmatched input as well: both sides are NA.
  expect_equal(date_to_isoyear_n(oob_d), date_to_isoyear_n(oob_c))
})

test_that("P3 METHOD AGREEMENT: calendar and season date conversions give the same answer for Date and character input", {
  expect_equal(date_to_calyear_n(d_d), date_to_calyear_n(d_c))
  expect_equal(date_to_calmonth_n(d_d), date_to_calmonth_n(d_c))
  expect_equal(date_to_calyearmonth_c(d_d), date_to_calyearmonth_c(d_c))
  expect_equal(date_to_season_c(d_d), date_to_season_c(d_c))
  expect_equal(date_to_seasonweek_n(d_d), date_to_seasonweek_n(d_c))

  expect_equal(
    date_to_calyearmonth_c(as.Date("2021-01-01")),
    date_to_calyearmonth_c("2021-01-01")
  )
  expect_equal(date_to_calyearmonth_c(as.Date("2021-01-01")), "2021-M01")
  expect_equal(date_to_calmonth_n(as.Date("2021-01-01")), 1L)
  expect_equal(date_to_season_c(as.Date("2021-01-01")), "2020/2021")
  expect_equal(date_to_seasonweek_n(as.Date("2021-01-01")), 18.5)
})

test_that("P3 METHOD AGREEMENT: isoyear conversions give the same answer for numeric and character input", {
  expect_equal(
    isoyear_to_last_isoweek_n(c(2019, 2020, 2021, 2024)),
    isoyear_to_last_isoweek_n(c("2019", "2020", "2021", "2024"))
  )
  expect_equal(isoyear_to_last_isoweek_n(2020), 53L)
  expect_equal(isoyear_to_last_isoweek_n("2020"), 53L)
  expect_equal(isoyear_to_last_isoyearweek_c(2020), "2020-53")
  expect_equal(isoyear_to_last_isoyearweek_c("2020"), "2020-53")
  expect_equal(isoyear_to_last_date(2020), isoyear_to_last_date("2020"))
})
