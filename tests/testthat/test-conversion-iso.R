# These are keyed lookups into the precomputed tables in R/sysdata.rda, not
# computations. Every expectation below is the value the shipped table returns.
#
# tests/testthat.R attaches cstime for R CMD check. Running this file on its own
# with testthat::test_file() does not, so load the source tree in that case.
if (!isNamespaceLoaded("cstime")) {
  suppressPackageStartupMessages(pkgload::load_all(quiet = TRUE))
}

# 2019-12-30 and 2024-12-30 are late December belonging to the NEXT ISO year.
# 2021-01-01 is early January belonging to the PREVIOUS ISO year.
# 2020 is a 53-week ISO year.
d_c <- c("2019-12-30", "2020-12-31", "2021-01-01", "2021-01-04", "2024-12-30")
d_d <- as.Date(d_c)

# 2020-53 is the 53rd week of a long ISO year; 2019-19 is an ordinary midyear week.
yw <- c("2020-01", "2020-53", "2021-01", "2019-19", "2025-01")

iy_n <- c(2019, 2020, 2021, 2024)
iy_c <- c("2019", "2020", "2021", "2024")

# date -> isoyear, isoweek, isoyearweek ====

test_that("date_to_isoyear_c returns the table's ISO year across December/January boundaries", {
  expect_equal(
    date_to_isoyear_c(d_c),
    c("2020", "2020", "2020", "2021", "2025")
  )
  expect_equal(
    date_to_isoyear_c(d_d),
    c("2020", "2020", "2020", "2021", "2025")
  )
  # Invalid input returns NA, it does not error.
  expect_equal(date_to_isoyear_c(42), NA_character_)
  expect_equal(date_to_isoyear_c(NA), NA_character_)
})

test_that("date_to_isoyear_n returns the table's ISO year as a number", {
  expect_equal(date_to_isoyear_n(d_c), c(2020L, 2020L, 2020L, 2021L, 2025L))
  expect_equal(date_to_isoyear_n(d_d), c(2020L, 2020L, 2020L, 2021L, 2025L))
  expect_equal(
    date_to_isoyear_n(c("2021-01-01", "2021-01-04")),
    c(2020L, 2021L)
  )
  # Outside the 1900-01-01..9999-09-09 table, so NA rather than an error.
  expect_equal(date_to_isoyear_n("1800-01-01"), NA_integer_)
})

test_that("date_to_isoweek_c returns the zero-padded ISO week, including week 53", {
  expect_equal(date_to_isoweek_c(d_c), c("01", "53", "53", "01", "01"))
  expect_equal(date_to_isoweek_c(d_d), c("01", "53", "53", "01", "01"))
  expect_equal(date_to_isoweek_c(42), NA_character_)
})

test_that("date_to_isoweek_n returns the ISO week as a number, including week 53", {
  expect_equal(date_to_isoweek_n(d_c), c(1L, 53L, 53L, 1L, 1L))
  expect_equal(date_to_isoweek_n(d_d), c(1L, 53L, 53L, 1L, 1L))
  expect_equal(date_to_isoweek_n(42), NA_integer_)
})

test_that("date_to_isoyearweek_c joins ISO year and week, so 2021-01-01 is 2020-53", {
  expect_equal(
    date_to_isoyearweek_c(d_c),
    c("2020-01", "2020-53", "2020-53", "2021-01", "2025-01")
  )
  expect_equal(
    date_to_isoyearweek_c(d_d),
    c("2020-01", "2020-53", "2020-53", "2021-01", "2025-01")
  )
  expect_equal(date_to_isoyearweek_c(42), NA_character_)
})

# date -> isoquarter, isoyearquarter ====

test_that("date_to_isoquarter_c returns the ISO-week-based quarter as a string", {
  expect_equal(date_to_isoquarter_c(d_c), c("1", "4", "4", "1", "1"))
  expect_equal(date_to_isoquarter_c(d_d), c("1", "4", "4", "1", "1"))
  expect_equal(date_to_isoquarter_c(42), NA_character_)
})

test_that("date_to_isoquarter_n puts calendar January 2021-01-01 in quarter 4", {
  # Not a bug: 2021-01-01 is ISO 2020 week 53, and week 53 is quarter 4.
  expect_equal(date_to_isoquarter_n("2021-01-01"), 4)
  expect_equal(date_to_isoquarter_n(d_c), c(1, 4, 4, 1, 1))
  expect_equal(date_to_isoquarter_n(d_d), c(1, 4, 4, 1, 1))
  expect_equal(date_to_isoquarter_n(42), NA_integer_)
})

test_that("date_to_isoyearquarter_c joins ISO year and ISO quarter", {
  expect_equal(
    date_to_isoyearquarter_c(d_c),
    c("2020-Q1", "2020-Q4", "2020-Q4", "2021-Q1", "2025-Q1")
  )
  expect_equal(
    date_to_isoyearquarter_c(d_d),
    c("2020-Q1", "2020-Q4", "2020-Q4", "2021-Q1", "2025-Q1")
  )
  expect_equal(date_to_isoyearquarter_c(42), NA_character_)
})

# isoyearweek -> isoyear, isoweek ====

test_that("isoyearweek_to_isoyear_c takes the year part of the yearweek", {
  expect_equal(
    isoyearweek_to_isoyear_c(yw),
    c("2020", "2020", "2021", "2019", "2025")
  )
  expect_equal(isoyearweek_to_isoyear_c(42), NA_character_)
})

test_that("isoyearweek_to_isoyear_n takes the year part of the yearweek as a number", {
  expect_equal(
    isoyearweek_to_isoyear_n(yw),
    c(2020L, 2020L, 2021L, 2019L, 2025L)
  )
  expect_equal(isoyearweek_to_isoyear_n(42), NA_integer_)
})

test_that("isoyearweek_to_isoweek_c takes the zero-padded week part of the yearweek", {
  expect_equal(isoyearweek_to_isoweek_c(yw), c("01", "53", "01", "19", "01"))
  expect_equal(isoyearweek_to_isoweek_c(42), NA_character_)
})

test_that("isoyearweek_to_isoweek_n takes the week part of the yearweek as a number", {
  expect_equal(isoyearweek_to_isoweek_n(yw), c(1L, 53L, 1L, 19L, 1L))
  expect_equal(isoyearweek_to_isoweek_n(42), NA_integer_)
})

# isoyearweek -> isoquarter, isoyearquarter ====

test_that("isoyearweek_to_isoquarter_c maps week 53 to quarter 4", {
  expect_equal(isoyearweek_to_isoquarter_c(yw), c("1", "4", "1", "2", "1"))
  expect_equal(isoyearweek_to_isoquarter_c(42), NA_character_)
})

test_that("isoyearweek_to_isoquarter_n maps week 53 to quarter 4 as a number", {
  expect_equal(isoyearweek_to_isoquarter_n(yw), c(1, 4, 1, 2, 1))
  expect_equal(isoyearweek_to_isoquarter_n(42), NA_integer_)
})

test_that("isoyearweek_to_isoyearquarter_c keeps the ISO year and appends the quarter", {
  expect_equal(
    isoyearweek_to_isoyearquarter_c(yw),
    c("2020-Q1", "2020-Q4", "2021-Q1", "2019-Q2", "2025-Q1")
  )
  expect_equal(isoyearweek_to_isoyearquarter_c(42), NA_character_)
})

# downsizing: isoyearweek/isoyear -> date ====

test_that("isoyearweek_to_last_date returns the Sunday ending the week", {
  expect_equal(
    isoyearweek_to_last_date(yw),
    as.Date(c(
      "2020-01-05",
      "2021-01-03",
      "2021-01-10",
      "2019-05-12",
      "2025-01-05"
    ))
  )
  # The 53rd week of ISO 2020 ends in calendar 2021.
  expect_equal(isoyearweek_to_last_date("2020-53"), as.Date("2021-01-03"))
  expect_equal(isoyearweek_to_last_date("2021-01"), as.Date("2021-01-10"))
  expect_equal(isoyearweek_to_last_date(42), as.Date(NA))
})

test_that("isoyear_to_last_date returns the Sunday ending the last ISO week", {
  expect_equal(
    isoyear_to_last_date(iy_n),
    as.Date(c("2019-12-29", "2021-01-03", "2022-01-02", "2024-12-29"))
  )
  expect_equal(
    isoyear_to_last_date(iy_c),
    as.Date(c("2019-12-29", "2021-01-03", "2022-01-02", "2024-12-29"))
  )
  expect_equal(isoyear_to_last_date(TRUE), as.Date(NA))
})

test_that("isoyear_to_last_isoweek_n is 53 for 2020 and 52 for 2019, 2021 and 2024", {
  expect_equal(isoyear_to_last_isoweek_n(iy_n), c(52L, 53L, 52L, 52L))
  expect_equal(isoyear_to_last_isoweek_n(iy_c), c(52L, 53L, 52L, 52L))
  expect_equal(isoyear_to_last_isoweek_n(TRUE), NA_integer_)
})

test_that("isoyear_to_last_isoyearweek_c is 2020-53 for the long ISO year", {
  expect_equal(
    isoyear_to_last_isoyearweek_c(iy_n),
    c("2019-52", "2020-53", "2021-52", "2024-52")
  )
  expect_equal(
    isoyear_to_last_isoyearweek_c(iy_c),
    c("2019-52", "2020-53", "2021-52", "2024-52")
  )
  expect_equal(isoyear_to_last_isoyearweek_c(TRUE), NA_character_)
})

# calendar and season values at the same boundaries, for contrast ====

test_that("date_to_calyear_n follows the calendar year, not the ISO year", {
  expect_equal(date_to_calyear_n(d_c), c(2019L, 2020L, 2021L, 2021L, 2024L))
})

test_that("date_to_calyearmonth_c returns 2021-M01 for 2021-01-01", {
  expect_equal(date_to_calyearmonth_c("2021-01-01"), "2021-M01")
})

test_that("date_to_calmonth_n returns 1 for 2021-01-01", {
  expect_equal(date_to_calmonth_n("2021-01-01"), 1L)
})

test_that("date_to_season_c returns 2020/2021 for 2021-01-01", {
  expect_equal(date_to_season_c("2021-01-01"), "2020/2021")
})

test_that("date_to_seasonweek_n returns the fractional 18.5 for 2021-01-01", {
  # Not a bug: season weeks are half-weeks in a 53-week ISO year, so this is 18.5.
  expect_equal(date_to_seasonweek_n("2021-01-01"), 18.5)
})
