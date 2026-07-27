# Calendar and season conversions.
#
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
# 2020 is a 53-week ISO year, so 2020-12-31 and 2021-01-01 are both 2020-53.
d_c <- c("2019-12-30", "2020-12-31", "2021-01-01", "2021-01-04", "2024-12-30")
d_d <- as.Date(d_c)

# 2020-53 is the 53rd week of a long ISO year; 2019-19 is an ordinary midyear week.
yw <- c("2020-01", "2020-53", "2021-01", "2021-35", "2021-50", "2019-19")

# date -> calendar month / year / yearmonth ====

test_that("date_to_calmonth_c returns the table's zero-padded calendar month", {
  expect_equal(
    date_to_calmonth_c(d_c),
    c("12", "12", "01", "01", "12")
  )
  expect_equal(
    date_to_calmonth_c(d_d),
    c("12", "12", "01", "01", "12")
  )
  expect_type(date_to_calmonth_c(d_c), "character")
  # 2020-12-31 and 2021-01-01 are the same ISO week (2020-53) but different
  # calendar months: the calendar month comes from the date, not the ISO week.
  expect_equal(date_to_calmonth_c("2020-12-31"), "12")
  expect_equal(date_to_calmonth_c("2021-01-01"), "01")
  # invalid input returns NA via the .default method; it does not error
  expect_equal(date_to_calmonth_c(42), NA_character_)
})

test_that("date_to_calmonth_n returns the table's calendar month as an integer", {
  expect_equal(
    date_to_calmonth_n(d_c),
    c(12L, 12L, 1L, 1L, 12L)
  )
  expect_equal(
    date_to_calmonth_n(d_d),
    c(12L, 12L, 1L, 1L, 12L)
  )
  expect_type(date_to_calmonth_n(d_c), "integer")
  expect_equal(date_to_calmonth_n("2021-01-01"), 1L)
  expect_equal(date_to_calmonth_n(42), NA_integer_)
})

test_that("date_to_calyear_c returns the calendar year, not the ISO year", {
  expect_equal(
    date_to_calyear_c(d_c),
    c("2019", "2020", "2021", "2021", "2024")
  )
  expect_equal(
    date_to_calyear_c(d_d),
    c("2019", "2020", "2021", "2021", "2024")
  )
  expect_type(date_to_calyear_c(d_c), "character")
  # 2019-12-30 is ISO year 2020 but calendar year 2019.
  expect_equal(date_to_calyear_c("2019-12-30"), "2019")
  # 2021-01-01 is ISO year 2020 but calendar year 2021.
  expect_equal(date_to_calyear_c("2021-01-01"), "2021")
  expect_equal(date_to_calyear_c(42), NA_character_)
})

test_that("date_to_calyear_n returns the calendar year as an integer", {
  expect_equal(
    date_to_calyear_n(d_c),
    c(2019L, 2020L, 2021L, 2021L, 2024L)
  )
  expect_equal(
    date_to_calyear_n(d_d),
    c(2019L, 2020L, 2021L, 2021L, 2024L)
  )
  expect_type(date_to_calyear_n(d_c), "integer")
  expect_equal(date_to_calyear_n("2024-12-30"), 2024L)
  expect_equal(date_to_calyear_n(42), NA_integer_)
})

test_that("date_to_calyearmonth_c joins calendar year and month with -M", {
  expect_equal(
    date_to_calyearmonth_c(d_c),
    c("2019-M12", "2020-M12", "2021-M01", "2021-M01", "2024-M12")
  )
  expect_equal(
    date_to_calyearmonth_c(d_d),
    c("2019-M12", "2020-M12", "2021-M01", "2021-M01", "2024-M12")
  )
  expect_type(date_to_calyearmonth_c(d_c), "character")
  expect_equal(date_to_calyearmonth_c("2021-01-01"), "2021-M01")
  expect_equal(date_to_calyearmonth_c(42), NA_character_)
})

# date -> season ====

test_that("date_to_season_c maps a date to its yyyy/yyyy surveillance season", {
  expect_equal(
    date_to_season_c(d_c),
    c("2019/2020", "2020/2021", "2020/2021", "2020/2021", "2024/2025")
  )
  expect_equal(
    date_to_season_c(d_d),
    c("2019/2020", "2020/2021", "2020/2021", "2020/2021", "2024/2025")
  )
  expect_type(date_to_season_c(d_c), "character")
  # January dates belong to the season that started the previous calendar year.
  expect_equal(date_to_season_c("2021-01-01"), "2020/2021")
})

test_that("date_to_seasonweek_n returns 18.5 for ISO week 53, not an integer", {
  # PINNED ODDITY 1: this is 18.5, not an integer. ISO week 53 is the half-step
  # season week so the surrounding weeks keep consistent numbering. Do not fix.
  expect_equal(date_to_seasonweek_n("2021-01-01"), 18.5)
  expect_equal(date_to_seasonweek_n(as.Date("2021-01-01")), 18.5)
  expect_false(date_to_seasonweek_n("2021-01-01") == 18)
  expect_false(date_to_seasonweek_n("2021-01-01") == 19)
  expect_equal(
    date_to_seasonweek_n(d_c),
    c(19, 18.5, 18.5, 19, 19)
  )
  expect_equal(
    date_to_seasonweek_n(d_d),
    c(19, 18.5, 18.5, 19, 19)
  )
  expect_type(date_to_seasonweek_n(d_c), "double")
})

# isoweek / isoyearweek -> season ====

test_that("isoweek_to_seasonweek_n starts the season at ISO week 35 and maps week 53 to 18.5", {
  expect_equal(
    isoweek_to_seasonweek_n(c(1, 34, 35, 36, 52, 53)),
    c(19, 52, 1, 2, 18, 18.5)
  )
  expect_equal(
    isoweek_to_seasonweek_n(c("01", "34", "35", "36", "52", "53")),
    c(19, 52, 1, 2, 18, 18.5)
  )
  expect_type(isoweek_to_seasonweek_n(35), "double")
  expect_equal(isoweek_to_seasonweek_n(53), 18.5)
  expect_equal(isoweek_to_seasonweek_n(TRUE), NA_integer_)
})

test_that("isoyearweek_to_season_c splits seasons at ISO week 35", {
  expect_equal(
    isoyearweek_to_season_c(yw),
    c(
      "2019/2020",
      "2020/2021",
      "2020/2021",
      "2021/2022",
      "2021/2022",
      "2018/2019"
    )
  )
  expect_type(isoyearweek_to_season_c(yw), "character")
  # week 34 still belongs to the old season, week 35 starts the new one
  expect_equal(isoyearweek_to_season_c("2021-34"), "2020/2021")
  expect_equal(isoyearweek_to_season_c("2021-35"), "2021/2022")
  expect_equal(isoyearweek_to_season_c(42), NA_integer_)
})

test_that("isoyearweek_to_seasonweek_n reads the ISO week out of the yearweek", {
  expect_equal(
    isoyearweek_to_seasonweek_n(yw),
    c(19, 18.5, 19, 1, 16, 37)
  )
  expect_type(isoyearweek_to_seasonweek_n(yw), "double")
  expect_equal(isoyearweek_to_seasonweek_n("2020-53"), 18.5)
  expect_equal(isoyearweek_to_seasonweek_n("2021-35"), 1)
})

# seasonweek -> isoweek ====

test_that("seasonweek_to_isoweek_c inverts the season week as a zero-padded string", {
  expect_equal(
    seasonweek_to_isoweek_c(c(1, 2, 18, 19, 52)),
    c("35", "36", "52", "01", "34")
  )
  expect_type(seasonweek_to_isoweek_c(1), "character")
  expect_equal(seasonweek_to_isoweek_c(1), "35")
  expect_equal(seasonweek_to_isoweek_c(19), "01")
  expect_equal(seasonweek_to_isoweek_c("1"), NA_character_)
})

test_that("seasonweek_to_isoweek_n inverts the season week as an integer", {
  expect_equal(
    seasonweek_to_isoweek_n(c(1, 2, 18, 19, 52)),
    c(35L, 36L, 52L, 1L, 34L)
  )
  expect_type(seasonweek_to_isoweek_n(1), "integer")
  expect_equal(seasonweek_to_isoweek_n(1), 35L)
  expect_equal(seasonweek_to_isoweek_n(19), 1L)
  expect_equal(seasonweek_to_isoweek_n("1"), NA_integer_)
})

# season -> last date ====

test_that("season_to_last_date returns the Sunday ending the season", {
  expect_equal(
    season_to_last_date(c("2019/2020", "2020/2021", "2021/2022")),
    as.Date(c("2020-08-23", "2021-08-29", "2022-08-28"))
  )
  expect_s3_class(season_to_last_date("2019/2020"), "Date")
  # every returned date is a Sunday
  expect_equal(
    weekdays(
      season_to_last_date(c("2019/2020", "2020/2021", "2021/2022")),
      abbreviate = FALSE
    ),
    weekdays(
      as.Date(c("2020-08-23", "2020-08-23", "2020-08-23")),
      abbreviate = FALSE
    )
  )
  expect_equal(season_to_last_date(42), as.Date(NA))
})

# pinned oddities that are NOT bugs ====

test_that("date_to_isoquarter_n gives 4 for 2021-01-01 because the ISO year is 2020 week 53", {
  # PINNED ODDITY 2: calendar January, but ISO year 2020 week 53, so Q4. Do not fix.
  expect_equal(date_to_isoquarter_n("2021-01-01"), 4)
  expect_false(date_to_isoquarter_n("2021-01-01") == 1)
  # the calendar month of the same date is January, which is what makes it odd
  expect_equal(date_to_calmonth_n("2021-01-01"), 1L)
})

test_that("invalid input returns NA rather than erroring", {
  # PINNED ODDITY 3: out-of-table and wrong-type input return NA. There is no
  # validation, no stop(), no warning. Do not add any.
  expect_equal(date_to_isoyear_n("1800-01-01"), NA_integer_)
  expect_equal(date_to_isoyear_c(42), NA_character_)
  expect_equal(date_to_isoyear_c(NA), NA_character_)
  expect_silent(date_to_isoyear_n("1800-01-01"))
  expect_silent(date_to_isoyear_c(42))
  expect_silent(date_to_isoyear_c(NA))
  # the same holds for the calendar and season conversions
  expect_equal(date_to_calyear_n("1800-01-01"), NA_integer_)
  expect_equal(date_to_calyearmonth_c(NA), NA_character_)
})

# now_c ====

test_that("now_c returns a single formatted timestamp string", {
  # now_c() returns the CURRENT time, so there is no fixed value to pin and it
  # cannot be mutation-proven. Shape only.
  x <- now_c()
  expect_type(x, "character")
  expect_length(x, 1)
  expect_match(x, "^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}$")
  expect_match(now_c(format = "%Y-%m-%d"), "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")
})
