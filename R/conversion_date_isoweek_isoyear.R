#' Date to ISO year (character)
#'
#' Converts a date to its ISO 8601 week-based year, returned as a character
#' string.
#'
#' @details
#' The ISO 8601 week-based year is not always the same as the calendar year.
#' ISO weeks run Monday to Sunday. Week 1 is the week that contains the first
#' Thursday of the year. As a result, the first days of January can belong to
#' the last ISO week of the previous year. The last days of December can belong
#' to ISO week 1 of the following year. For example, 2021-01-01 is a Friday that
#' falls in ISO week 53 of ISO year 2020.
#'
#' @param x A Date object, or a character string in the format 'yyyy-mm-dd'.
#'
#' @return ISO year as a character vector (e.g. "2021").
#' @family date-to-character converters
#' @seealso `vignette("cstime", package = "cstime")` and
#'   `vignette("date_conversion", package = "cstime")`, which both run this
#'   function.
#' @export
#'
#' @examples
#' date_to_isoyear_c(as.Date("2021-08-11"))
#' date_to_isoyear_c("2021-01-01")
date_to_isoyear_c <- function(x = lubridate::today()) {
  UseMethod("date_to_isoyear_c", x)
}

#' @rdname date_to_isoyear_c
#' @export
date_to_isoyear_c.default <- function(x) {
  rep(NA_character_, length(x))
}

#' @rdname date_to_isoyear_c
#' @export
date_to_isoyear_c.character <- function(x = lubridate::today()) {
  conversions_date_c_to[.(x)]$isoyear_c
}

#' @rdname date_to_isoyear_c
#' @export
date_to_isoyear_c.Date <- function(x = lubridate::today()) {
  conversions_date_to[.(x)]$isoyear_c
}

#' Date to ISO year (numeric)
#'
#' Converts a date to its ISO 8601 week-based year, returned as a number.
#'
#' @details
#' The ISO 8601 week-based year can differ from the calendar year near the
#' start and end of January and December. See [date_to_isoyear_c()] for the
#' rules used to assign weeks and years.
#'
#' @param x A Date object, or a character string in the format 'yyyy-mm-dd'.
#'
#' @return ISO year as an integer vector (e.g. 2021).
#' @family date-to-number converters
#' @seealso `vignette("date_conversion", package = "cstime")`, which runs this
#'   function.
#' @export
#'
#' @examples
#' date_to_isoyear_n(as.Date("2021-08-11"))
#' date_to_isoyear_n("2021-01-01")
date_to_isoyear_n <- function(x = lubridate::today()) {
  UseMethod("date_to_isoyear_n", x)
}

#' @rdname date_to_isoyear_n
#' @export
date_to_isoyear_n.default <- function(x) {
  rep(NA_integer_, length(x))
}

#' @rdname date_to_isoyear_n
#' @export
date_to_isoyear_n.character <- function(x = lubridate::today()) {
  conversions_date_c_to[.(x)]$isoyear_n
}

#' @rdname date_to_isoyear_n
#' @export
date_to_isoyear_n.Date <- function(x = lubridate::today()) {
  conversions_date_to[.(x)]$isoyear_n
}

#' Date to ISO week (character)
#'
#' Converts a date to its ISO 8601 week number, returned as a zero-padded
#' character string.
#'
#' @details
#' ISO weeks run Monday to Sunday and are numbered 01 to 52 or 53. Week 01 is
#' the week containing the first Thursday of the ISO year. The week is returned
#' as two digits, e.g. "01" or "53".
#'
#' @param x A Date object, or a character string in the format 'yyyy-mm-dd'.
#'
#' @return ISO week as a character vector (e.g. "32").
#' @family date-to-character converters
#' @seealso `vignette("date_conversion", package = "cstime")`, which runs this
#'   function.
#' @export
#'
#' @examples
#' date_to_isoweek_c(as.Date("2021-08-11"))
#' date_to_isoweek_c("2021-01-01")
date_to_isoweek_c <- function(x = lubridate::today()) {
  UseMethod("date_to_isoweek_c", x)
}

#' @rdname date_to_isoweek_c
#' @export
date_to_isoweek_c.default <- function(x) {
  rep(NA_character_, length(x))
}

#' @rdname date_to_isoweek_c
#' @export
date_to_isoweek_c.character <- function(x = lubridate::today()) {
  conversions_date_c_to[.(x)]$isoweek_c
}

#' @rdname date_to_isoweek_c
#' @export
date_to_isoweek_c.Date <- function(x = lubridate::today()) {
  conversions_date_to[.(x)]$isoweek_c
}

#' Date to ISO week (numeric)
#'
#' Converts a date to its ISO 8601 week number, returned as a number.
#'
#' @details
#' ISO weeks run Monday to Sunday and are numbered 1 to 52 or 53. Week 1 is the
#' week containing the first Thursday of the ISO year.
#'
#' @param x A Date object, or a character string in the format 'yyyy-mm-dd'.
#'
#' @return ISO week as an integer vector (1 to 53).
#' @family date-to-number converters
#' @seealso `vignette("date_conversion", package = "cstime")`, which runs this
#'   function.
#' @export
#'
#' @examples
#' date_to_isoweek_n(as.Date("2021-08-11"))
#' date_to_isoweek_n("2021-01-01")
date_to_isoweek_n <- function(x = lubridate::today()) {
  UseMethod("date_to_isoweek_n", x)
}

#' @rdname date_to_isoweek_n
#' @export
date_to_isoweek_n.default <- function(x) {
  rep(NA_integer_, length(x))
}

#' @rdname date_to_isoweek_n
#' @export
date_to_isoweek_n.character <- function(x = lubridate::today()) {
  conversions_date_c_to[.(x)]$isoweek_n
}

#' @rdname date_to_isoweek_n
#' @export
date_to_isoweek_n.Date <- function(x = lubridate::today()) {
  conversions_date_to[.(x)]$isoweek_n
}

#' Date to ISO yearweek (character)
#'
#' Converts a date to a combined ISO 8601 year and week string of the form
#' "yyyy-ww".
#'
#' @details
#' The output combines the ISO year (see [date_to_isoyear_c()]) and the
#' zero-padded ISO week (see [date_to_isoweek_c()]), separated by a hyphen, for
#' example "2021-32". Because the ISO year can differ from the calendar year,
#' 2021-01-01 maps to "2020-53".
#'
#' @param x A Date object, or a character string in the format 'yyyy-mm-dd'.
#'
#' @return ISO yearweek as a character vector (e.g. "2021-32").
#' @family date-to-character converters
#' @seealso `vignette("date_conversion", package = "cstime")`, which runs this
#'   function.
#' @export
#'
#' @examples
#' date_to_isoyearweek_c(as.Date("2021-08-11"))
#' date_to_isoyearweek_c("2021-01-01")
date_to_isoyearweek_c <- function(x = lubridate::today()) {
  UseMethod("date_to_isoyearweek_c", x)
}

#' @rdname date_to_isoyearweek_c
#' @export
date_to_isoyearweek_c.default <- function(x) {
  rep(NA_character_, length(x))
}

#' @rdname date_to_isoyearweek_c
#' @export
date_to_isoyearweek_c.character <- function(x = lubridate::today()) {
  conversions_date_c_to[.(x)]$isoyearweek_c
}

#' @rdname date_to_isoyearweek_c
#' @export
date_to_isoyearweek_c.Date <- function(x = lubridate::today()) {
  conversions_date_to[.(x)]$isoyearweek_c
}

#' Date to ISO quarter (numeric)
#'
#' Converts a date to an ISO-week-based quarter (1 to 4), returned as a number.
#'
#' @details
#' The quarter comes from the ISO week, not from the calendar month:
#'
#' - Weeks 1 to 13 are quarter 1.
#' - Weeks 14 to 26 are quarter 2.
#' - Weeks 27 to 39 are quarter 3.
#' - Weeks 40 and later are quarter 4. This includes week 53 in a long ISO year.
#'
#' @param x A Date object, or a character string in the format 'yyyy-mm-dd'.
#'
#' @return ISO quarter as a numeric vector (1 to 4).
#' @family date-to-number converters
#' @seealso `vignette("date_conversion", package = "cstime")` for worked date,
#'   ISO year and ISO week conversions. No vignette runs this function.
#' @export
#'
#' @examples
#' date_to_isoquarter_n(as.Date("2021-08-11"))
#' date_to_isoquarter_n("2021-01-01")
date_to_isoquarter_n <- function(x = lubridate::today()) {
  UseMethod("date_to_isoquarter_n", x)
}

#' @rdname date_to_isoquarter_n
#' @export
date_to_isoquarter_n.default <- function(x) {
  rep(NA_integer_, length(x))
}

#' @rdname date_to_isoquarter_n
#' @export
date_to_isoquarter_n.character <- function(x = lubridate::today()) {
  conversions_date_c_to[.(x)]$isoquarter_n
}

#' @rdname date_to_isoquarter_n
#' @export
date_to_isoquarter_n.Date <- function(x = lubridate::today()) {
  conversions_date_to[.(x)]$isoquarter_n
}

#' Date to ISO quarter (character)
#'
#' Converts a date to an ISO-week-based quarter (1 to 4), returned as a
#' character string.
#'
#' @details
#' The quarter is derived from the ISO week. See [date_to_isoquarter_n()] for
#' the week-to-quarter boundaries.
#'
#' @param x A Date object, or a character string in the format 'yyyy-mm-dd'.
#'
#' @return ISO quarter as a character vector (e.g. "3").
#' @family date-to-character converters
#' @seealso `vignette("date_conversion", package = "cstime")` for worked date,
#'   ISO year and ISO week conversions. No vignette runs this function.
#' @export
#'
#' @examples
#' date_to_isoquarter_c(as.Date("2021-08-11"))
#' date_to_isoquarter_c("2021-01-01")
date_to_isoquarter_c <- function(x = lubridate::today()) {
  UseMethod("date_to_isoquarter_c", x)
}

#' @rdname date_to_isoquarter_c
#' @export
date_to_isoquarter_c.default <- function(x) {
  rep(NA_character_, length(x))
}

#' @rdname date_to_isoquarter_c
#' @export
date_to_isoquarter_c.character <- function(x = lubridate::today()) {
  conversions_date_c_to[.(x)]$isoquarter_c
}

#' @rdname date_to_isoquarter_c
#' @export
date_to_isoquarter_c.Date <- function(x = lubridate::today()) {
  conversions_date_to[.(x)]$isoquarter_c
}

#' Date to ISO yearquarter (character)
#'
#' Converts a date to a combined ISO year and quarter string of the form
#' "yyyy-Qn".
#'
#' @details
#' The output combines the ISO year (see [date_to_isoyear_c()]) and the
#' ISO-week-based quarter (see [date_to_isoquarter_c()]), for example
#' "2021-Q3".
#'
#' @param x A Date object, or a character string in the format 'yyyy-mm-dd'.
#'
#' @return ISO yearquarter as a character vector (e.g. "2021-Q3").
#' @family date-to-character converters
#' @seealso `vignette("date_conversion", package = "cstime")` for worked date,
#'   ISO year and ISO week conversions. No vignette runs this function.
#' @export
#'
#' @examples
#' date_to_isoyearquarter_c(as.Date("2021-08-11"))
#' date_to_isoyearquarter_c("2021-01-01")
date_to_isoyearquarter_c <- function(x = lubridate::today()) {
  UseMethod("date_to_isoyearquarter_c", x)
}

#' @rdname date_to_isoyearquarter_c
#' @export
date_to_isoyearquarter_c.default <- function(x) {
  rep(NA_character_, length(x))
}

#' @rdname date_to_isoyearquarter_c
#' @export
date_to_isoyearquarter_c.character <- function(x = lubridate::today()) {
  conversions_date_c_to[.(x)]$isoyearquarter_c
}

#' @rdname date_to_isoyearquarter_c
#' @export
date_to_isoyearquarter_c.Date <- function(x = lubridate::today()) {
  conversions_date_to[.(x)]$isoyearquarter_c
}

#
# isoyearweek vs isoyear, isoweek, isoquarter ====
#

#' ISO yearweek to ISO year (numeric)
#'
#' Extracts the ISO year from an ISO yearweek string and returns it as a number.
#'
#' @details
#' The input is split on the hyphen into year and week, and the year part is
#' returned. The week part is ignored.
#'
#' @param x ISO yearweek as a character string of the form "yyyy-ww", e.g.
#'   "2020-19" for the 19th ISO week of 2020.
#' @return ISO year as an integer vector (e.g. 2020).
#' @family ISO yearweek-to-number converters
#' @seealso `vignette("date_conversion", package = "cstime")`, which runs this
#'   function.
#' @rdname isoyearweek_to_isoyear_n
#' @export
#'
#' @examples
#' isoyearweek_to_isoyear_n("2020-10")
isoyearweek_to_isoyear_n <- function(x) {
  UseMethod("isoyearweek_to_isoyear_n", x)
}

#' @rdname isoyearweek_to_isoyear_n
#' @export
isoyearweek_to_isoyear_n.default <- function(x) {
  rep(NA_integer_, length(x))
}

#' @rdname isoyearweek_to_isoyear_n
#' @export
isoyearweek_to_isoyear_n.character <- function(x) {
  conversions_isoyearweek_to[.(x)]$isoyear_n
}

#' ISO yearweek to ISO year (character)
#'
#' Extracts the ISO year from an ISO yearweek string and returns it as a
#' character string.
#'
#' @details
#' The input is split on the hyphen into year and week, and the year part is
#' returned. The week part is ignored.
#'
#' @param x ISO yearweek as a character string of the form "yyyy-ww", e.g.
#'   "2020-19" for the 19th ISO week of 2020.
#' @return ISO year as a character vector (e.g. "2020").
#' @family ISO yearweek-to-character converters
#' @seealso `vignette("cstime", package = "cstime")` and
#'   `vignette("date_conversion", package = "cstime")`, which both run this
#'   function.
#' @rdname isoyearweek_to_isoyear_c
#' @export
#' @examples
#' isoyearweek_to_isoyear_c("2020-10")
isoyearweek_to_isoyear_c <- function(x) {
  UseMethod("isoyearweek_to_isoyear_c", x)
}

#' @rdname isoyearweek_to_isoyear_c
#' @export
isoyearweek_to_isoyear_c.default <- function(x) {
  rep(NA_character_, length(x))
}

#' @rdname isoyearweek_to_isoyear_c
#' @export
isoyearweek_to_isoyear_c.character <- function(x) {
  conversions_isoyearweek_to[.(x)]$isoyear_c
}

#' ISO yearweek to ISO week (numeric)
#'
#' Extracts the ISO week from an ISO yearweek string and returns it as a number.
#'
#' @details
#' The input is split on the hyphen into year and week, and the week part is
#' returned. The year part is ignored.
#'
#' @param x ISO yearweek as a character string of the form "yyyy-ww", e.g.
#'   "2020-19" for the 19th ISO week of 2020.
#' @return ISO week as an integer vector (1 to 53).
#' @family ISO yearweek-to-number converters
#' @seealso `vignette("date_conversion", package = "cstime")`, which runs this
#'   function.
#' @rdname isoyearweek_to_isoweek_n
#' @export
#' @examples
#' isoyearweek_to_isoweek_n("2020-19")
isoyearweek_to_isoweek_n <- function(x) {
  UseMethod("isoyearweek_to_isoweek_n", x)
}

#' @rdname isoyearweek_to_isoweek_n
#' @export
isoyearweek_to_isoweek_n.default <- function(x) {
  rep(NA_integer_, length(x))
}

#' @rdname isoyearweek_to_isoweek_n
#' @export
isoyearweek_to_isoweek_n.character <- function(x) {
  conversions_isoyearweek_to[.(x)]$isoweek_n
}

#' ISO yearweek to ISO week (character)
#'
#' Extracts the ISO week from an ISO yearweek string and returns it as a
#' character string.
#'
#' @details
#' The input is split on the hyphen into year and week, and the week part is
#' returned. The year part is ignored.
#'
#' @param x ISO yearweek as a character string of the form "yyyy-ww", e.g.
#'   "2020-19" for the 19th ISO week of 2020.
#' @return ISO week as a character vector (e.g. "19").
#' @family ISO yearweek-to-character converters
#' @seealso `vignette("cstime", package = "cstime")` and
#'   `vignette("date_conversion", package = "cstime")`, which both run this
#'   function.
#' @rdname isoyearweek_to_isoweek_c
#' @export
#' @examples
#' isoyearweek_to_isoweek_c("2020-19")
isoyearweek_to_isoweek_c <- function(x) {
  UseMethod("isoyearweek_to_isoweek_c", x)
}

#' @rdname isoyearweek_to_isoweek_c
#' @export
isoyearweek_to_isoweek_c.default <- function(x) {
  rep(NA_character_, length(x))
}

#' @rdname isoyearweek_to_isoweek_c
#' @export
isoyearweek_to_isoweek_c.character <- function(x) {
  conversions_isoyearweek_to[.(x)]$isoweek_c
}

#' ISO yearweek to ISO quarter (numeric)
#'
#' Maps an ISO yearweek to its ISO-week-based quarter (1 to 4), returned as a
#' number.
#'
#' @details
#' The quarter comes from the ISO week part of the input:
#'
#' - Weeks 1 to 13 are quarter 1.
#' - Weeks 14 to 26 are quarter 2.
#' - Weeks 27 to 39 are quarter 3.
#' - Weeks 40 and later are quarter 4. This includes week 53.
#'
#' @param x ISO yearweek as a character string of the form "yyyy-ww", e.g.
#'   "2020-19" for the 19th ISO week of 2020.
#' @return ISO quarter as a numeric vector (1 to 4).
#' @family ISO yearweek-to-number converters
#' @seealso `vignette("date_conversion", package = "cstime")` for worked date,
#'   ISO year and ISO week conversions. No vignette runs this function.
#' @rdname isoyearweek_to_isoquarter_n
#' @export
#' @examples
#' isoyearweek_to_isoquarter_n("2020-19")
isoyearweek_to_isoquarter_n <- function(x) {
  UseMethod("isoyearweek_to_isoquarter_n", x)
}

#' @rdname isoyearweek_to_isoquarter_n
#' @export
isoyearweek_to_isoquarter_n.default <- function(x) {
  rep(NA_integer_, length(x))
}

#' @rdname isoyearweek_to_isoquarter_n
#' @export
isoyearweek_to_isoquarter_n.character <- function(x) {
  conversions_isoyearweek_to[.(x)]$isoquarter_n
}

#' ISO yearweek to ISO quarter (character)
#'
#' Maps an ISO yearweek to its ISO-week-based quarter (1 to 4), returned as a
#' character string.
#'
#' @details
#' The quarter is derived from the ISO week part of the input. See
#' [isoyearweek_to_isoquarter_n()] for the week-to-quarter boundaries.
#'
#' @param x ISO yearweek as a character string of the form "yyyy-ww", e.g.
#'   "2020-19" for the 19th ISO week of 2020.
#' @return ISO quarter as a character vector (e.g. "2").
#' @family ISO yearweek-to-character converters
#' @seealso `vignette("date_conversion", package = "cstime")` for worked date,
#'   ISO year and ISO week conversions. No vignette runs this function.
#' @rdname isoyearweek_to_isoquarter_c
#' @export
#' @examples
#' isoyearweek_to_isoquarter_c("2020-19")
isoyearweek_to_isoquarter_c <- function(x) {
  UseMethod("isoyearweek_to_isoquarter_c", x)
}

#' @rdname isoyearweek_to_isoquarter_c
#' @export
isoyearweek_to_isoquarter_c.default <- function(x) {
  rep(NA_character_, length(x))
}

#' @rdname isoyearweek_to_isoquarter_c
#' @export
isoyearweek_to_isoquarter_c.character <- function(x) {
  conversions_isoyearweek_to[.(x)]$isoquarter_c
}

#' ISO yearweek to ISO yearquarter (character)
#'
#' Maps an ISO yearweek to a combined ISO year and quarter string of the form
#' "yyyy-Qn".
#'
#' @details
#' The output keeps the year part of the input and appends the ISO-week-based
#' quarter (see [isoyearweek_to_isoquarter_c()]), for example "2020-Q2".
#'
#' @param x ISO yearweek as a character string of the form "yyyy-ww", e.g.
#'   "2020-19" for the 19th ISO week of 2020.
#' @return ISO yearquarter as a character vector (e.g. "2020-Q2").
#' @family ISO yearweek-to-character converters
#' @seealso `vignette("date_conversion", package = "cstime")` for worked date,
#'   ISO year and ISO week conversions. No vignette runs this function.
#' @rdname isoyearweek_to_isoyearquarter_c
#' @export
#' @examples
#' isoyearweek_to_isoyearquarter_c("2020-19")
isoyearweek_to_isoyearquarter_c <- function(x) {
  UseMethod("isoyearweek_to_isoyearquarter_c", x)
}

#' @rdname isoyearweek_to_isoyearquarter_c
#' @export
isoyearweek_to_isoyearquarter_c.default <- function(x) {
  rep(NA_character_, length(x))
}

#' @rdname isoyearweek_to_isoyearquarter_c
#' @export
isoyearweek_to_isoyearquarter_c.character <- function(x) {
  conversions_isoyearweek_to[.(x)]$isoyearquarter_c
}

#
# Downsizing (isoyear -> isoyearweek/date -> date) ====
#

#' ISO year to last ISO yearweek (character)
#'
#' Returns the last ISO yearweek of a given ISO year as a "yyyy-ww" string.
#'
#' @details
#' Most ISO years have 52 weeks, so the result is usually "yyyy-52". ISO years
#' that contain 53 weeks (such as 2020) instead return "yyyy-53". The year is
#' accepted as either a number or a character string.
#'
#' @param x ISO year as a number or character string, e.g. 2020 or "2020".
#' @return Last ISO yearweek of the year as a character vector (e.g. "2020-53").
#' @seealso [isoyear_to_last_isoweek_n()] and [isoyear_to_last_date()] answer
#'   the same question as a week number and as a date.
#'   `vignette("date_conversion", package = "cstime")` for worked date, ISO year
#'   and ISO week conversions. No vignette runs this function.
#' @rdname isoyear_to_last_isoyearweek_c
#' @examples
#' isoyear_to_last_isoyearweek_c(c(2019, 2020, 2021))
#' isoyear_to_last_isoyearweek_c("2020")
#' @export
isoyear_to_last_isoyearweek_c <- function(x) {
  UseMethod("isoyear_to_last_isoyearweek_c", x)
}

#' @rdname isoyear_to_last_isoyearweek_c
#' @export
isoyear_to_last_isoyearweek_c.default <- function(x) {
  rep(NA_character_, length(x))
}

#' @rdname isoyear_to_last_isoyearweek_c
#' @export
isoyear_to_last_isoyearweek_c.character <- function(x) {
  conversions_isoyear_c_to[.(x)]$last_isoyearweek_c
}

#' @rdname isoyear_to_last_isoyearweek_c
#' @export
isoyear_to_last_isoyearweek_c.numeric <- function(x) {
  conversions_isoyear_n_to[.(x)]$last_isoyearweek_c
}

#' ISO year to last ISO week (numeric)
#'
#' Returns the number of the last ISO week in a given ISO year, that is, the
#' count of ISO weeks in that year.
#'
#' @details
#' This is 52 for most years and 53 for long ISO years such as 2020. The year is
#' accepted as either a number or a character string.
#'
#' @param x ISO year as a number or character string, e.g. 2020 or "2020".
#' @return Last ISO week of the year as an integer vector (52 or 53).
#' @seealso [isoyear_to_last_isoyearweek_c()] and [isoyear_to_last_date()]
#'   answer the same question as a yearweek string and as a date.
#'   `vignette("date_conversion", package = "cstime")` for worked date, ISO year
#'   and ISO week conversions. No vignette runs this function.
#' @rdname isoyear_to_last_isoweek_n
#' @examples
#' isoyear_to_last_isoweek_n(c(2019, 2020, 2021))
#' isoyear_to_last_isoweek_n("2020")
#' @export
isoyear_to_last_isoweek_n <- function(x) {
  UseMethod("isoyear_to_last_isoweek_n", x)
}

#' @rdname isoyear_to_last_isoweek_n
#' @export
isoyear_to_last_isoweek_n.default <- function(x) {
  rep(NA_integer_, length(x))
}

#' @rdname isoyear_to_last_isoweek_n
#' @export
isoyear_to_last_isoweek_n.character <- function(x) {
  conversions_isoyear_c_to[.(x)]$last_isoweek_n
}

#' @rdname isoyear_to_last_isoweek_n
#' @export
isoyear_to_last_isoweek_n.numeric <- function(x) {
  conversions_isoyear_n_to[.(x)]$last_isoweek_n
}

#' ISO year to last date (Sunday)
#'
#' Returns the date of the Sunday that ends the last ISO week of a given ISO
#' year.
#'
#' @details
#' ISO weeks end on Sunday, so the returned date is the Sunday of the final ISO
#' week. ISO years and calendar years are not aligned. The returned date can
#' therefore fall in early January of the following calendar year. For example,
#' the last date of ISO year 2020 is 2021-01-03. The function accepts the year
#' as either a number or a character string.
#'
#' @param x ISO year as a number or character string, e.g. 2020 or "2020".
#' @return A [base::Date] vector giving the last Sunday of each ISO year.
#' @seealso [isoyear_to_last_isoweek_n()] and [isoyear_to_last_isoyearweek_c()]
#'   answer the same question as a week number and as a yearweek string.
#'   [isoyearweek_to_last_date()] and [season_to_last_date()] do the same for an
#'   ISO yearweek and for a season.
#'   `vignette("date_conversion", package = "cstime")` for worked date, ISO year
#'   and ISO week conversions. No vignette runs this function.
#' @rdname isoyear_to_last_date
#' @examples
#' isoyear_to_last_date(c(2019, 2020, 2021))
#' isoyear_to_last_date("2020")
#' @export
isoyear_to_last_date <- function(x) {
  UseMethod("isoyear_to_last_date", x)
}

#' @rdname isoyear_to_last_date
#' @export
isoyear_to_last_date.default <- function(x) {
  rep(as.Date(NA), length(x))
}

#' @rdname isoyear_to_last_date
#' @export
isoyear_to_last_date.character <- function(x) {
  conversions_isoyear_c_to[.(x)]$last_date
}

#' @rdname isoyear_to_last_date
#' @export
isoyear_to_last_date.numeric <- function(x) {
  conversions_isoyear_n_to[.(x)]$last_date
}

#' ISO yearweek to last date (Sunday)
#'
#' Returns the date of the Sunday that ends a given ISO yearweek.
#'
#' @details
#' ISO weeks run Monday to Sunday, so the returned date is the Sunday of the
#' supplied yearweek.
#'
#' @param x ISO yearweek as a character string of the form "yyyy-ww", e.g.
#'   "2020-19" for the 19th ISO week of 2020.
#' @return A [base::Date] vector giving the Sunday of each ISO yearweek.
#' @seealso [isoyear_to_last_date()] and [season_to_last_date()] do the same for
#'   an ISO year and for a season. Each takes a different input grammar, so they
#'   are not interchangeable.
#'   `vignette("date_conversion", package = "cstime")` for worked date, ISO year
#'   and ISO week conversions. No vignette runs this function.
#' @rdname isoyearweek_to_last_date
#' @examples
#' isoyearweek_to_last_date(c("2019-19", "2020-01"))
#' @export
isoyearweek_to_last_date <- function(x) {
  UseMethod("isoyearweek_to_last_date", x)
}

#' @rdname isoyearweek_to_last_date
#' @export
isoyearweek_to_last_date.default <- function(x) {
  rep(as.Date(NA), length(x))
}

#' @rdname isoyearweek_to_last_date
#' @export
isoyearweek_to_last_date.character <- function(x) {
  conversions_isoyearweek_to[.(x)]$last_date
}

#' Season to last date (Sunday)
#'
#' Returns the date of the Sunday that ends a given season.
#'
#' @details
#' A season is written "yyyy/yyyy" where the two years are consecutive, for
#' example "2019/2020". Seasons are aligned to ISO weeks, and season week 1
#' starts at ISO week 35. The season therefore ends in late summer of the second
#' year. The returned date is the Sunday of the final week of the season.
#'
#' @param x Season as a character string of the form "yyyy/yyyy", e.g.
#'   "2019/2020".
#' @return A [base::Date] vector giving the last Sunday of each season.
#' @seealso [isoyearweek_to_season_c()] finds the season an ISO yearweek belongs
#'   to. [isoyear_to_last_date()] and [isoyearweek_to_last_date()] do the same
#'   for an ISO year and for an ISO yearweek.
#'   `vignette("season", package = "cstime")` for worked season week
#'   conversions. No vignette runs this function.
#' @rdname season_to_last_date
#' @examples
#' season_to_last_date(c("2019/2020", "2020/2021"))
#' @export
season_to_last_date <- function(x) {
  UseMethod("season_to_last_date", x)
}

#' @rdname season_to_last_date
#' @export
season_to_last_date.default <- function(x) {
  rep(as.Date(NA), length(x))
}

#' @rdname season_to_last_date
#' @export
season_to_last_date.character <- function(x) {
  conversions_season_to[.(x)]$last_date
}
