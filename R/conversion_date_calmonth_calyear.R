#
# date -> calyear ====
#

#' Date to calendar year (character)
#'
#' Converts a date to its calendar (Gregorian) year, returned as a character
#' string.
#'
#' @details
#' Unlike the ISO year (see [date_to_isoyear_c()]), the calendar year is taken
#' directly from the date, so 2021-01-01 is calendar year "2021".
#'
#' @param x A Date object, or a character string in the format 'yyyy-mm-dd'.
#'
#' @return Calendar year as a character vector (e.g. "2021").
#' @rdname date_to_calyear_c
#' @export
#' @examples
#' date_to_calyear_c(as.Date("2021-08-11"))
#' date_to_calyear_c("2021-01-01")
date_to_calyear_c <- function(x = lubridate::today()) {
  UseMethod("date_to_calyear_c", x)
}

#' @rdname date_to_calyear_c
#' @export
date_to_calyear_c.default <- function(x) {
  rep(NA_character_, length(x))
}

#' @rdname date_to_calyear_c
#' @export
date_to_calyear_c.character <- function(x = lubridate::today()) {
  conversions_date_c_to[.(x)]$calyear_c
}

#' @rdname date_to_calyear_c
#' @export
date_to_calyear_c.Date <- function(x = lubridate::today()) {
  conversions_date_to[.(x)]$calyear_c
}


#' Date to calendar year (numeric)
#'
#' Converts a date to its calendar (Gregorian) year, returned as a number.
#'
#' @details
#' Unlike the ISO year (see [date_to_isoyear_n()]), the calendar year is taken
#' directly from the date, so 2021-01-01 is calendar year 2021.
#'
#' @param x A Date object, or a character string in the format 'yyyy-mm-dd'.
#'
#' @return Calendar year as an integer vector (e.g. 2021).
#' @rdname date_to_calyear_n
#' @export
#'
#' @examples
#' date_to_calyear_n(as.Date("2021-08-11"))
#' date_to_calyear_n("2021-01-01")
date_to_calyear_n <- function(x = lubridate::today()) {
  UseMethod("date_to_calyear_n", x)
}

#' @rdname date_to_calyear_n
#' @export
date_to_calyear_n.default <- function(x) {
  rep(NA_integer_, length(x))
}

#' @rdname date_to_calyear_n
#' @export
date_to_calyear_n.character <- function(x = lubridate::today()) {
  conversions_date_c_to[.(x)]$calyear_n
}

#' @rdname date_to_calyear_n
#' @export
date_to_calyear_n.Date <- function(x = lubridate::today()) {
  conversions_date_to[.(x)]$calyear_n
}

#' Date to calendar month (character)
#'
#' Converts a date to its calendar month number, returned as a zero-padded
#' character string.
#'
#' @details
#' The month is returned as two digits, "01" for January through "12" for
#' December.
#'
#' @param x A Date object, or a character string in the format 'yyyy-mm-dd'.
#'
#' @return Calendar month as a character vector ("01" to "12").
#' @rdname date_to_calmonth_c
#' @export
#'
#' @examples
#' date_to_calmonth_c(as.Date("2021-08-11"))
#' date_to_calmonth_c("2021-01-01")
date_to_calmonth_c <- function(x = lubridate::today()) {
  UseMethod("date_to_calmonth_c", x)
}

#' @rdname date_to_calmonth_c
#' @export
date_to_calmonth_c.default <- function(x) {
  rep(NA_character_, length(x))
}

#' @rdname date_to_calmonth_c
#' @export
date_to_calmonth_c.character <- function(x = lubridate::today()) {
  conversions_date_c_to[.(x)]$calmonth_c
}

#' @rdname date_to_calmonth_c
#' @export
date_to_calmonth_c.Date <- function(x = lubridate::today()) {
  conversions_date_to[.(x)]$calmonth_c
}

#' Date to calendar month (numeric)
#'
#' Converts a date to its calendar month number, returned as a number.
#'
#' @param x A Date object, or a character string in the format 'yyyy-mm-dd'.
#'
#' @return Calendar month as an integer vector (1 for January to 12 for
#'   December).
#' @rdname date_to_calmonth_n
#' @export
#'
#' @examples
#' date_to_calmonth_n(as.Date("2021-08-11"))
#' date_to_calmonth_n("2021-01-01")
date_to_calmonth_n <- function(x = lubridate::today()) {
  UseMethod("date_to_calmonth_n", x)
}

#' @rdname date_to_calmonth_n
#' @export
date_to_calmonth_n.default <- function(x) {
  rep(NA_integer_, length(x))
}

#' @rdname date_to_calmonth_n
#' @export
date_to_calmonth_n.character <- function(x = lubridate::today()) {
  conversions_date_c_to[.(x)]$calmonth_n
}

#' @rdname date_to_calmonth_n
#' @export
date_to_calmonth_n.Date <- function(x = lubridate::today()) {
  conversions_date_to[.(x)]$calmonth_n
}

#' Date to calendar yearmonth (character)
#'
#' Converts a date to a combined calendar year and month string of the form
#' "yyyy-Mmm".
#'
#' @details
#' The output combines the calendar year and the zero-padded calendar month,
#' separated by "-M", for example "2021-M08".
#'
#' @param x A Date object, or a character string in the format 'yyyy-mm-dd'.
#'
#' @return Calendar yearmonth as a character vector (e.g. "2021-M08").
#' @rdname date_to_calyearmonth_c
#' @export
#'
#' @examples
#' date_to_calyearmonth_c(as.Date("2021-08-11"))
#' date_to_calyearmonth_c("2021-01-01")
date_to_calyearmonth_c <- function(x = lubridate::today()) {
  UseMethod("date_to_calyearmonth_c", x)
}

#' @rdname date_to_calyearmonth_c
#' @export
date_to_calyearmonth_c.default <- function(x) {
  rep(NA_character_, length(x))
}

#' @rdname date_to_calyearmonth_c
#' @export
date_to_calyearmonth_c.character <- function(x = lubridate::today()) {
  conversions_date_c_to[.(x)]$calyearmonth_c
}

#' @rdname date_to_calyearmonth_c
#' @export
date_to_calyearmonth_c.Date <- function(x = lubridate::today()) {
  conversions_date_to[.(x)]$calyearmonth_c
}

