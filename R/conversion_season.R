#' ISO week to season week (numeric)
#'
#' Maps an ISO week number to its position within the surveillance season,
#' where season week 1 corresponds to ISO week 35.
#'
#' @details
#' Surveillance seasons start at ISO week 35, so ISO week 35 is season week 1.
#' ISO week 36 is season week 2, and the numbering continues across the new
#' year. ISO week 53 occurs only in long ISO years. It maps to the half-step
#' season week 18.5, so that the surrounding weeks keep consistent numbering.
#'
#' @param x ISO week as a number between 1 and 53.
#' @return Season week as a numeric vector (ISO week 53 returns 18.5).
#' @seealso [seasonweek_to_isoweek_n()] and [seasonweek_to_isoweek_c()] convert
#'   back. They return different classes from each other, so they are not a
#'   family.
#'   `vignette("cstime", package = "cstime")` and
#'   `vignette("season", package = "cstime")`, which both run this function.
#' @rdname isoweek_to_seasonweek_n
#' @export
#' @examples
#' isoweek_to_seasonweek_n(35)
#' isoweek_to_seasonweek_n(c(31, 53))
isoweek_to_seasonweek_n <- function(x) {
  UseMethod("isoweek_to_seasonweek_n", x)
}

#' @rdname isoweek_to_seasonweek_n
#' @export
isoweek_to_seasonweek_n.default <- function(x) {
  rep(NA_integer_, length(x))
}

#' @rdname isoweek_to_seasonweek_n
#' @export
isoweek_to_seasonweek_n.character <- function(x) {
  conversions_isoweek_c_to[.(x)]$seasonweek_n
}

#' @rdname isoweek_to_seasonweek_n
#' @export
isoweek_to_seasonweek_n.numeric <- function(x) {
  conversions_isoweek_n_to[.(x)]$seasonweek_n
}

#' ISO yearweek to season week (numeric)
#'
#' Maps an ISO yearweek to its position within the surveillance season, where
#' season week 1 corresponds to ISO week 35.
#'
#' @details
#' This function takes the ISO week from the yearweek string and converts it
#' with [isoweek_to_seasonweek_n()]. The same season-week numbering applies,
#' including the 18.5 half-step for ISO week 53.
#'
#' @param x ISO yearweek as a character string of the form "yyyy-ww", e.g.
#'   "2021-01".
#' @return Season week as a numeric vector.
#' @family ISO yearweek-to-number converters
#' @seealso `vignette("season", package = "cstime")` for worked season week
#'   conversions. No vignette runs this function.
#' @examples
#' isoyearweek_to_seasonweek_n(c("2021-01", "2021-35"))
#' @export
isoyearweek_to_seasonweek_n <- function(x) {
  isoweek_to_seasonweek_n(isoyearweek_to_isoweek_n(x))
}

#' Season week to ISO week (character)
#'
#' Maps a season week number back to its ISO week, returned as a zero-padded
#' character string. This is the inverse of [isoweek_to_seasonweek_n()].
#'
#' @details
#' Season week 1 corresponds to ISO week 35, season week 2 to ISO week 36, and
#' so on, wrapping around the new year. The ISO week is returned as two digits,
#' e.g. "35" or "01".
#'
#' @param x Season week as a number between 1 and 52.
#' @return ISO week as a character vector (e.g. "35").
#' @seealso [seasonweek_to_isoweek_n()] returns the same week as a number, and
#'   [isoweek_to_seasonweek_n()] converts back.
#'   `vignette("season", package = "cstime")`, which runs this function.
#' @rdname seasonweek_to_isoweek_c
#' @export
#' @examples
#' seasonweek_to_isoweek_c(1)
#' seasonweek_to_isoweek_c(c(31, 52))
seasonweek_to_isoweek_c <- function(x) {
  UseMethod("seasonweek_to_isoweek_c", x)
}

#' @rdname seasonweek_to_isoweek_c
#' @export
seasonweek_to_isoweek_c.default <- function(x) {
  rep(NA_character_, length(x))
}

#' @rdname seasonweek_to_isoweek_c
#' @export
seasonweek_to_isoweek_c.numeric <- function(x) {
  conversions_seasonweek_to[.(x)]$isoweek_c
}

#' Season week to ISO week (numeric)
#'
#' Maps a season week number back to its ISO week, returned as a number. This is
#' the inverse of [isoweek_to_seasonweek_n()].
#'
#' @details
#' Season week 1 corresponds to ISO week 35, season week 2 to ISO week 36, and
#' so on, wrapping around the new year.
#'
#' @param x Season week as a number between 1 and 52.
#' @return ISO week as an integer vector (1 to 53).
#' @seealso [seasonweek_to_isoweek_c()] returns the same week as a zero-padded
#'   string, and [isoweek_to_seasonweek_n()] converts back.
#'   `vignette("cstime", package = "cstime")` and
#'   `vignette("season", package = "cstime")`, which both run this function.
#' @rdname seasonweek_to_isoweek_n
#' @export
#' @examples
#' seasonweek_to_isoweek_n(1)
#' seasonweek_to_isoweek_n(c(31, 52))
seasonweek_to_isoweek_n <- function(x) {
  UseMethod("seasonweek_to_isoweek_n", x)
}

#' @rdname seasonweek_to_isoweek_n
#' @export
seasonweek_to_isoweek_n.default <- function(x) {
  rep(NA_integer_, length(x))
}

#' @rdname seasonweek_to_isoweek_n
#' @export
seasonweek_to_isoweek_n.numeric <- function(x) {
  conversions_seasonweek_to[.(x)]$isoweek_n
}

#' ISO yearweek to season (character)
#'
#' Maps an ISO yearweek to the surveillance season it belongs to, written as
#' "yyyy/yyyy".
#'
#' @details
#' Seasons start at ISO week 35 (season week 1). ISO weeks 35 and later belong
#' to the season that starts in that calendar year. Earlier weeks belong to the
#' season that started in the previous calendar year. For example, "2021-01"
#' falls in season "2020/2021" and "2021-50" falls in season "2021/2022".
#'
#' @param x ISO yearweek as a character string of the form "yyyy-ww", e.g.
#'   "2021-01".
#' @return Season as a character vector (e.g. "2020/2021").
#' @seealso [isoyearweek_to_seasonweek_n()] for the week within that season, and
#'   [season_to_last_date()] for the date the season ends.
#'   `vignette("season", package = "cstime")` for worked season week
#'   conversions. No vignette runs this function.
#' @rdname isoyearweek_to_season_c
#' @examples
#' isoyearweek_to_season_c(c("2021-01", "2021-50"))
#' @export
isoyearweek_to_season_c <- function(x) {
  UseMethod("isoyearweek_to_season_c", x)
}

#' @rdname isoyearweek_to_season_c
#' @export
isoyearweek_to_season_c.default <- function(x) {
  rep(NA_integer_, length(x))
}

#' @rdname isoyearweek_to_season_c
#' @export
isoyearweek_to_season_c.character <- function(x) {
  conversions_isoyearweek_to[.(x)]$season_c
}

#' Date to season (character)
#'
#' Maps a date to the surveillance season it belongs to, written as
#' "yyyy/yyyy".
#'
#' @details
#' The date is first converted to an ISO yearweek with
#' [date_to_isoyearweek_c()] and then to a season with
#' [isoyearweek_to_season_c()]. Seasons start at ISO week 35, so dates in early
#' January belong to the season that began the previous calendar year.
#'
#' @param x A Date object, or a character string in the format 'yyyy-mm-dd'.
#' @return Season as a character vector (e.g. "2020/2021").
#' @seealso [date_to_seasonweek_n()] for the week within that season, and
#'   [isoyearweek_to_season_c()] to start from an ISO yearweek instead.
#'   `vignette("season", package = "cstime")` for worked season week
#'   conversions. No vignette runs this function.
#' @examples
#' date_to_season_c(c("2021-01-01", "2021-12-01"))
#' date_to_season_c(as.Date("2021-09-01"))
#' @export
date_to_season_c <- function(x) {
  isoyearweek_to_season_c(date_to_isoyearweek_c(x))
}

#' Date to season week (numeric)
#'
#' Maps a date to its position within the surveillance season, where season
#' week 1 corresponds to ISO week 35.
#'
#' @details
#' The date is first converted to an ISO yearweek with
#' [date_to_isoyearweek_c()] and then to a season week with
#' [isoyearweek_to_seasonweek_n()]. As with [isoweek_to_seasonweek_n()], ISO
#' week 53 maps to the half-step season week 18.5.
#'
#' @param x A Date object, or a character string in the format 'yyyy-mm-dd'.
#' @return Season week as a numeric vector.
#' @seealso [date_to_season_c()] for the season itself, and
#'   [isoyearweek_to_seasonweek_n()] to start from an ISO yearweek instead.
#'   `vignette("season", package = "cstime")` for worked season week
#'   conversions. No vignette runs this function.
#' @examples
#' date_to_seasonweek_n(c("2021-01-01", "2021-12-01"))
#' date_to_seasonweek_n(as.Date("2021-09-01"))
#' @export
date_to_seasonweek_n <- function(x) {
  isoyearweek_to_seasonweek_n(date_to_isoyearweek_c(x))
}
