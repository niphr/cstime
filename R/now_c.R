#' Current time as character
#'
#' Returns the current system time formatted as a character string.
#'
#' @details
#' The current time is taken from [base::Sys.time()] and formatted with the
#' supplied `format` string, which uses the conversion codes documented in
#' [base::strptime()].
#'
#' The result depends on the time zone of the session. At one instant, a session
#' in Pacific/Auckland and a session in America/Los_Angeles can report different
#' calendar dates, not only different clock times.
#'
#' @param format A format string passed to [base::format()]. Defaults to
#'   "%Y-%m-%d %H:%M:%S".
#' @return The current time as a single character string.
#' @seealso [date_to_isoyearweek_c()] to convert a date rather than to read the
#'   clock. No vignette runs this function.
#' @export
#' @examples
#' now_c()
#' now_c(format = "%Y-%m-%d")
now_c <- function(format = "%Y-%m-%d %H:%M:%S") {
  format(Sys.time(), format)
}
