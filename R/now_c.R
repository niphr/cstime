#' Current time as character
#'
#' Returns the current system time formatted as a character string.
#'
#' @details
#' The current time is taken from [base::Sys.time()] and formatted with the
#' supplied `format` string, which uses the conversion codes documented in
#' [base::strptime()].
#'
#' @param format A format string passed to [base::format()]. Defaults to
#'   "%Y-%m-%d %H:%M:%S".
#' @return The current time as a single character string.
#' @export
#' @examples
#' now_c()
#' now_c(format = "%Y-%m-%d")
now_c <- function(format = "%Y-%m-%d %H:%M:%S"){
  format(Sys.time(), format)
}
