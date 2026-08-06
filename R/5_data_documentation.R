#' Dates of each weekday within ISO yearweeks
#'
#' A lookup table with one row per ISO yearweek, for ISO years 1900 to 2099.
#' Each row holds the date of every day of that week, Monday to Sunday, plus
#' list-columns that group those dates. The 10436 rows run from "1900-01" to
#' "2099-53", so the dates covered are 1900-01-01 to 2100-01-03.
#'
#' @format
#' A [data.table::data.table] with one row per ISO yearweek and the following
#' columns:
#' \describe{
#' \item{isoyear}{Integer. ISO year.}
#' \item{isoyearweek}{Character. ISO yearweek, "yyyy-ww".}
#' \item{mon}{Date of Monday.}
#' \item{tue}{Date of Tuesday.}
#' \item{wed}{Date of Wednesday.}
#' \item{thu}{Date of Thursday.}
#' \item{fri}{Date of Friday.}
#' \item{sat}{Date of Saturday.}
#' \item{sun}{Date of Sunday.}
#' \item{weekdays}{List column. The dates Monday to Friday.}
#' \item{weekend}{List column. The dates Saturday and Sunday.}
#' \item{days}{List column. The dates Monday to Sunday.}
#' }
#' @source Generated from a daily date sequence using ISO 8601 week rules. See
#'   `data-raw/1_gen-data.R` in the package source.
#' @examples
#' library(data.table)
#' # Look up the Monday and Sunday of two ISO yearweeks
#' dates_by_isoyearweek[isoyearweek %in% c("2021-01", "2021-02"), .(isoyearweek, mon, sun)]
#'
#' # Constructing a vector of dates without removing the Date class
#' do.call("c", dates_by_isoyearweek[isoyearweek %in% c("2021-01", "2021-02")]$weekdays)
"dates_by_isoyearweek"

#' Norwegian workdays and holidays by date
#'
#' A daily calendar flagging Norwegian public holidays, weekends and workdays
#' for each date in the covered range.
#'
#' @format
#' A [data.table::data.table] with one row per date and the following columns:
#' \describe{
#' \item{date}{Date.}
#' \item{day_of_week}{Integer. 1 = Monday, 7 = Sunday.}
#' \item{mon_to_fri}{Integer. 1 if Monday to Friday, 0 if Saturday or Sunday.}
#' \item{sat_to_sun}{Integer. 1 if Saturday or Sunday, 0 if Monday to Friday.}
#' \item{public_holiday}{Integer. 1 if a public holiday (helligdag), 0 otherwise.}
#' \item{freeday}{Integer. 1 if a public holiday or Saturday/Sunday, 0 otherwise.}
#' \item{workday}{Integer. 1 if `freeday` is 0, 0 if `freeday` is 1.}
#' }
#' @source Derived from the Norwegian public-holiday list in
#'   `inst/rawdata/norway_holidays.xlsx`. See `data-raw/1_gen-data.R` in the
#'   package source.
#' @examples
#' head(nor_workdays_by_date)
#'
#' # Count the workdays in the dataset
#' sum(nor_workdays_by_date$workday)
"nor_workdays_by_date"

#' Norwegian workdays and holidays by ISO yearweek
#'
#' For each complete (7-day) ISO yearweek, the proportion of days that are
#' public holidays, free days and workdays in Norway. The proportions are stored
#' rounded to two decimal places, so 5 workdays in 7 is 0.71 rather than 5/7.
#'
#' @format
#' A [data.table::data.table] with one row per ISO yearweek and the following
#' columns:
#' \describe{
#' \item{isoyearweek}{Character. ISO yearweek, "yyyy-ww".}
#' \item{public_holiday}{The proportion of days in the ISO yearweek that are public holidays, rounded to two decimal places.}
#' \item{freeday}{The proportion of days in the ISO yearweek that are public holidays or Saturday/Sunday, rounded to two decimal places.}
#' \item{workday}{1 minus `freeday`.}
#' }
#' @source Aggregated from [nor_workdays_by_date]. See `data-raw/1_gen-data.R`
#'   in the package source.
#' @examples
#' head(nor_workdays_by_isoyearweek)
#'
#' # ISO yearweeks that contain at least one public holiday
#' head(nor_workdays_by_isoyearweek[nor_workdays_by_isoyearweek$public_holiday > 0, ])
"nor_workdays_by_isoyearweek"
