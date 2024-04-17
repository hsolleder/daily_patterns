#' Get number of time units since birth
#'
#' \code[get_age] calculates the number of days, weeks or months between each row of the raw_patterns object and the
#' birth date. For the first day, week or month, the age is considered to be 0 days, weeks or months.
#'
#' @param x `raw_patterns` object
#' @param unit String, specifies the unit of time for which the age should be calculated
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @export
#'
get_age <- function(x, ..., unit = c("days", "weeks", "months")) {
  assertthat::assert_that(inherits(x, "raw_patterns"))
  unit <- rlang::arg_match(unit)

  switch(
    unit,
    days = floor(as.numeric(difftime(x$start_date, attr(x, "birth_date")), units = "days")),
    weeks = floor(as.numeric(difftime(x$start_date, attr(x, "birth_date")), units = "weeks")),
    months = get_age_months(x)
  )
}


# Helpers -----------------------------------------------------------------

get_age_months <- function(x) {
  end_date <- x$start_date
  start_date <- attr(x, "birth_date")

  results <- 12 * (clock::get_year(end_date) - clock::get_year(start_date))
  results <- results + clock::get_month(end_date) - clock::get_month(start_date)
  results <- results - (clock::get_day(end_date) < clock::get_day(start_date)) # Subtract one as equivalent to floor
  results
}
