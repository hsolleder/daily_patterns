#' Create raw_patterns object
#'
#' \code[raw_patterns] creates a raw_patterns object, essentially a `tibble` with a `birth_date` attribute.
#'
#' @param start_date Character vector (n), specifies the start date (and time, if start_time is not provided) of each
#'  interval.
#' @param end_date Character vector (n), specifies the end date (and time, if end_time is not provided) of each
#'  interval.
#' @param start_time Character vector (n), specifies the start time of each interval
#' @param end_time Character vector (n), specifies the end time of each interval
#' @param type Character vector (n), specifies the type (possibly NA) of each interval
#' @param quantity Numeric vector (n), specifies the quantity (possibly NA) of each interval
#' @param date_format String, specifies the format of start_date and end_date. Will be used by
#'  \code[clock::date_time_parse] to automatically parse the start and end dates.
#' @param time_zone String, specifies the time zone of the measurements. Will be used by \code[clock::date_time_parse]
#'  to automatically parse the start and end dates.
#' @param birth_date String, specifies the birth date. Must be in the same format as start_date and end_date
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @export
#' @seealso clock::date_time_parse
#'
#'
# Input can be *_date and *_time, or start_date directly
# TODO: take care of case where start_time and/or end_time are missing
raw_patterns <- function(
    start_date,
    end_date,
    ...,
    start_time = character(),
    end_time = character(),
    type = character(),
    quantity = numeric(),
    date_format = "%d/%m/%Y",
    time_zone = "Europe/Paris",
    birth_date = "01/01/1970") {

  assertthat::assert_that(rlang::is_character(start_date))
  assertthat::assert_that(rlang::is_character(end_date))
  assertthat::assert_that(length(start_date) == length(end_date))

  x <- tibble::tibble(start_date = start_date, end_date = end_date)

  if (length(start_time) > 0) {
    assertthat::assert_that(
      rlang::is_character(start_time),
      length(start_time) == length(start_date)
    )

    assertthat::assert_that(
      all(is.na(start_time) | stringr::str_detect(start_time, "^[:digit:]{1,2}:[:digit:]{1,2}:[:digit:]{1,2}$"))
    )

    x$start_time <- hms::parse_hms(start_time)
  }

  if (length(end_time) > 0) {
    assertthat::assert_that(
      rlang::is_character(end_time),
      length(end_time) == length(end_date)
    )

    assertthat::assert_that(
      all(is.na(end_time) | stringr::str_detect(end_time, "^[:digit:]{1,2}:[:digit:]{1,2}:[:digit:]{1,2}$"))
    )

    x$end_time <- hms::parse_hms(end_time)
  }

  if (length(type) > 0) {
    assertthat::assert_that(
      rlang::is_character(type),
      length(type) == length(start_date)
    )

    x$type <- type
  }

  if (length(quantity) > 0) {
    assertthat::assert_that(
      is.numeric(quantity),
      length(quantity) == length(start_date)
    )
    x$quantity <- quantity
  }

  birth_date <- clock::date_time_parse(birth_date, zone = time_zone, format = date_format)

  x <- prepare_data(x, date_format = date_format, time_zone = time_zone, birth_date = birth_date)

  validate_raw_patterns(new_raw_patterns(x, birth_date = birth_date))

}

# Internal representation as start_date, end_date
# TODO: add doc
new_raw_patterns <- function(
    x = tibble::tibble(start_date = lubridate::POSIXct(), end_date = lubridate::POSIXct()),
    birth_date = lubridate::POSIXct()) {

  assertthat::assert_that(inherits(x, "tbl_df"))
  check_required_colnames(x, c("start_date", "end_date"))
  assertthat::assert_that(inherits(x[["start_date"]], "POSIXct"), inherits(x[["end_date"]], "POSIXct"))
  assertthat::assert_that(inherits(birth_date, "POSIXct"))

  structure(
    x,
    class = c("raw_patterns", class(x)),
    birth_date = birth_date
  )

}

# TODO: add doc
# TODO: add validation checks
validate_raw_patterns <- function(x) {
  x
}

# TODO: add doc
prepare_data <- function(
    x,
    ...,
    date_format = "%d/%m/%Y",
    time_zone = "Europe/Paris",
    birth_date = "01/01/1970") {

  rlang::check_dots_empty()

  check_required_colnames(
    x, c("start_date", "start_time", "end_date", "end_time")
  )
  # time can be calculated from the information given
  # type, quantity are optional. If not provided, type will be set to "default"
  # while quantity will be set to NA_real_

  x <- x |>
    dplyr::select(
      all_of(c("start_date", "start_time", "end_date", "end_time")),
      any_of(c("type", "quantity"))
    ) |>
    dplyr::filter(complete.cases(start_date, end_date, start_time, end_time))

  # TODO: add input checks to verify that data is correctly formatted
  # TODO: add check that no element is above a certain length

  # Split elements spanning two days into two separate elements, with the first finishing at 23h59 and the second
  # starting at 00h00
  x <- x |>
    dplyr::mutate(n_repeats = 1 + (start_date != end_date)) |>
    tidyr::uncount(n_repeats, .remove = FALSE, .id = "id") |>
    dplyr::mutate(
      end_time = dplyr::if_else(
        id == 1 & n_repeats > 1,
        pmax(end_time, hms::hms(hours = 23, minutes = 59)),
        end_time
      ),
      end_date = dplyr::if_else(
        id == 1 & n_repeats > 1,
        start_date,
        end_date
      ),
      start_time = dplyr::if_else(
        id == 2 & n_repeats > 1,
        pmin(start_time, hms::hms(hours = 0, minutes = 0)),
        start_time
      ),
      start_date = dplyr::if_else(
        id == 2 & n_repeats > 1,
        end_date,
        start_date
      ),
      .keep = "unused"
    )

  # Convert dates and times to data and POSIXct
  # TODO: write function to facilitate modifying date objects
  # TODO: clean up this mess. It works, but it is ugly
  x <- x |>
    dplyr::mutate(dplyr::across(
      c(start_date, end_date),
      ~ clock::date_time_parse(.x, format = date_format, zone = time_zone)
    )) |>
    dplyr::mutate(dplyr::across(c(start_time, end_time), ~ as.POSIXct(.x))) |>
    dplyr::mutate(
      start_date = clock::set_hour(start_date, clock::get_hour(start_time), ambiguous = "latest"),
      start_date = clock::set_minute(start_date, clock::get_minute(start_time)),
      end_date = clock::set_hour(end_date, clock::get_hour(end_time), ambiguous = "latest"),
      end_date = clock::set_minute(end_date, clock::get_minute(end_time)),
      .keep = "unused"
    ) |>
    dplyr::mutate(duration = as.numeric(difftime(end_date, start_date, units = "hours")))

  return(x)
}
