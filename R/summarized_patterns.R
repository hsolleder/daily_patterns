summarized_patterns <- function(x, ..., unit = c("days", "weeks", "months"), type_struct = NULL) {
  rlang::check_dots_empty()
  assertthat::assert_that(inherits(x, "raw_patterns"))
  assertthat::assert_that(rlang::is_null(type_struct) || inherits(type_struct, "type_structure"))
  
  if (rlang::is_null(type_struct)) {
    type_struct <- type_structure(!!!unique(x$type))
  }
  
  type_df <- tibble::enframe(type_struct, name = "type", value = "subtype") |> 
    tidyr::unnest(cols = c(subtype)) 
  
  x <- x |> 
    dplyr::rename(subtype = type) |> 
    dplyr::left_join(type_df, by = dplyr::join_by(subtype))
  
  x <- x |>
    dplyr::mutate(
      days = get_age(x, unit = "days"),
      age = get_age(x, unit = unit)
    ) |>
    dplyr::group_by(age, days, type, subtype) |>
    dplyr::summarize(
      dplyr::across(
        tidyr::any_of(c("duration", "quantity")),
        ~ sum(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    )

  class(x)[which(class(x) == "raw_patterns")] <- "summarized_patterns"

  structure(
    x,
    class = c("summarized_patterns", setdiff(class(x), "raw_patterns")),
    unit = unit
  )
}

#' Create type structure
#' 
#' type_structure enables the user to create an object of class `type_structure`, which is used to structure the data 
#' and figures. Such an object corresponds to a named list with 
#'
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples 
#' type_structure(food = c("Breastfeeding", "Solid"), sleep = c("Nap", "Night"))
#' type_structure("Breastfeeding", sleep = c("Nap", "Night"))
type_structure <- function(...) {

  dots <- rlang::list2(...)
  
  # This check should also enforce that the depth (purrr::pluck_depth) is 1
  assertthat::assert_that(purrr::every(dots, rlang::is_character))

  if (!rlang::is_null(names(dots))) {
    # Check that unnamed elements are of length 1
    unnamed_dots <- which(names(dots) == "")
    if (length(unnamed_dots) > 0) {
      assertthat::assert_that(purrr::map_lgl(unnamed_dots, \(k) length(dots[[k]]) == 1))
      names(dots)[unnamed_dots] <- purrr::list_c(dots[unnamed_dots])
    }
  } else {

    # If no named argument is passed to type_structure, calling names(dots) returns NULL
    names(dots) <- purrr::list_c(dots)
  }

  
  #TODO: add check that each type is non-intersecting
  
  structure(dots, class = c("type_structure", class(dots)))
  
}


plot.summarized_patterns <- function(x) {
  
  p <- ggplot2::ggplot(
    x,
    mapping = ggplot2::aes(x = age, color = type, y = duration)
  ) +
    ggplot2::geom_point() +
    ggplot2::scale_color_manual(values = shades::brightness(base_colors, 0.7)) +
    ggplot2::scale_fill_manual(values = shades::saturation(base_colors, shades::delta(-0.4))) +
    ggplot2::theme_minimal() +
    ggplot2::geom_smooth(linewidth = 1.5, position = "jitter") +
    ggplot2::scale_x_continuous(breaks = x$age, name = sprintf("Age (%s)", attr(x, "unit"))) +
    ggplot2::scale_y_continuous(name = "Duration (h)")
  
  # p <- ggplot2::ggplot(
  #   x,
  #   mapping = ggplot2::aes(x = age, color = type, y = duration)
  # ) +
  #   ggplot2::geom_point() +
  #   ggplot2::scale_color_manual(values = shades::brightness(base_colors, 0.7)) +
  #   ggplot2::scale_fill_manual(values = shades::saturation(base_colors, shades::delta(-0.4))) +
  #   ggplot2::theme_minimal() +
  #   ggplot2::facet_grid(~ major_type) +
  #   ggplot2::geom_smooth(linewidth = 1.5, position = "jitter")
  
  p
  
}
