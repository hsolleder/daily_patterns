#' Check presence of specific column names in data frame
#'
#' check_required_colnames enables the user to quickly check that all required
#' column names are present in the data frame. If this is not the case, the
#' function will throw an error and indicate which variables are missing.
#'
#' @param x Data frame, to be checked
#' @param required_names Character vector, specifies the names of the columns
#'  that are required
#' @param case_sensitive Boolean, specifies whether the check should be case
#'  sensitive
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @export
check_required_colnames <- function(x,
                                    required_names,
                                    ...,
                                    case_sensitive = TRUE) {
  
  rlang::check_dots_empty()
  x_names <- names(x)
  
  if (!case_sensitive) {
    x_names <- tolower(x_names)
    required_names <- tolower(required_names)
  }
  
  missing_names <- setdiff(required_names, x_names)
  if (length(missing_names) > 0) {
    stop(sprintf(
      "Variable(s) `%s` missing from x.",
      paste(missing_names, collapse = ", ")
    ))
  }
  
  return(invisible(NULL))
}