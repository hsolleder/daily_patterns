plot_daily_summary <- function(x, ..., ignore_first = FALSE, ignore_last = FALSE) {
  
  UseMethod("plot_daily_summary")
  
}

plot_daily_summary.raw_patterns <- function(
    x, ..., ignore_first = FALSE, ignore_last = FALSE, unit = c("days", "weeks", "months")) {
  
  x <- summarized_patterns(x, unit = unit)
  plot_daily_summary(x, ignore_first = ignore_first, ignore_last = ignore_last)
  
}

plot_daily_summary.summarized_patterns <- function(x, ..., ignore_first = FALSE, ignore_last = FALSE) {
  
  assertthat::assert_that(inherits(x, "summarized_patterns"))
  
  print(class(x))
  
}
