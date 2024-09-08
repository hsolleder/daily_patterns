## code to prepare `DATASET` dataset goes here

n_days <- 120

n_sleep <- c(8, .5)
cycle_duration <- c(45, 5)
n_cycles_night <- c(10, .5)
p_nap_cycles <- c(0.5, 0.3, 0.2)
n_eat <- c(8, 2)
eat_duration <- c(10, 3)
wake_duration <- c(70, 10)

simulate_day <- function(n_sleep, cycle_duration, wake_duration, n_cycles_night, n_eat, eat_duration) {

  n_night_cycles <- round(rnorm(
    1, mean = n_cycles_night[1], sd = n_cycles_night[2]
  ))

  night_duration <- sum(rnorm(
    n_night_cycles,
    mean = cycle_duration[1],
    sd = cycle_duration[2]
  ))

  sleep_start <- 0
  sleep_end <- night_duration

  # Does not work completely, if the last waking-up is quite late, then the
  # next sleep phase will go overboard.
  while (sleep_end[length(sleep_end)] < 1439) {

    wake_time <- rnorm(1, wake_duration[1], wake_duration[2])

    sleep_start <- c(sleep_start, sleep_end[length(sleep_end)] + wake_time)

    n_nap_cycles <- rbinom(1, length(p_nap_cycles), p_nap_cycles)
    sleep_time <- sum(rnorm(
      n_nap_cycles, mean = cycle_duration[1], sd = cycle_duration[2]
    ))

    sleep_end <- c(sleep_end, sleep_start[length(sleep_start)] + sleep_time)

  }

  sleep_end[length(sleep_end)] <- min(1439, sleep_end[length(sleep_end)])

  day <- tibble::tibble(
    start_time = format(as.POSIXlt(sleep_start * 60, tz = "GMT"), "%H:%M"),
    end_time = format(as.POSIXlt(sleep_end * 60, tz = "GMT"), "%H:%M")
  )

}


usethis::use_data(DATASET, overwrite = TRUE)
