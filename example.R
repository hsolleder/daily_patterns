R.utils::sourceDirectory("R")

raw_data <- readr::read_tsv("data/example_data.tsv")

x <- raw_patterns(
  raw_data$start_date,
  raw_data$end_date,
  start_time = as.character(raw_data$start_time),
  end_time = as.character(raw_data$end_time),
  quantity = raw_data$quantity,
  type = raw_data$type,
  date_format = "%d/%m/%Y",
  time_zone = "Europe/Paris",
  birth_date = "01/04/2024"
)

types <- type_structure(Food = c("Breastfeeding", "Solid"), Sleep = c("Nap", "Night"))

y <- summarized_patterns(x, unit = "weeks", type_struct = types)

types <- type_structure(food = c("Breastfeeding", "Solid"), sleep = c("Nap", "Night"))

# TODO: add type/major_type to summarized_patterns directly

y <- y |> 
    dplyr::mutate(major_type = dplyr::case_match(
      type,
      c("Nap", "Night") ~ "Sleep",
      c("Solid", "Breastfeeding") ~ "Food"
    )
  ) |> 
  dplyr::mutate(
    type = factor(type, levels = c("Night", "Nap", "Solid", "Breastfeeding")),
    major_type = factor(major_type)
  )

base_colors <- ggsci::pal_nejm()(nlevels(y$type))

ggplot2::ggplot(
  y, 
  mapping = ggplot2::aes(x = days, fill = type, y = duration)
) + 
  ggplot2::geom_col() +
  ggplot2::theme_minimal() +
  ggplot2::facet_grid(~ major_type) +
  ggplot2::scale_y_continuous(limits = c(0,20)) +
  ggplot2::geom_smooth(ggplot2::aes(y = duration, colour = type), linewidth = 1.5) +
  ggplot2::scale_color_manual(values = shades::brightness(base_colors, 0.7)) +
  ggplot2::scale_fill_manual(values = shades::saturation(base_colors, shades::delta(-0.4)))

ggplot2::ggplot(
  y, 
  mapping = ggplot2::aes(x = age, color = type, y = duration)
) + 
  ggplot2::geom_point() +
  ggplot2::scale_color_manual(values = shades::brightness(base_colors, 0.7)) +
  ggplot2::scale_fill_manual(values = shades::saturation(base_colors, shades::delta(-0.4))) + 
  ggplot2::scale_y_continuous(limits = c(0,20)) +
  ggplot2::theme_minimal() +
  ggplot2::facet_grid(~ major_type) +
  ggplot2::geom_smooth(linewidth = 1.5, position = "jitter")


+ 
  ggplot2::geom_col() +
  ggplot2::theme_minimal() +
  ggplot2::facet_grid(~ major_type) +
  ggplot2::scale_y_continuous(limits = c(0,20)) +
  ggplot2::geom_smooth(ggplot2::aes(y = duration, colour = type), linewidth = 1.5) +
  ggplot2::scale_color_manual(values = shades::brightness(base_colors, 0.7)) +
  ggplot2::scale_fill_manual(values = shades::saturation(base_colors, shades::delta(-0.4)))

# TODO: let user decide whether to plot total or individual curves
 

# p <- ggplot2::ggplot(
#   daily_summary |> dplyr::filter(type %in% c("Food", "Sleep")), 
#   mapping = ggplot2::aes(x = day_index, fill = type)
# ) +
#   ggplot2::geom_col(ggplot2::aes(y = total), position = "dodge") +
#   ggplot2::geom_smooth(ggplot2::aes(y = moving_average, colour = type), linewidth = 1.5) + 
#   ggplot2::theme_minimal() +
#   ggplot2::labs(x = "Days since birth (n)", y = "Time (h)") +
#   ggplot2::scale_x_continuous(breaks = days_range) + 
#   ggplot2::scale_color_manual(values = shades::brightness(base_colors, 0.7)) +
#   ggplot2::scale_fill_manual(values = shades::saturation(base_colors, delta(-0.4)))

# TODO: display each type by major_type, using 
# https://stackoverflow.com/questions/27803710/ggplot2-divide-legend-into-two-columns-each-with-its-own-title
# ggplot2::ggplot(
#   y, 
#   mapping = ggplot2::aes(x = days, fill = type, y = duration)
# ) + 
#   ggplot2::geom_col() +
#   ggplot2::theme_minimal() + 
#   ggplot2::scale_fill_manual(values = c("white", "steelblue4", "black", "white", "white", "darkorange","green"), drop = FALSE) +
#   ggplot2::facet_grid(~ major_type) +
#   ggplot2::scale_y_continuous(limits = c(0,20)) + 
#   ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1)) +
#   ggplot2::theme(
#     legend.position = "right", 
#     legend.key = ggplot2::element_rect(fill = NA)
#   )

# scale_fill_manual(values=c("white",hcl(seq(15,325,length.out=5), 100, 65)[1:2], 
#                            "white","white",
#                            hcl(seq(15,325,length.out=5), 100, 65)[3:5]),
#                   drop=FALSE) +
#   guides(fill=guide_legend(ncol=1)) +
#   theme(legend.position="bottom", 
#         legend.key = element_rect(fill=NA),
#         legend.title=element_blank())

# 
# p <- ggplot2::ggplot(
#   y,
#   mapping = ggplot2::aes(x = days, fill = type, group = major_type, y = duration)
# ) +
#   ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.9)) +
#   ggplot2::theme_minimal() +
#   ggplot2::scale_fill_manual(values = c("steelblue4", "black", "darkorange","green" )) +
#   ggplot2::scale_y_continuous(limits = c(0,20))
# 
# p <- ggplot2::ggplot(
#   y,
#   mapping = ggplot2::aes(x = test, fill = type, group = major_type, y = duration)
# ) +
#   ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.9)) +
#   ggplot2::theme_minimal() +
#   ggplot2::scale_fill_manual(values = c("steelblue4", "black", "darkorange","green" )) +
#   ggplot2::scale_y_continuous(limits = c(0,20)) +
#   ggplot2::facet_grid(~ days)
# 
# p <- ggplot2::ggplot(
#   y,
#   mapping = ggplot2::aes(x = days, fill = type, group = major_type, y = duration)
# ) +
#   ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.9)) +
#   ggplot2::theme_minimal() +
#   ggplot2::scale_fill_manual(values = c("steelblue4", "black", "darkorange","green" )) +
#   ggplot2::scale_y_continuous(limits = c(0,20))
# 
# p <- ggplot2::ggplot(
#   y,
#   mapping = ggplot2::aes(x = days, fill = type, group = major_type, y = duration)
# ) +
#   ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.9)) +
#   ggplot2::theme_minimal() +
#   ggplot2::scale_fill_manual(values = c("steelblue4", "black", "darkorange","green" )) +
#   ggplot2::scale_y_continuous(limits = c(0,20))
# 
# z <- tibble::tibble(type = y$type, major_type = y$major_type, duration = y$duration, days = y$days)
# 
# p <- ggplot2::ggplot(
#   z,
#   mapping = ggplot2::aes(x = days, fill = type, group = major_type, y = duration)
# ) +
#   ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.9)) +
#   ggplot2::theme_minimal() +
#   ggplot2::scale_fill_manual(values = c("steelblue4", "black", "darkorange","green" )) +
#   ggplot2::scale_y_continuous(limits = c(0,20))
# 
# # p <- ggplot2::ggplot(
# #   y |> dplyr::select(days, type, major_type, duration), 
# #   mapping = ggplot2::aes(x = days, fill = type, y = duration)
# # ) + 
# #   ggplot2::geom_col(ggplot2::aes(group = major_type), position = "dodge") +
# #   ggplot2::theme_minimal() + 
# #   ggplot2::scale_fill_manual(values = c("steelblue4", "black", "darkorange","green" )) +
# #   ggplot2::facet_grid(~ major_type) +
# #   ggplot2::scale_y_continuous(limits = c(0,20))

ggplot2::ggplot(
  y, 
  mapping = ggplot2::aes(x = days, fill = type, y = duration)
) + 
  ggplot2::geom_col() +
  ggplot2::theme_minimal() + 
  ggplot2::scale_fill_manual(values = c("steelblue4", "black", "darkorange","green" )) +
  ggplot2::facet_grid(~ major_type) +
  ggplot2::scale_y_continuous(limits = c(0,20))


plot_daily_summary

p <- ggplot2::ggplot(
  daily_summary |> dplyr::filter(type %in% c("Food", "Sleep")), 
  mapping = ggplot2::aes(x = day_index, fill = type)
) +
  ggplot2::geom_col(ggplot2::aes(y = total), position = "dodge") +
  ggplot2::geom_smooth(ggplot2::aes(y = moving_average, colour = type), linewidth = 1.5) + 
  ggplot2::theme_minimal() +
  ggplot2::labs(x = "Days since birth (n)", y = "Time (h)") +
  ggplot2::scale_x_continuous(breaks = days_range) + 
  ggplot2::scale_color_manual(values = shades::brightness(base_colors, 0.7)) +
  ggplot2::scale_fill_manual(values = shades::saturation(base_colors, delta(-0.4)))