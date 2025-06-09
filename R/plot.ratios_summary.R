#' Plot Ratios Summary for Indicator Ratios Summary Object
#'
#' This function generates a bar plot for a `cd_ratios_summary` object,
#' displaying the calculated indicator ratios for each year. It allows
#' for visual comparison across years, showing how each indicator ratio
#' changes over time.
#'
#' @param x A `cd_ratios_summary` object created by the `calculate_ratios_summary` function.
#'   It should contain a `year` column and one or more columns with names starting with `"Ratio"`,
#'   representing the calculated indicator ratios.
#' @param ... Additional arguments passed to other methods (currently unused).
#'
#' @return A `ggplot` object representing a bar plot of indicator ratios by year.
#'
#' @details
#' This function provides a visual summary of indicator ratios, with each ratio displayed
#' as a bar for each year. The `Expected Ratio` row is included if available, allowing
#' for easy comparison of actual ratios against expected values. The bars are grouped
#' by year, with distinct colors representing each year for clear differentiation.
#'
#' @examples
#' \dontrun{
#' # Assuming `cd_ratios_summary` is the object returned by `calculate_ratios_summary`
#' plot(cd_ratios_summary)
#' }
#'
#' @export
plot.cd_ratios_summary <- function(x, ...) {
  year <- name <- value <- NULL

  plot_data <- x %>%
    select(year, starts_with("Ratio ")) %>%
    pivot_longer(cols = -year) %>%
    mutate(year = factor(year, levels = sort(unique(year))))

  # Generate color palette with unique colors for each combination of name and year
  # unique_names <- unique(plot_data$year)
  # colors <- c('darkgreen', 'darkgoldenrod3', 'firebrick4', 'springgreen3', 'darkolivegreen3', 'steelblue2')
  # color_mapping <- set_names(colors, unique_names)

  years <- x %>%
    distinct(year) %>%
    pull(year)
  base_colors <- c("darkgreen", "darkgoldenrod3", "firebrick4", "springgreen3", "darkolivegreen3", "steelblue2")

  extra_needed <- robust_max(c(0, length(years) - length(base_colors)))
  extra_colors <- if (extra_needed > 0) {
    scales::hue_pal()(extra_needed)
  } else {
    NULL
  }

  color_mapping <- c(base_colors, extra_colors)
  names(color_mapping) <- years

  plot_data %>%
    ggplot(aes(name, value, fill = year)) +
    geom_col(position = position_dodge2()) +
    scale_y_continuous(breaks = scales::pretty_breaks(6)) +
    scale_fill_manual(values = color_mapping) +
    labs(
      title = "Ratio of number of facility reported ANC1 to penta1, and penta1 to penta3 compared to expected ratios",
      x = NULL,
      y = NULL
    ) +
    cd_plot_theme() +
    theme(
      plot.title = element_text(size = 14, hjust = 0.5),
      panel.grid.major.y = element_line(colour = "lightblue1", linetype = "dashed"),
    )
}
