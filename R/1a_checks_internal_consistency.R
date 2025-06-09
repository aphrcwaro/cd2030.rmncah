#' Plot Comparison with Linear Fit and R-squared
#'
#' Creates a scatter plot to compare two indicators over multiple years, including
#' a linear regression line and a diagonal reference line. The plot is faceted by year,
#' and displays R-squared values for each year to assess the relationship between indicators.
#'
#' @param .data A `cd_data` object containing merged data for the specified indicators.
#' @param x_var Character. Name of the variable to plot on the x-axis (e.g., 'anc1').
#' @param y_var Character. Name of the variable to plot on the y-axis (e.g., 'penta1').
#' @param title Character. The main title for the plot. Defaults to a title based on `x_var` and `y_var`.
#' @param x_label Character. Label for the x-axis. Defaults to the value of `x_var`.
#' @param y_label Character. Label for the y-axis. Defaults to the value of `y_var`.
#' @param call The calling environment.
#' @param ... Additional arguments for customization, such as `size`, `color`, or `linetype`, for finer control over plot appearance.
#'
#' @details
#' This function calculates R-squared values for each year and displays them in each facet,
#' helping assess the relationship between the two indicators over time. The diagonal reference line
#' is added to aid in visualizing deviations from a perfect 1:1 relationship.
#'
#' Common indicator comparisons include:
#' - ANC1 vs PENTA1
#' - PENTA1 vs PENTA3
#' - OPV1 vs OPV3
#'
#' @return A ggplot2 object showing the comparison plot of two indicators with linear regression and R-squared values.
#'
#' @examples
#' \dontrun{
#' plot_comparison(cd_data, x_var = "anc1", y_var = "penta1")
#' }
#'
#' @export
plot_comparison.cd_data <- function(.data, x_var, y_var, title = NULL, x_label = NULL, y_label = NULL,
                                    call = caller_env(), ...) {
  district <- year <- min_x <- max_x <- r_squared <- NULL

  check_cd_data(.data)
  check_required(x_var)
  check_required(y_var)

  # Check if specified indicators exist in the data
  if (!(x_var %in% names(.data)) || !(y_var %in% names(.data))) {
    cd_abort(c("x" = "Specified indicators do not exist in {.arg .arg}. Please check variable names."))
  }

  # Filter data and calculate yearly sums
  data_filtered <- .data %>%
    summarize(
      across(all_of(c(x_var, y_var)), sum, na.rm = TRUE),
      .by = c(district, year)
    ) %>%
    # Add min and max for the diagonal line
    mutate(
      min_x = min(!!sym(x_var), na.rm = TRUE),
      max_x = robust_max(!!sym(x_var)),
      .by = year
    )

  # Calculate R-squared values by year
  r_squared_value <- data_filtered %>%
    summarize(
      r_squared = round(summary(stats::lm(stats::as.formula(paste(y_var, "~", x_var)), data = cur_data()))$r.squared, 4)
    ) %>%
    pull(r_squared)


  # Set default title and labels if not provided
  title <- title %||% paste("Comparison of", x_var, "and", y_var, "by Year")
  x_label <- x_label %||% x_var
  y_label <- y_label %||% y_var

  # Create the plot
  plot <- data_filtered %>%
    ggplot(aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_point(aes(colour = "District"), size = 1.5) +
    geom_smooth(aes(color = "Linear fit"), method = "lm", formula = y ~ x, linetype = "solid", se = FALSE, size = 0.8) +
    geom_segment(aes(x = min_x, y = min_x, xend = max_x, yend = max_x, colour = "Diagonale"),
      linetype = "dashed", size = 0.8
    ) +
    facet_wrap(~year, scales = "free_y", ncol = 3) +
    labs(x = x_label, y = y_label, title = title, caption = paste("R-squared:", r_squared_value)) +
    scale_x_continuous(labels = scales::label_number()) +
    scale_y_continuous(labels = scales::label_number()) +
    cd_plot_theme() +
    scale_color_manual(
      values = c("District" = "navy", "Linear fit" = "black", "Diagonale" = "red"),
      breaks = c("District", "Linear fit", "Diagonale")
    )

  return(plot)
}

#' Internal Consistency Plot Functions for Indicator Comparisons
#'
#' These functions generate scatter plots that compare two health indicators across years, assessing internal consistency
#' through visualization of linear regression fits and R-squared values by year. Each function is designed to compare a specific
#' pair of indicators, with options for customizing plot titles, axis labels, and the inclusion of trend information.
#'
#' @param .data A data frame of class `cd_data` that includes the relevant indicators for comparison.
#' @param x_var Character. The name of the indicator variable to plot on the x-axis.
#' @param y_var Character. The name of the indicator variable to plot on the y-axis.
#' @param title Character. The title for the plot. Defaults to a title based on `x_var` and `y_var`.
#' @param x_label Character. Label for the x-axis. Defaults to `x_var`.
#' @param y_label Character. Label for the y-axis. Defaults to `y_var`.
#' @param call The calling environment, used for error handling. Default is `caller_env()`.
#' @param ... Additional parameters for further customization, such as point size, line type, or color.
#'
#' @details
#' - The scatter plot displays the relationship between the two specified indicators over multiple years,
#'   with each year presented in a separate facet.
#' - A linear regression line is added to each year's plot to help visualize the trend.
#' - R-squared values, calculated per year, are optionally displayed on each facet, indicating the strength
#'   of the linear relationship between the indicators for that year.
#' - A diagonal reference line is also provided for visual comparison to a 1:1 relationship, where applicable.
#'
#' This function is commonly used for consistency checks between indicators such as:
#' - ANC1 vs PENTA1
#' - PENTA1 vs PENTA3
#' - OPV1 vs OPV3
#'
#' @return A `ggplot2` object, comprising:
#' - A scatter plot comparing two specified indicators over multiple years.
#' - A linear regression line for each year, indicating the trend between the indicators.
#' - Optional display of R-squared values per facet, summarizing the correlation strength.
#' - Faceting by year to facilitate a detailed comparison of trends across years.
#'
#' Users can render this `ggplot2` object directly, save it, or further modify it using the `+` operator.
#'
#' @examples
#' \dontrun{
#' # Plot a comparison between ANC1 and PENTA1 indicators
#' plot_comparison(cd_data, x_var = "anc1", y_var = "penta1")
#'
#' # Dedicated function for comparing ANC1 and PENTA1 over time
#' plot_comparison_penta1_anc1(cd_data)
#' }
#'
#' @rdname internal_consistency
#' @export
plot_comparison <- function(.data, x_var, y_var, title = NULL, x_label = NULL, y_label = NULL,
                            call = caller_env(), ...) {
  check_cd_data(.data, call = call)
  check_required(x_var, call = call)
  check_required(y_var, call = call)
  UseMethod("plot_comparison")
}

#' Plot Comparison of Penta1 and ANC1
#'
#' `plot_comparison_penta1_anc1` compares Penta1 and ANC1 indicators across.
#'
#' @rdname internal_consistency
#'
#' @export
plot_comparison_anc1_penta1 <- function(.data) {
  .data %>%
    plot_comparison(
      x_var = "anc1",
      y_var = "penta1",
      title = "Comparison of numbers of ANC1 and Penta1 by year",
      x_label = "ANC1",
      y_label = "Penta1"
    )
}

#' Plot Comparison of Penta1 and Penta3
#'
#' `plot_comparison_penta1_penta3` compares Penta1 and Penta23 indicators.
#'
#' @rdname internal_consistency
#'
#' @export
plot_comparison_penta1_penta3 <- function(.data) {
  .data %>%
    plot_comparison(
      x_var = "penta1",
      y_var = "penta3",
      title = "Comparison of numbers of Penta1 and Penta3 by year",
      x_label = "Penta1",
      y_label = "Penta3"
    )
}

#' Plot Comparison of OPV1 and OPV3
#'
#' `plot_comparison_opv1_opv3` compares OPV1 and OPV3 indicators.
#'
#' @rdname internal_consistency
#'
#' @export
plot_comparison_opv1_opv3 <- function(.data) {
  .data %>%
    plot_comparison(
      x_var = "opv1",
      y_var = "opv3",
      title = "Comparison of numbers of opv1 and opv3 by year",
      x_label = "OPV1",
      y_label = "OPV3"
    )
}
