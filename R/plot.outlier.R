#' Visualize Summary of Outlier Detection
#'
#' Plots annual trends or heat maps of non-outlier rates for immunization indicators
#' at subnational levels.
#'
#' @param x A `cd_outlier` object with precomputed outlier flags.
#' @param selection_type One of `"region"`, `"indicator"`, or `"heat_map"`:
#'   - `"region"`: Non-outlier rates by year and region.
#'   - `"indicator"`: Yearly average non-outlier rate per indicator.
#'   - `"heat_map"`: Year-by-unit heat map of all or selected indicators.
#' @param indicator Optional. Specific indicator name (e.g., `"penta3"`). Required
#'    for `"region"` view.
#' @param ... Not used.
#'
#' @details
#' - Values are assumed to be percentages of non-outliers (i.e., 100 = no outliers).
#' - `"region"` and `"indicator"` use bar plots with gradient fill.
#' - `"heat_map"` shows indicator values by year and region.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' \dontrun{
#' plot(outlier_data, selection_type = "region", indicator = "penta3")
#' plot(outlier_data, selection_type = "heat_map")
#' }
#' @export
plot.cd_outlier <- function(x,
                            selection_type = c("region", "indicator", "heat_map"),
                            indicator = NULL,
                            ...) {

  admin_level <- attr_or_abort(x, "admin_level")
  region <- attr_or_null(x, 'region')
  admin_level_col <- get_plot_admin_column(admin_level, region)

  indicator <- if (is.null(indicator) || indicator == "") {
    NULL
  } else {
    arg_match(indicator, get_all_indicators())
  }

  selection_type <- arg_match(selection_type)

  if (selection_type %in% c("region", "indicator")) {
    data_prepared <- if (selection_type == "region") {
      if (is.null(indicator) || indicator == '') return(NULL)
      x %>%
        mutate(
          category = !!sym(admin_level_col),
          value = !!sym(paste0(indicator, "_outlier5std"))
        )
    } else {
      # TODO: To review later
      # cols <- intersect(c("year", "category"), names(x))
      x %>%
        pivot_longer(
          cols = ends_with("_outlier5std"),
          names_to = "category",
          names_pattern = "^(.*)_outlier5std"
        ) %>%
        summarise(value = mean(value, na.rm = TRUE), .by = c(year, category))
    }

    min_rr <- min(data_prepared$value, na.rm = TRUE)
    low_threshold <- ifelse(min_rr < 80, min_rr, 70)
    breaks_vals <- c(low_threshold, 70, 80, 90, 100)

    ggplot(data_prepared, aes(x = factor(year), y = value, fill = value)) +
      geom_col() +
      facet_wrap(~category) +
      labs(
        title = paste("Percent Non-Outliers by Year and", str_to_title(selection_type)),
        x = "Year", y = "% Non-Outliers", fill = "% Non-Outliers"
      ) +
      scale_fill_gradientn(
        colors = c("red", "red", "orange", "yellowgreen", "forestgreen"),
        values = scales::rescale(breaks_vals),
        limits = c(low_threshold, 100)
      ) +
      theme_minimal() +
      theme(
        panel.grid.major = element_line(color = "gray95"),
        axis.ticks = element_blank(),
        strip.background = element_blank()
      )
  } else if (selection_type == "heat_map") {
    if (is.null(indicator)) {
      x <- x %>%
        summarise(across(ends_with('_outlier5std'), ~ round(mean(.x, na.rm = TRUE))), .by = all_of(admin_level_col)) %>%
        pivot_longer(cols = ends_with("_outlier5std"), names_to = "indicator") %>%
        mutate(indicator = str_remove(indicator, "_outlier5std"))

      ggplot(x, aes(x = !!sym(admin_level_col), y = indicator, fill = value)) +
        geom_tile(color = "white") +
        geom_text(aes(label = value), color = "black", size = 3, vjust = 0.5) +
        scale_fill_gradient2(low = "red3", mid = "orange", high = "forestgreen", midpoint = 80) +
        labs(x = admin_level_col, y = "Indicator", fill = "Value") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))
    } else {
      column_name <- paste0(indicator, "_outlier5std")
      ggplot(x, aes(x = !!sym(admin_level_col), y = factor(year), fill = !!sym(column_name))) +
        geom_tile(color = "white") +
        geom_text(aes(label = !!sym(column_name)), color = "black", size = 3, vjust = 0.5) +
        scale_fill_gradient2(low = "red3", mid = "orange", high = "forestgreen", midpoint = 80) +
        labs(x = admin_level_col, y = "Year", fill = paste0(indicator, " Value")) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))
    }
  }
}

#' Plot Outlier Time Series for a Region
#'
#' Displays a time-series plot of one indicator for a single region or district,
#' with outlier highlights.
#'
#' @param x A `cd_outlier_list` object from `list_outlier_units()`.
#' @param region_name The name of the unit to plot.
#' @param ... Not used.
#'
#' @details
#' - Plots observed values, median trend, and 5×MAD range.
#' - Flags outliers in red.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' \dontrun{
#' list_outlier_units(cd_data, "penta1") %>%
#'   plot(region_name = "Nakuru")
#' }
#' @export
plot.cd_outlier_list <- function(x, region_name = NULL, ...) {

  admin_level <- attr_or_abort(x, "admin_level")
  indicator <- attr_or_abort(x, "indicator")
  region <- attr_or_null(x, 'region')

  if (is.null(region_name) || !is_scalar_character(region_name)) {
    cd_abort(c('x' = "{.arg region} must be a scalar string"))
  }

  admin_level_col <- get_plot_admin_column(admin_level, region)

  med <- paste0(indicator, "_med")
  mad <- paste0(indicator, "_mad")

  year = robust_max(x$year)

  x %>%
    filter(!!sym(admin_level_col) == region_name) %>%
    mutate(
      date = ym(paste0(year, month, sep = "-")),
      upper_bound = !!sym(med) + !!sym(mad) * 5,
      lower_bound = !!sym(med) - !!sym(mad) * 5,
      outlier_flag = !!sym(indicator) > upper_bound | !!sym(indicator) < lower_bound
    ) %>%
    ggplot(aes(date)) +
      geom_line(aes(y = !!sym(indicator)), colour = "forestgreen") +
      geom_point(aes(y = !!sym(indicator)), colour = "forestgreen") +
      geom_line(aes(y = !!sym(med)), colour = "cyan", linetype = "dashed") +
      geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "gray80", alpha = 0.5) +
      geom_point(
        data = function(df) filter(df, outlier_flag),
        aes(y = !!sym(indicator)), color = "red", size = 2
      ) +
      labs(
        title = str_glue('{indicator} trend for {region_name} in {year}'),
        y = indicator,
        x = "Month"
      ) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
      scale_x_date(date_breaks = "1 months", date_labels = "%b") +
      cd_plot_theme() +
      # theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 16)
      )
}
