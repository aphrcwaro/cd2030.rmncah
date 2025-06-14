check_file_path <- function(path, call = caller_env()) {
  check_required(.data, call = call)
  # Validate if the file exists
  if (!file.exists(path)) {
    cd_abort(
      c("x" = "The specified file {.val {path}} does not exist. Please provide a valid file path."),
      call = call
    )
  }

  invisible(TRUE)
}

check_cd_class <- function(.data,
                           expected_class,
                           arg = caller_arg(.data),
                           call = caller_env()) {
  check_required(.data, arg = arg, call = call)

  if (!inherits(.data, expected_class)) {
    cd_abort(
      c("x" = "The data object must be of class {.cls {expected_class}}."),
      call = call
    )
  }

  invisible(TRUE)
}


check_cd_population_metrics <- function(.data, arg = caller_arg(.data), call = caller_env()) {
  check_cd_class(.data, "cd_population_metrics", arg = arg, call = call)
}

check_cd_indicator_coverage <- function(.data, arg = caller_arg(.data), call = caller_env()) {
  check_cd_class(.data, "cd_indicator_coverage", arg = arg, call = call)
}

check_cd_population <- function(.data, arg = caller_arg(.data), call = caller_env()) {
  check_cd_class(.data, "cd_population", arg = arg, call = call)
}

check_cd_reporting_rate <- function(.data, arg = caller_arg(.data), call = caller_env()) {
  check_cd_class(.data, "cd_average_reporting_rate", arg = arg, call = call)
}

check_cd_data <- function(.data, arg = caller_arg(.data), call = caller_env()) {
  check_cd_class(.data, "cd_data", arg = arg, call = call)
}

check_cd_fpet <- function(.data, arg = caller_arg(.data), call = caller_env()) {
  check_cd_class(.data, "cd_fpet", arg = arg, call = call)
}

check_cd_mapping <- function(.data, arg = caller_arg(.data), call = caller_env()) {
  check_cd_class(.data, "cd_mapping", arg = arg, call = call)
}

check_cd_coverage <- function(.data, arg = caller_arg(.data), call = caller_env()) {
  check_cd_class(.data, "cd_coverage", arg = arg, call = call)
}

#' Validate UN Estimates Data for Population Metrics
#'
#' Ensures the provided UN estimates data is valid and appropriate for the selected
#' administrative level.
#'
#' @param .data A tibble containing UN estimates data or `NULL`.
#' @param admin_level Character. Specifies the administrative level for aggregation.
#'   Must be one of `"national"`, `"adminlevel_1"`, or `"district"`.
#' @param call The calling environment for error messages.
#'
#' @return Invisible `NULL`. Throws an error if the validation fails.
#' @noRd
check_un_estimates_data <- function(.data = NULL,
                                    admin_level = c("national", "adminlevel_1", "district"),
                                    arg = caller_arg(.data),
                                    call = caller_env()) {
  admin_level <- arg_match(admin_level)
  if (admin_level == "national") {
    if (is.null(.data)) {
      cd_abort(c("x" = "{.arg un_estimate} must be provided for {.val national} metrics."), call = call)
    }
    check_cd_class(.data, "cd_un_estimates", arg = arg, call = call)
  }

  invisible(TRUE)
}

check_un_mortality_data <- function(.data, arg = caller_arg(.data), call = caller_env()) {
  check_cd_class(.data, "cd_un_mortality", arg = arg, call = call)
}

check_fpet_data <- function(.data, arg = caller_arg(.data), call = caller_env()) {
  check_cd_class(.data, "cd_fpet_data", arg = arg, call = call)
}

check_wuenic_data <- function(.data, arg = caller_arg(.data), call = caller_env()) {
  check_cd_class(.data, "cd_wuenic_data", arg = arg, call = call)

  if (!all(c("iso", "year") %in% colnames(.data))) {
    cd_abort(c("x" = "WUENIC data must contain {.field iso} and {.field year} columns."), call = call)
  }

  invisible(TRUE)
}

check_survey_data <- function(.data,
                              admin_level = c("national", "adminlevel_1", "district"),
                              arg = caller_arg(.data),
                              call = caller_env()) {
  check_cd_class(.data, "cd_survey_data", arg = arg, call = call)

  admin_level <- arg_match(admin_level)
  if (admin_level == "national" && "adminlevel_1" %in% colnames(.data)) {
    cd_abort(c("x" = "Regional survey data used in national level"), call = call)
  } else if (admin_level != "national" && !"adminlevel_1" %in% colnames(.data)) {
    cd_abort(c("x" = "National survey data used in subnational level"), call = call)
  }

  if (!all(c("iso3", "year") %in% colnames(.data))) {
    cd_abort(c("x" = "Survey data must contain {.field iso} and {.field year} columns."), call = call)
  }

  invisible(TRUE)
}

check_equity_data <- function(.data, arg = caller_arg(.data), call = caller_env()) {
  check_cd_class(.data, "cd_equity_data", arg = arg, call = call)
}

check_service_utilization <- function(.data, arg = caller_arg(.data), call = caller_env()) {
  check_cd_class(.data, "cd_service_utilization", arg = arg, call = call)
}

check_un_mortality <- function(.data, arg = caller_arg(.data), call = caller_env()) {
  check_cd_class(.data, "cd_un_mortality", arg = arg, call = call)
}

check_ratio_pairs <- function(.list, arg = call_args(.list), call = caller_env()) {
  check_required(.data, call = call)

  # Check that ratio_pairs is a named list with each element as a character vector of length 2
  is_ratio_pairs <- all(
    is.list(.list),
    all(lengths(.list) == 2),
    all(map_lgl(.list, ~ is.character(.x) && length(.x) == 2))
  )

  if (!is_ratio_pairs) {
    cd_abort(c("x" = "{.arg arg} is not a proper ratio pair."), call = call)
  }

  invisible(TRUE)
}

check_scalar_integerish <- function(vec, arg = caller_arg(vec), call = caller_env()) {
  check_required(vec, arg = arg, call = call)

  if (!is_scalar_integerish(vec)) {
    cd_abort(
      message = c(
        "x" = "{.arg {arg}} is not an integer",
        "!" = "Provide a scalar integer value"
      ),
      call = call
    )
  }
}

cd_plot_theme <- function() {
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    legend.background = element_rect(color = "black", size = 0.5),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 13),
    legend.key.size = unit(8, "mm"),
    legend.box.spacing = unit(0.5, "cm"),
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.caption = element_text(size = 12, hjust = 0),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16),
    strip.background = element_blank(),
    strip.text = element_text(size = 12)
  )
}

cd_report_theme <- function(base_size = 10, base_family = "",
                            base_line_size = base_size / 22,
                            base_rect_size = base_size / 22) {
  # theme(
  #   panel.background = element_blank(),
  #   panel.border = element_rect(color = "black", fill = NA, size = 0.8),
  #
  #   legend.background = element_rect(color = "black",size = 0.5),
  #   legend.position = "bottom",
  #   legend.title = element_blank(),
  #   legend.text = element_text(size = 8),
  #   # legend.text = element_text(size = 13),
  #   # legend.key.size = unit(8, "mm"),
  #
  #   plot.title = element_text(size = 10, hjust = 0.5),
  #   plot.subtitle = element_text(size = 10, hjust = 0.5),
  #   plot.caption = element_text(hjust = 0),
  #
  #   # plot.title = element_text(size = 16, hjust = 0.5),
  #   # plot.subtitle = element_text(size = 12, hjust = 0.5),
  #   # plot.caption = element_text(size = 12, hjust = 0),
  #
  #   axis.text = element_text(size = 8),
  #   # axis.text = element_text(size = 14),
  #   axis.title = element_text(size = 10),
  #   # axis.title = element_text(size = 18),
  #
  #   strip.background = element_blank(),
  #   strip.text = element_text(size = 10),
  #   # strip.text = element_text(size = 12)
  # )

  half_line <- base_size / 2
  small_rel <- 0.85
  small_size <- base_size * small_rel
  theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %>%
    theme(
      line = element_line(
        colour = "black", size = base_line_size,
        linetype = 1, lineend = "butt"
      ),
      rect = element_rect(
        fill = NA, colour = "black",
        size = base_rect_size, linetype = 1
      ),
      text = element_text(
        family = base_family, face = "plain",
        colour = "black", size = base_size,
        lineheight = 0.9, hjust = 0.5, vjust = 0.5,
        angle = 0, margin = margin(), debug = FALSE
      ),
      axis.line = element_line(colour = "black", size = base_line_size),
      axis.line.x = element_line(colour = "black", size = base_line_size),
      axis.line.y = element_line(colour = "black", size = base_line_size),
      axis.text = element_text(size = small_size),
      axis.text.x = element_text(margin = margin(t = small_rel / 2), vjust = 1),
      axis.text.y = element_text(margin = margin(r = small_rel / 2), hjust = 1),
      axis.ticks = element_line(colour = "black", size = base_line_size),
      axis.title = element_text(size = base_size),
      axis.title.x = element_text(margin = margin(t = half_line)),
      axis.title.y = element_text(angle = 90L, margin = margin(r = half_line)),
      legend.key = element_rect(fill = "white", colour = NA),
      legend.key.size = unit(0.8, "lines"),
      legend.background = element_rect(fill = alpha("white", 0)), # Transparent background
      legend.position = "bottom", # or "bottom"
      legend.text = element_text(size = small_size),
      legend.title = element_text(size = small_size),
      panel.background = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, size = 0.8),
      panel.grid = element_blank(),

      # plot.background = element_rect(fill = NA, colour = NA),
      plot.title = element_text(
        size = base_size, hjust = 0.5,
        margin = margin(b = half_line)
      ),
      plot.subtitle = element_text(
        size = base_size * 0.95, hjust = 0.5,
        margin = margin(b = half_line)
      ),
      plot.caption = element_text(
        size = small_size, hjust = 0,
        margin = margin(t = half_line)
      ),
      strip.background = element_blank(),
      strip.text = element_text(colour = "black", size = small_size)
    )
}



#' Plot Line Graph for Multiple Series with Dynamic Y-axis Scaling
#'
#' Generates a line graph for specified y variables over a shared x-axis, with dynamic scaling of the y-axis based on the data.
#'
#' @param .data A data frame containing the variables to plot.
#' @param x The unquoted column name for the x-axis variable (e.g., `year`).
#' @param y_vars A character vector of column names for the y variables to plot.
#' @param y_labels A character vector of labels for the y variables (must match the length of `y_vars`).
#' @param title The title of the plot.
#' @param y_axis_title The title of the y-axis.
#' @param hline An optional numeric value to draw a horizontal line.
#' @param hline_style The line style for the horizontal line, default is "dashed".
#'
#' @return A ggplot object.
#' @export
plot_line_graph <- function(.data, x, y_vars, y_labels, title, y_axis_title, hline = NULL, hline_style = "dashed") {
  variable <- value <- NULL

  # Ensure `y_vars` and `y_labels` are of the same length
  if (length(y_vars) != length(y_labels)) {
    cd_abort(c("x" = "`y_vars` and `y_labels` must have the same length."))
  }

  # Determine max and min for dynamic scaling, accounting for negatives
  y_max <- robust_max(sapply(y_vars, function(var) robust_max(.data[[var]])))
  y_min <- min(sapply(y_vars, function(var) min(.data[[var]], na.rm = TRUE)), na.rm = TRUE)

  # Set y_min to 0 if all values are non-negative
  if (y_min > 0) {
    y_min <- 0
  }

  # Choose the scaling factor and y-axis label suffix
  scale_factor <- if (y_max > 1e6) 1e6 else 1
  y_label_suffix <- if (scale_factor == 1e6) " (Millions)" else ""

  # Adjust limits based on scale factor and round up/down
  y_max <- ceiling(y_max / scale_factor) * scale_factor
  y_min <- floor(y_min / scale_factor) * scale_factor

  # Add a small allowance to y_max to create space at the top of the plot
  y_max <- y_max * 1.05

  # Set dynamic breaks for the y-axis
  y_breaks <- scales::pretty_breaks(n = 6)(c(y_min, y_max))

  # Reshape data for ggplot
  .data_long <- .data %>%
    select({{ x }}, !!!syms(y_vars)) %>%
    pivot_longer(cols = all_of(y_vars), names_to = "variable", values_to = "value") %>%
    mutate(
      variable = factor(variable, levels = y_vars, labels = y_labels),
      value = value / scale_factor
    )

  # Generate the line plot
  p <- .data_long %>%
    ggplot(aes(x = !!sym(x), y = value, colour = variable, group = variable)) +
    geom_point(size = 4) +
    geom_line(size = 1) +
    labs(
      title = title,
      x = NULL,
      y = paste0(y_axis_title, y_label_suffix)
    ) +
    scale_y_continuous(
      limits = c(y_min / scale_factor, y_max / scale_factor),
      breaks = scales::pretty_breaks(11),
      labels = scales::label_number(accuracy = 1, big.mark = ",")
      # expand = c(0, 0)
    ) +
    scale_color_manual(values = set_names(c("darkgreen", "orange", "blue", "purple", "red2", "brown")[1:length(y_vars)], y_labels)) +
    cd_plot_theme() +
    theme(
      plot.title = element_text(size = 14),
      axis.text = element_text(size = 10),
      legend.text = element_text(size = 10),
      legend.title = element_blank()
    )

  # Add horizontal line if specified
  if (!is.null(hline)) {
    p <- p +
      geom_hline(yintercept = hline, linetype = hline_style, color = "grey40")
  }

  return(p)
}
