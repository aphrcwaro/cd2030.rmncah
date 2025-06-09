#' Plot Mortality Rate Indicators
#'
#' Produces line plots for national mortality indicators with regional points for comparison.
#'
#' @param x A `cd_mortality_summary` object.
#' @param indicator One of `"mmr_inst"`, `"ratio_md_sb"`, `"sbr_inst"`, or `"nn_inst"`.
#' @param ... Additional arguments passed to methods.
#'
#' @return A ggplot object.
#'
#' @export
plot.cd_mortality_summary <- function(x, indicator = c('mmr_inst', 'ratio_md_sb', 'sbr_inst', 'nn_inst'), ...) {
  indicator <- arg_match(indicator)

  label <- switch(
    indicator,
    mmr_inst = 'National inst. MMR',
    nn_inst = 'National NN',
    sbr_inst = 'National inst. SBR',
    ratio_md_sb = 'National SB/MM ratio'
  )

  title <- switch(
    indicator,
    mmr_inst = 'Maternal mortality per 100,000 live births in health facilities',
    ratio_md_sb = 'Ratio number of stillbirths to maternal deaths in health facilities',
    sbr_inst = 'Stillbirths per 1,000 births in health facilities',
    nn_inst = 'Neonatal deaths before discharge per 1,000 live births in health facilities'
  )

  national <- x %>% filter(adminlevel_1 == 'National')
  regional <- x %>% filter(adminlevel_1 != 'National')

  max_y <- robust_max(x[[indicator]], 100)
  limits <- c(0, max_y)
  breaks <- scales::pretty_breaks(n = 11)(limits)

  ggplot() +
    geom_point(data = regional, aes(x = year, y = !!sym(indicator), color = 'Regions'), size = 2, alpha = 0.7) +
    geom_line(data = national, aes(x = year, y = !!sym(indicator), color = label), linewidth = 1.2) +
    geom_point(data = national, aes(x = year, y = !!sym(indicator), color = label), size = 2) +
    geom_text(
      data = national,
      aes(x = year, y = !!sym(indicator), label = round(!!sym(indicator), 1)),
      color = 'black', vjust = -0.5, hjust = -0.1, size = 3
    ) +
    labs(
      title = title,
      x = NULL,
      y = NULL
    ) +
    scale_y_continuous(limits = limits, breaks = breaks, expand = expansion(mult = c(0, 0.05))) +
    scale_color_manual(values = set_names(c('forestgreen', 'orangered'), c('Regions', label))) +
    cd_plot_theme()
}


#' Plot Completeness of Facility Reporting Ratios
#'
#' Calculates and plots the estimated completeness of facility reporting
#' for maternal deaths or stillbirths based on UN estimates and assumed
#' community-to-institution ratios.
#'
#' @param x A `cd_mortality_ratio_summarised ` object from completeness estimation.
#' @param ... Additional arguments (not used).
#'
#' @return A ggplot object with ratio lines, labels, and reference points.
#'
#' @export
plot.cd_mortality_ratio_summarised <- function(x, ...) {

  plot_type <- attr_or_abort(x, 'plot_type')

  lower_bound <- paste0('UN ', str_to_upper(plot_type),' lower bound')
  upper_bound <- paste0('UN ', str_to_upper(plot_type),' upper bound')
  best_estimates <- paste0('UN ', str_to_upper(plot_type),' best estimate')

  labels <- list(
    sbr = list(
      title = 'Completeness of facility stillbirth reporting (%), based on UN stillbirth estimates and community to institutional ratio',
      x = 'Ratio Community SBR to Institutional SBR',
      y = 'Completeness stillbirth reporting by facilities (%)'
    ),
    mmr = list(
      title = 'Completeness of facility maternal death reporting (%), based on UN MMR estimates and community to institutional ratio',
      x = 'Ratio Community MMR to Institutional MMR',
      y = 'Completeness maternal deaths reporting by facilities (%)'
    )
  )

  label_values <- labels[[plot_type]]

  data <- x %>%
    pivot_longer(cols = -ciratio, values_to = 'rat', names_to = 'name') %>%
    mutate(name = factor(name, levels = c(lower_bound, best_estimates, upper_bound)))

  max_y <- robust_max(data$rat)
  max_y <- if (max_y < 100) 100 else max_y * 1.05

  data %>%
    ggplot(aes(x = ciratio, y = rat, colour = name)) +
    geom_line(size = 2) +
    geom_point(size = 13) +
    geom_text(aes(label = rat), color = "black", size = 4) +
    scale_y_continuous(limits = c(0, max_y), breaks = scales::pretty_breaks(n = 10), expand = expansion(mult = c(0,0.05))) +
    labs(
      title = label_values$title,
      x = label_values$x,
      y = label_values$y
    ) +
    cd_plot_theme()
}

#' Plot Filtered Institutional Mortality Rates
#'
#' Visualizes institutional mortality rates (`MMR` or `SBR`) across regions using filled
#' geographic polygons. Facets the map by year and applies a color gradient by value.
#'
#' @param x A `cd_mortality_summary_filtered` object returned by [filter_mortality_rate()].
#' @param ... Additional arguments (not used).
#'
#' @return A `ggplot2` object. This function is called for its side effect of rendering a map.
#'
#' @details
#' The function:
#' - Determines the appropriate plot title and legend based on the `indicator` attribute
#' - Projects the geometry to WGS84 for consistent map rendering
#' - Facets by year and uses `geom_sf()` to draw filled polygons
#' - Applies a sequential `Reds` color scale with gray for missing values
#'
#' @examples
#' \dontrun{
#' filtered <- filter_mortality_summary(mortality_data, "UGA", indicator = "mmr", plot_year = 2020:2022)
#' plot(filtered)
#' }
#'
#' @export
plot.cd_mortality_summary_filtered <- function(x, ...) {
  indicator <- attr_or_abort(x, 'indicator')

  title <- switch (
    indicator,
    mmr_inst = 'Institutional MMR by Region',
    sbr_inst = 'Institutional SBR by Region'
  )

  legend <- switch (
    indicator,
    mmr_inst = 'Institutional MMR per 100,000 livebirths',
    sbr_inst = 'Institutional SBR per 1000'
  )

  x %>%
    st_set_geometry('geometry') %>%
    st_as_sf() %>%
    st_set_crs(4326) %>%
    st_transform(crs = 4326) %>%
    ggplot() +
      geom_sf(aes(fill = !!sym(indicator)), colour = 'white') +
      facet_wrap(~ year, scales = 'fixed', ncol = 5) +
      scale_fill_gradientn(
        colours = brewer.pal(9, 'Reds'),
        na.value = 'gray90',
        name = legend
      ) +
      labs(title = title) +
      cd_plot_theme() +
      theme(
        panel.border = element_blank(),
        panel.spacing = unit(1, "lines"),
        legend.key.size = unit(6, "mm"),
        legend.background = element_blank(),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        aspect.ratio = 1
      )
}
