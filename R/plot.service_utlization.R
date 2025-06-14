#' Plot Service Utilization Indicators
#'
#' Visualizes service utilization over time from a `cd_service_utilization_map` object.
#'
#' @param x A `cd_service_utilization_map` object created by [filter_service_utilization_map()].
#' @param ... not used
#'
#' @return A `ggplot2` plot object.
#'
#' @examples
#' \dontrun{
#' plot(filter_service_utilization_map(dat, indicator = 'opd'))
#' }
#'
#' @export
plot.cd_service_utilization_map <- function(x, ...) {

  indicator <- attr_or_abort(x, 'indicator')

  labels <- list(
    opd = list(
      y1 = 'mean_opd_under5',
      y2 = 'mean_opd_total',
      y1_label = 'Under-5',
      y2_label = 'All ages',
      title = 'OPD visits',
      y_label = 'Mean # of OPD visits per person'
    ),
    ipd = list(
      y1 = 'mean_ipd_under5',
      y2 = 'mean_ipd_total',
      y1_label = 'Under-5',
      y2_label = 'All ages',
      title = 'IPD admissions',
      y_label = 'Mean # of IPD admissions per 100 persons'
    ),
    under5 = list(
      y1 = 'perc_opd_under5',
      y2 = 'perc_ipd_under5',
      y1_label = 'OPD visits',
      y2_label = 'IPD admissions',
      title = 'Percentage of under-5 OPD visits and IPD admissions',
      y_label = 'Percentage'
    ),
    cfr = list(
      y1 = 'cfr_under5',
      y2 = 'cfr_total',
      y1_label = 'CFR: Under-5',
      y2_label = 'CFR: All ages',
      title = 'Case fatality rate (%)',
      y_label = 'Percentage'
    ),
    deaths = list(
      y1 = 'prop_death',
      y2 = 'prop_death',
      y1_label = NULL,
      y2_label = NULL,
      title = 'Proportion of deaths (%): under-5 to all-age',
      y_label = 'Percentage'
    )
  )

  labels_val <- labels[[indicator]]

  max_y <- robust_max(c(x[[labels_val$y1]], x[[labels_val$y2]]))
  limits <- c(0, max_y)
  breaks <- scales::pretty_breaks(n = 11)(limits)

  x %>%
    ggplot(aes(x = year)) +
    geom_point(aes(y = !!sym(labels_val$y1), color = labels_val$y1_label), size = 2) +
    geom_line(aes(y = !!sym(labels_val$y1), color = labels_val$y1_label), size = 1) +
    geom_point(aes(y = !!sym(labels_val$y2), color = labels_val$y2_label), size = 2) +
    geom_line(aes(y = !!sym(labels_val$y2), color = labels_val$y2_label), size = 1) +
    labs(title = labels_val$title, y = labels_val$y_label, x = 'Year') +
    scale_y_continuous(limits = limits, breaks = breaks, expand = expansion(mult = c(0, 0.1))) +
    cd_plot_theme()
}

#' Plot Filtered Service Utilization Indicators
#'
#' Generates a faceted map of service utilization metrics across subnational units
#' (admin level 1) for each available year. Uses spatial polygons filled by the selected indicator.
#'
#' @param x A `cd_service_utilization_filtered` object returned by [filter_service_utilization()].
#' @param ... Additional arguments (currently unused).
#'
#' @return A `ggplot2` object representing faceted service utilization maps by year.
#'
#' @details
#' This function:
#' - Extracts the appropriate indicator (`mean_opd_under5` or `mean_ipd_under5`)
#' - Projects the data to WGS84 for map consistency
#' - Renders the spatial data using `geom_sf()` with a sequential purple color scale
#' - Facets by year and applies the package's custom plot theme
#'
#' @examples
#' \dontrun{
#' filtered <- filter_service_utilization(service_data, "UGA", indicator = "opd", plot_years = 2019:2022)
#' plot(filtered)
#' }
#'
#' @export
plot.cd_service_utilization_filtered <- function(x, ...) {

  indicator <- attr_or_abort(x, 'indicator')

  title <- switch (indicator,
                   mean_opd_under5 = 'OPD under-five by Region',
                   mean_ipd_under5 = 'IPD under-five by Region'
  )

  legend <- switch (indicator,
                    mean_opd_under5 = 'Mean OPD per child per year',
                    mean_ipd_under5 = 'Mean IPD per 100 children per year'
  )

  x %>%
    st_set_geometry('geometry') %>%
    st_as_sf() %>%
    st_set_crs(4326) %>%
    st_transform(crs = 4326) %>%
    ggplot() +
    geom_sf(aes(fill = !!sym(indicator)), color = "white") +
    facet_wrap(~ year, scales = "fixed", ncol = 5) +
    scale_fill_gradientn(
      colours = brewer.pal(9, "Purples"),
      na.value = "grey90",
      name =  legend
    ) +
    cd_plot_theme() +
    labs(title = title) +
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
