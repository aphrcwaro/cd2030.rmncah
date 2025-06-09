#' Plot National Population or Births Metrics
#'
#' This function generates a plot to visualize national-level demographic data
#' for total population or live births over time.It compares DHIS-2 and UN
#' projections for the specified metric, limited to national-level data due to
#' the availability of UN estimates.
#'
#' @param x A `cd_population_metrics` object containing national-level demographic
#'   data.
#' @param metric A character string specifying the type of data to plot. It must
#'   be one of:
#'   - **'population'**: Plots total population estimates in thousands from DHIS-2 and UN sources.
#'   - **'births'**: Plots total live births in thousands from DHIS-2 and UN sources.
#' @param ... Additional arguments (not used in this function).
#'
#' @details
#' The `plot_population_metrics` function provides visualizations based on the
#'   selected `metric`:
#'   - **Population Plot**: Shows total population (in thousands) by year, with
#'     separate lines for DHIS-2 and UN projections.
#'   - **Live Births Plot**: Displays total live births (in thousands) by year,
#'     comparing DHIS-2 and UN projections.
#'
#' This function supports only national-level data as UN estimates are not available
#' at subnational levels. Attempting to plot subnational data will result in an error.
#'
#' @return A ggplot object visualizing the selected demographic data over time.
#'
#' @examples
#' \dontrun{
#' # Plot total population estimates at the national level
#' plot(population_metrics_data, metric = "population")
#'
#' # Plot total live births estimates at the national level
#' plot(population_metrics_data, metric = "births")
#' }
#'
#' @export
plot.cd_population_metrics <- function(x, metric = c("population", "births"), ...) {
  year <- un_population <- totpop_dhis2 <- un_births <- totlivebirths_dhis2 <- NULL

  # Match argument to enforce valid choices
  metric <- arg_match(metric)

  admin_level <- attr(x, "admin_level")
  if (admin_level != "national") {
    cd_abort(
      c("x" = "This plot supports only national-level data due to the requirement of UN estimates.")
    )
  }

  if (metric == "population") {
    plot_line_graph(
      .data = x,
      x = "year",
      y_vars = c("un_population", "totpop_dhis2"),
      y_labels = c("UN Population (in 1000)", "DHIS-2 Population Projection (in 1000)"),
      title = "Total Population (in thousands), DHIS2 and UN projections",
      y_axis_title = "Population"
    )
  } else if (metric == "births") {
    plot_line_graph(
      .data = x,
      x = "year",
      y_vars = c("un_births", "totlivebirths_dhis2"),
      y_labels = c("UN Live Births (in 1000)", "DHIS-2 Live Births Projection (in 1000)"),
      title = "Total Live Births (in thousands), DHIS2 and UN projections",
      y_axis_title = "Population"
    )
  }
}
