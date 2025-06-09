#' Plot National Denominators and Coverage Indicators
#'
#' Generates specific plots for national denominators and health coverage indicators based on the
#' calculated denominators in the `cd_indicator_coverage` object.
#'
#' @param x A `cd_indicator_coverage` object containing the calculated indicators.
#' @param plot_type Character. Type of plot to generate. Options include:
#'   - `"population_dhis2"`: Population estimates from DHIS-2 data
#'   - `"births_dhis2"`: Live birth estimates from DHIS-2
#'   - `"population_un"`: Population estimates from UN data
#'   - `"births_un"`: Live birth estimates from UN
#'   - `"anc_coverage"`: ANC coverage indicators
#'   - `"immunization_coverage_dhis2"`: DHIS-2 based immunization coverage indicators
#'   - `"immunization_coverage_un"`: UN-based immunization coverage
#'   - `"immunization_coverage_anc1"`: ANC-1 based immunization coverage
#' @param admin_name Name of the area to plot
#' @param ... Additional arguments (currently not used).
#'
#' @return A ggplot object for the selected plot type.
#' @export
plot.cd_indicator_coverage <- function(x,
                                       plot_type = c(
                                         "anc_coverage_dhis2",
                                         "delivery_coverage_dhis2",
                                         "immunization_coverage_dhis2",
                                         "anc_coverage_un",
                                         "delivery_coverage_un",
                                         "immunization_coverage_un",
                                         "anc_coverage_anc1",
                                         "delivery_coverage_anc1",
                                         "immunization_coverage_anc1",
                                         "anc_coverage_penta1",
                                         "delivery_coverage_penta1",
                                         "immunization_coverage_penta1"
                                       ),
                                       admin_name = NULL,
                                       ...) {
  plot_type <- arg_match(plot_type)

  admin_level <- attr(x, "admin_level")
  if (admin_level == "national" && !is.null(admin_name)) {
    cd_abort(
      c("x" = "{.arg admin_name} should be null when the coverage data is national")
    )
  }

  if (admin_level != "national" && is.null(admin_name)) {
    cd_abort(
      c("x" = "{.arg admin_name} should be not null when the coverage data is subnational")
    )
  }

  if (admin_level != "national" && endsWith(plot_type, "_un")) {
    cd_abort(
      c("x" = "{.arg {plot_type}} cannot be plotted at subnational level")
    )
  }

  if (!is.null(admin_level) && admin_level != "national") {
    x <- x %>%
      filter(!!sym(admin_level) == admin_name)
  }

  if (NROW(x) == 0) {
    cd_abort(
      c("x" = "{.arg {admin_name}} does not exist in the data.")
    )
  }


  if (plot_type == "anc_coverage_dhis2") {
    plot_line_graph(
      .data = x,
      x = "year",
      y_vars = c("cov_anc1_dhis2", "cov_instlivebirths_dhis2"),
      y_labels = c("Coverage ANC-1", "Inst'l birth coverage"),
      title = "ANC Coverage indicators, based on projected live births in DHIS-2",
      y_axis_title = "%"
    )
  } else if (plot_type == "delivery_coverage_dhis2") {
    plot_line_graph(
      .data = x,
      x = "year",
      y_vars = c("cov_zerodose_dhis2", "cov_undervax_dhis2", "cov_dropout_penta13_dhis2", "cov_dropout_measles12_dhis2"),
      y_labels = c(
        "Zerodose children (Penta1) %",
        "Underimmunized children %",
        "Penta 1 to 3 dropout rate",
        "Measles 1 to 2 dropout rate"
      ),
      title = "Delivery Coverage indicators, based on projected live births in DHIS-2",
      y_axis_title = "%"
    )
  } else if (plot_type == "immunization_coverage_dhis2") {
    plot_line_graph(
      .data = x,
      x = "year",
      y_vars = c("cov_penta1_dhis2", "cov_penta3_dhis2", "cov_opv3_dhis2", "cov_ipv1_dhis2", "cov_bcg_dhis2", "cov_measles1_dhis2", "cov_measles2_dhis2"),
      y_labels = c(
        "Coverage of Penta-1",
        "Coverage of Penta-3",
        "Coverage of opv-3",
        "Coverage of ipv-1",
        "Coverage of BCG",
        "Measles1 vacc coverage",
        "Measles2 vacc coverage"
      ),
      title = "Immunization coverage indicators, DHIS-2 data, NATIONAL, based on projected births in DHIS-2",
      y_axis_title = "%"
    )
  } else if (plot_type == "anc_coverage_un") {
    plot_line_graph(
      .data = x,
      x = "year",
      y_vars = c("cov_anc1_un", "cov_instlivebirths_un"),
      y_labels = c("Coverage ANC-1", "Inst'l birth coverage"),
      title = "ANC Coverage indicators, based on UN projections",
      y_axis_title = "%"
    )
  } else if (plot_type == "delivery_coverage_un") {
    plot_line_graph(
      .data = x,
      x = "year",
      y_vars = c("cov_zerodose_un", "cov_undervax_un", "cov_dropout_penta13_un", "cov_dropout_measles12_un"),
      y_labels = c(
        "Zerodose children (Penta1) %",
        "Underimmunized children %",
        "Penta 1 to 3 dropout rate",
        "Measles 1 to 2 dropout rate"
      ),
      title = "Delivery Coverage indicators, based on UN projections",
      y_axis_title = "%"
    )
  } else if (plot_type == "immunization_coverage_un") {
    plot_line_graph(
      .data = x,
      x = "year",
      y_vars = c("cov_penta1_un", "cov_penta3_un", "cov_opv3_un", "cov_bcg_un", "cov_measles1_un", "cov_measles2_un"),
      y_labels = c(
        "Coverage of Penta-1",
        "Coverage of Penta-3",
        "Coverage of opv-3",
        "Coverage of BCG",
        "Measles1 vacc coverage",
        "Measles2 vacc coverage"
      ),
      title = "Immunization coverage indicators, DHIS-2 data, NATIONAL, based on UN projections",
      y_axis_title = "%"
    )
  } else if (plot_type == "anc_coverage_anc1") {
    plot_line_graph(
      .data = x,
      x = "year",
      y_vars = c("cov_anc1_anc1", "cov_instlivebirths_anc1"),
      y_labels = c("Coverage ANC-1", "Inst'l birth coverage"),
      title = "ANC Coverage indicators, based on ANC1 derived denominator",
      y_axis_title = "%"
    )
  } else if (plot_type == "delivery_coverage_anc1") {
    plot_line_graph(
      .data = x,
      x = "year",
      y_vars = c("cov_zerodose_anc1", "cov_undervax_anc1", "cov_dropout_penta13_anc1", "cov_dropout_measles12_anc1"),
      y_labels = c(
        "Zerodose children (Penta1) %",
        "Underimmunized children %",
        "Penta 1 to 3 dropout rate",
        "Measles 1 to 2 dropout rate"
      ),
      title = "Delivery Coverage indicators, based on ANC1 derived denominator",
      y_axis_title = "%"
    )
  } else if (plot_type == "immunization_coverage_anc1") {
    plot_line_graph(
      .data = x,
      x = "year",
      y_vars = c("cov_penta1_anc1", "cov_penta3_anc1", "cov_opv3_anc1", "cov_ipv1_anc1", "cov_bcg_anc1", "cov_measles1_anc1", "cov_measles2_anc1"),
      y_labels = c(
        "Coverage of Penta-1",
        "Coverage of Penta-3",
        "Coverage of opv-3",
        "Coverage of ipv-1",
        "Coverage of BCG",
        "Measles1 vacc coverage",
        "Measles2 vacc coverage"
      ),
      title = "Immunization coverage indicators, DHIS-2 data, NATIONAL, based on ANC1 derived denominator",
      y_axis_title = "%"
    )
  } else if (plot_type == "anc_coverage_penta1") {
    plot_line_graph(
      .data = x,
      x = "year",
      y_vars = c("cov_anc1_penta1", "cov_instlivebirths_penta1"),
      y_labels = c("Coverage ANC-1", "Inst'l birth coverage"),
      title = "ANC Coverage indicators, based Penta1 derived denominator",
      y_axis_title = "%"
    )
  } else if (plot_type == "delivery_coverage_penta1") {
    plot_line_graph(
      .data = x,
      x = "year",
      y_vars = c("cov_zerodose_penta1", "cov_undervax_penta1", "cov_dropout_penta13_penta1", "cov_dropout_measles12_penta1"),
      y_labels = c(
        "Zerodose children (Penta1) %",
        "Underimmunized children %",
        "Penta 1 to 3 dropout rate",
        "Measles 1 to 2 dropout rate"
      ),
      title = "Delivery Coverage indicators, based on Penta1 derived denominator",
      y_axis_title = "%"
    )
  } else if (plot_type == "immunization_coverage_penta1") {
    plot_line_graph(
      .data = x,
      x = "year",
      y_vars = c("cov_penta1_penta1", "cov_penta3_penta1", "cov_opv3_penta1", "cov_ipv1_penta1", "cov_bcg_penta1", "cov_measles1_penta1", "cov_measles2_penta1"),
      y_labels = c(
        "Coverage of Penta-1",
        "Coverage of Penta-3",
        "Coverage of opv-3",
        "Coverage of ipv-1",
        "Coverage of BCG",
        "Measles1 vacc coverage",
        "Measles2 vacc coverage"
      ),
      title = "Immunization coverage indicators, DHIS-2 data, NATIONAL, based on Penta1 derived denominator",
      y_axis_title = "%"
    )
  }
}

#' Plot Coverage by Denominator Source with Survey Reference
#'
#' Generates a bar plot comparing DHIS2-based coverage using various denominator sources
#' against national survey coverage.
#'
#' @param x A data frame of class `'cd_indicator_coverage_filtered'` as returned by [filter_indicator_coverage()].
#' @param ... Additional arguments (not used).
#'
#' @return A `ggplot2` object showing coverage by denominator type and national survey line.
#'
#' @details
#' The function visualizes discrepancies in coverage estimates derived from different denominators:
#' DHIS2 population, ANC1, Penta1, UN projections, and optionally others. The horizontal line indicates
#' national survey coverage for the selected year, aiding in identifying possible under- or over-estimation.
#'
#' @seealso [filter_indicator_coverage()]
#'
#' @export
plot.cd_indicator_coverage_filtered <- function(x, ...) {
  country <- year <- category <- name <- indicator_name <- value <- NULL

  # Match the selected indicator to ensure it is valid
  indicator <- attr_or_abort(x, 'indicator')
  coverage <- attr_or_abort(x, 'coverage')

  # Auto-generate title based on the selected indicator
  title_text <- paste(indicator, "Coverage, DHIS2-based with different denominators, and survey coverage")

  # Plot
  ggplot(x, aes(x = category, y = value)) +
    geom_col(aes(color = "Facility-based coverage (%)"), fill = "darkgoldenrod3", width = 0.6) +

    # Add horizontal line for survey coverage, mapped to color aesthetic
    geom_hline(aes(yintercept = coverage, color = "Coverage survey, national"), size = 1) +
    labs(
      title = title_text,
      x = NULL, y = "Coverage (%)"
    ) +
    # scale_y_continuous(labels = scales::number_format()) +
    scale_color_manual(
      values = c("Facility-based coverage (%)" = "darkgoldenrod3", "Coverage survey, national" = "darkgreen"),
      name = "Coverage Type"
    ) +
    cd_plot_theme()
}

