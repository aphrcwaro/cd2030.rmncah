#' Compute Service Utilization Metrics
#'
#' Calculates annualized service utilization statistics including outpatient (OPD), inpatient (IPD),
#' case fatality rates, and under-5 service use, aggregated either at national or admin level 1.
#'
#' @param .data A data frame containing service and population indicators.
#' @param admin_level Either `"national"` or `"adminlevel_1"` to define the aggregation level.
#'
#' @return A `cd_service_utilization` object (tibble subclass) with summarized and derived indicators:
#' - OPD/IPD totals and rates
#' - Under-5 service utilization
#' - Case fatality rates
#' - Proportion of under-5 deaths
#'
#' @examples
#' \dontrun{
#'   compute_service_utilization(dat, admin_level = "national")
#' }
#'
#' @export
compute_service_utilization <- function(.data, admin_level = c('national', 'adminlevel_1', 'district')) {
  check_cd_data(.data)
  admin_level <- arg_match(admin_level)
  admin_level_cols <- get_admin_columns(admin_level)
  admin_level_cols <- c(admin_level_cols, 'year')

  pop_vars <- c('total_pop', 'under5_pop', 'under1_pop', 'live_births', 'total_births')
  vars <- c('opd_total', 'opd_under5', 'ipd_total', 'ipd_under5', 'under5_deaths', 'total_deaths')

  result <- .data %>%
    mutate(
      year_opd_rr = round(mean(opd_rr, na.rm = TRUE), 1),
      year_ipd_rr = round(mean(ipd_rr, na.rm = TRUE), 1),
      .by = all_of(admin_level_cols)
    ) %>%
    summarise(
      across(all_of(vars), sum, na.rm = TRUE),
      across(all_of(pop_vars), robust_max),
      across(starts_with('year_'), robust_max, .names = "{sub('^year_', '', .col)}"),
      .by = c(adminlevel_1, district, year),
    ) %>%
    summarise(
      across(all_of(c(vars, pop_vars)), sum, na.rm = TRUE),
      across(ends_with('_rr'), robust_max),
      .by = all_of(admin_level_cols)
    ) %>%
    mutate(
      mean_opd_total = opd_total / total_pop,
      mean_ipd_total = 100 * ipd_total / total_pop,
      mean_opd_under5 = opd_under5 / under5_pop,
      mean_ipd_under5 = 100 * ipd_under5 / under5_pop,
      perc_opd_under5 = 100 * opd_under5 / opd_total,
      perc_ipd_under5 = 100 * ipd_under5 / ipd_total,
      cfr_under5 = 100 * under5_deaths / ipd_under5,
      cfr_total = 100 * total_deaths / ipd_total,
      prop_death = 100 * under5_deaths / total_deaths
    ) %>%
    select(-live_births, -under1_pop, -total_births)

  new_tibble(
    result,
    class = 'cd_service_utilization',
    admin_level = admin_level
  )
}

#' Filter and Prepare Service Utilization Data for Mapping
#'
#' Filters a `cd_service_utilization` object by country, indicator, and year.
#' Joins spatial data for subnational mapping and renames geometry for use with `ggplot2::geom_sf()`.
#'
#' @param .data A `cd_service_utilization` object returned by [compute_service_utilization()].
#' @param country_iso Character. ISO3 country code.
#' @param indicator Character. Service indicator to map, either `"ipd"` or `"opd"`. Defaults to `"ipd"`.
#' @param plot_years Optional. Integer or vector of years to include.
#' @param subnational_map Optional. A mapping data frame to link `NAME_1` from the shapefile to internal admin labels.
#'
#' @return A tibble of class `cd_service_utilization_filtered`, ready for faceted spatial plotting.
#'
#' @details
#' This function:
#' - Selects the under-five service indicator (`mean_opd_under5` or `mean_ipd_under5`)
#' - Joins the appropriate admin level 1 shapefile using the specified country ISO
#' - Filters to specified `plot_years` if provided
#' - Renames the geometry column for compatibility with `geom_sf()`
#'
#' Only `adminlevel_1` data is currently supported for mapping.
#'
#' @examples
#' \dontrun{
#' filtered <- filter_service_utilization(service_data, "UGA", indicator = "opd", plot_years = 2019:2022)
#' plot(filtered)
#' }
#'
#' @export
filter_service_utilization <- function(.data, country_iso, indicator = c('ipd', 'opd'), plot_years = NULL, subnational_map = NULL) {

  check_cd_class(.data, expected_class = 'cd_service_utilization')
  check_required(country_iso)
  indicator <- arg_match(indicator)
  indicator <- paste0('mean_', indicator, '_under5')
  admin_level <- attr_or_abort(.data, 'admin_level')

  if (admin_level != 'adminlevel_1') {
    cd_abort(c('x' = 'only {.arg adminlevel_1} is supported for mapping.'))
  }

  shapefile <- get_country_shapefile(country_iso, 'admin_level_1')

  shapefile <- if (is.null(subnational_map)) {
    shapefile %>% mutate(adminlevel_1 = NAME_1)
  } else {
    shapefile %>%
      left_join(subnational_map, by = "NAME_1") %>%
      rename(adminlevel_1 = admin_level_1)
  }

  merged_data <- .data %>%
    left_join(shapefile, by = join_by(adminlevel_1)) %>%
    filter(if (is.null(plot_years)) TRUE else year %in% plot_years) %>%
    select(any_of(c('adminlevel_1', 'district', 'year', indicator)), starts_with('geom')) %>%
    rename_with(~ 'geometry', starts_with('geom'))

  new_tibble(
    merged_data,
    class = 'cd_service_utilization_filtered',
    indicator = indicator
  )
}

#' Filter Service Utilization Data for a Specific Region and Indicator
#'
#' Prepares a `cd_service_utilization` object for non-spatial plotting (e.g., time series)
#' by filtering by administrative region and indicator type.
#'
#' @param .data A `cd_service_utilization` object created by [compute_service_utilization()].
#' @param indicator Character. The indicator to visualize. Options include `"opd"`, `"ipd"`,
#' `"under5"` (proportion under 5), `"cfr"` (case fatality rate), or `"deaths"` (proportion of under-5 deaths).
#' @param region Optional. A single region name to filter when data is subnational (`adminlevel_1` or `district`).
#'
#' @return A tibble of class `cd_service_utilization_map`, with an attached `indicator` attribute for plotting.
#'
#' @details
#' This function:
#' - Validates the `.data` class and `indicator` input
#' - If data is subnational, filters it by the specified `region`
#' - If data is national, ensures `region` is not provided
#' - Returns a filtered tibble tagged with `cd_service_utilization_map` class for downstream plotting
#'
#' @examples
#' \dontrun{
#' # Filter IPD indicator for Central region
#' filtered <- filter_service_utilization_map(service_data, indicator = "ipd", region = "Central")
#' }
#'
#' @export
filter_service_utilization_map <- function(.data, indicator = c('opd', 'ipd', 'under5', 'cfr', 'deaths'), region = NULL) {
  check_cd_class(.data, expected_class = 'cd_service_utilization')
  indicator <- arg_match(indicator)

  admin_level <- attr_or_abort(.data, 'admin_level')

  if (admin_level == 'national' && !is.null(region)) {
    cd_abort('x' = '{.arg region} cannot be specified in {.field national} data.')
  }

  if (admin_level != 'national' && is.null(region) && !is_scalar_character(region)) {
    cd_abort('x' = '{.arg region} must be a scalar string.')
  }

  data <- if (admin_level != 'national') {
    .data %>%
      filter(!!sym(admin_level) == region)
  } else {
    .data
  }

  new_tibble(
    data,
    class = 'cd_service_utilization_map',
    indicator = indicator
  )
}

#' Format Service Utilization Data for Excel Export
#'
#' Transforms a `cd_service_utilization` object into a wide-format table
#' with labeled indicators, suitable for export to Excel.
#'
#' @param .data A `cd_service_utilization` object produced by [compute_service_utilization()].
#'
#' @return A tibble in wide format with:
#' - One row per indicator per admin unit
#' - One column per year
#' - A human-readable label for each indicator (`indiclabel`)
#'
#' @details
#' The output table includes population, OPD/IPD service utilization,
#' under-5 service metrics, reporting completeness, and case fatality rates,
#' reshaped to facilitate Excel review or reporting.
#'
#' @examples
#' \dontrun{
#'   x <- compute_service_utilization(dat, admin_level = "adminlevel_1")
#'   get_excel_version(x)
#' }
#'
#' @export

get_excel_version <- function(.data) {
  check_service_utilization(.data)

  admin_level <- attr_or_abort(.data, 'admin_level')
  admin_level_col <- get_admin_columns(admin_level)

  indicator_labels <- c(
    "total_pop"           = "Estimated population, all ages",
    "under5_pop"          = "Estimated population under-5",
    "mean_opd_total"      = "Mean OPD visits per person per year, all ages",
    "mean_ipd_total"      = "Mean IPD admissions per 100 persons per year, all ages",
    "opd_rr"              = "Completeness reporting OPD",
    "opd_total"           = "N of OPD visits per year, all ages",
    "opd_under5"          = "N of OPD visits per year, under-5",
    "mean_opd_under5"     = "Mean OPD visits per child per year, under-5",
    "perc_opd_under5"     = "Percent of OPD visits that are under-5",
    "ipd_rr"              = "Completeness reporting IPD",
    "ipd_total"           = "N of IPD admissions per year, all ages",
    "ipd_under5"          = "N of IPD admissions per year, under-5",
    "mean_ipd_under5"     = "Mean IPD admissions per 100 children per year, under-5",
    "perc_ipd_under5"     = "Percent of IPD admissions that are under-5",
    "total_deaths"         = "N of deaths per year, all ages",
    "under5_deaths"        = "N of deaths in children under-5",
    "cfr_total"           = "CFR: deaths per 100 admissions per year, all ages",
    "cfr_under5"          = "CFR: deaths per 100 admissions per year, under-5",
    "prop_death"          = "Proportion of under-5 deaths (%): under-5 to total deaths"
  )

  .data %>%
    pivot_longer(
      cols = -any_of(c(admin_level_col, 'year')),
      names_to = "indic",
      values_to = "value"
    ) %>%
    pivot_wider( names_from = year, values_from = value) %>%
    mutate(indiclabel = factor(indicator_labels[indic], levels = unname(indicator_labels))) %>%
    arrange(!!!syms(admin_level_col)) %>%
    relocate(!!!syms(admin_level_col), indiclabel)
}
