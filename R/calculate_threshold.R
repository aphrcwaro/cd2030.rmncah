#' Calculate Dropout Coverage for Health Indicators Below a Threshold
#'
#' This function filters health indicator data to identify the percentage of administrative
#' regions where the coverage for a specified indicator falls below a 10% threshold for a given year.
#' If no regions meet the criteria (i.e., all values are below the threshold), a default output is returned.
#'
#' @param .data A tibble of class `cd_data`.
#' @param survey_year Integer. The year of Penta-1 survey provided
#' @param admin_level The level of analysis.
#' @param indicator Character. The specific health indicator to evaluate. Options are:
#'   - `"coverage"`:coverage indicators.
#'   - `"dropout"`: dropout indicators.
#' @param sbr Numeric. The stillbirth rate. Default is `0.02`.
#' @param nmr Numeric. Neonatal mortality rate. Default is `0.025`.
#' @param pnmr Numeric. Post-neonatal mortality rate. Default is `0.024`.
#' @param anc1survey Numeric. Survey-derived coverage rate for ANC-1 (antenatal care, first visit). Default is `0.98`.
#' @param dpt1survey Numeric. Survey-derived coverage rate for Penta-1 (DPT1 vaccination). Default is `0.97`.
#' @param survey_year Integer. The year of Penta-1 survey provided
#' @param preg_loss Numeric. Pregnancy loss rate
#' @param twin Numeric. Twin birth rate. Default is `0.015`.
#'
#' @return A tibble with the selected administrative level and coverage value for regions
#'   that do not meet the below-10% threshold for the specified indicator and year. If no regions
#'   meet the criteria, a default row is returned with "None" and 0 as values.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' result <- calculate_threshold(data, filter_year = 2023, indicator = "zerodose", source = "dhis2")
#' result
#' }
#'
#' @export
calculate_threshold <- function(.data,
                                denominator = c('dhis2', 'anc1', 'penta1', 'penta1derived'),
                                indicator = c('anc4', 'instdeliveries', 'vaccine')) {
  check_cd_indicator_coverage(.data)
  indicator <- arg_match(indicator)
  denominator <- arg_match(denominator)

  admin_level <- attr_or_abort(.data, 'admin_level')
  region <- attr_or_null(.data, 'region')

  admin_level_cols <- get_admin_columns(admin_level, region)

  indicators <- switch(
    indicator,
    vaccine = 'penta3|measles1|bcg',
    indicator
  )

  coverage <- switch (
    indicator,
    vaccine = 90,
    anc4 = 70,
    instdeliveries = 80
  )

  threshold <- .data %>%
    select(year, any_of(admin_level_cols), matches(paste0('cov_(', indicators, ')_', denominator, '$'))) %>%
    summarise(
      across(starts_with('cov_'), ~ mean(.x >= coverage, na.rm = TRUE) * 100),
      .by = year
    )

  new_tibble(
    threshold,
    class = "cd_threshold",
    admin_level = admin_level,
    indicator = indicator,
    region = region,
    coverage = coverage
  )
}

#' Filter High-Performing Areas by Coverage
#'
#' Filters a dataset to retain administrative areas with coverage values
#' above a specified threshold for a given indicator and denominator.
#'
#' @param .data An object of class `cd_indicator_coverage`.
#' @param indicator A string specifying the indicator.
#' @param denominator A string. The denominator used in coverage calculation.
#'   One of: `"dhis2"`, `"anc1"`, `"penta1"`, `"penta1derived"`.
#' @param threshold A numeric threshold for filtering. Default is `90`.
#'
#' @return A filtered data frame with the following columns: `adminlevel_1`, `district`, `year`, and the selected coverage column.
#'
#' @examples
#' \dontrun{
#' filter_high_performers(
#'   .data = survey_data,
#'   indicator = "penta3",
#'   denominator = "penta1",
#'   threshold = 85
#' )
#' }
#'
#' @export
filter_high_performers <- function(.data,
                                   indicator,
                                   denominator = c('dhis2', 'anc1', 'penta1', 'penta1derived'),
                                   threshold = 90) {
  check_cd_indicator_coverage(.data)
  indicator <- arg_match(indicator, get_indicator_without_opd_ipd())
  denominator <- arg_match(denominator)

  column <- paste0('cov_', indicator, '_', denominator)

  .data %>%
    mutate(!!sym(column) := round(!!sym(column))) %>%
    filter(!!sym(column) >= threshold) %>%
    select(any_of(c('adminlevel_1', 'district', 'year', column)))
}
