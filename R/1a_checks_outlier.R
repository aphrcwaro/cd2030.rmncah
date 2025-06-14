#' Annual Summary of Outlier-Free Reporting Rates
#'
#' `calculate_outliers_summary` computes yearly percentages of non-outlier values
#' for each health indicator, based on precomputed 5-MAD outlier flags. Summarizes
#' data quality by administrative level and year.
#'
#' @param .data A `cd_data` object with `_outlier5std` flag columns (0 = valid, 1 = outlier).
#' @param admin_level Character. Aggregation level. One of:
#'   - `"national"`
#'   - `"adminlevel_1"`
#'   - `"district"`
#' @param region Optional. Restrict to a specific region (only when `admin_level = "adminlevel_1"`).
#'
#' @details
#' Outliers are defined using a Hampel filter with a 5-MAD threshold.
#'
#' This function:
#' - Aggregates outlier flags by year and administrative level
#' - Computes the share of valid (non-outlier) values per indicator
#' - Adds overall summary metrics:
#'   - `mean_out_all`: average non-outlier rate across all indicators
#'   - `mean_out_four`: average across a subset of key indicators (excluding IPD)
#'
#' Values are expressed as percentages (0–100).
#'
#'  @return A tibble of class `cd_outlier`.
#'
#' @examples
#' \dontrun{
#'   calculate_outliers_summary(data, admin_level = "district")
#' }
#'
#' @export
calculate_outliers_summary <- function(.data, admin_level = c('national', 'adminlevel_1', 'district'), region = NULL) {
  year <- . <- NULL

  check_cd_data(.data)
  admin_level <- arg_match(admin_level)
  admin_level_cols <- get_admin_columns(admin_level, region)

  all_indicators <- get_all_indicators()
  ipd <- get_indicator_groups()[['ipd']]
  four_indicators <- paste0(setdiff(all_indicators, ipd), '_outlier5std')

  outlier_cols <- paste0(all_indicators, '_outlier5std')

  data <- .data %>%
    calculate_outlier_core(indicators = all_indicators, admin_level = admin_level, region = region) %>%
    summarise(across(any_of(outlier_cols), mean, na.rm = TRUE), .by =c(admin_level_cols, 'year')) %>%
    mutate(
      mean_out_all = rowMeans(pick(any_of(outlier_cols)), na.rm = TRUE),
      mean_out_four = rowMeans(pick(any_of(four_indicators)), na.rm = TRUE),
      across(c(any_of(outlier_cols), starts_with('mean_out_')), ~ round((1 - .x) * 100, 0))
    )

  new_tibble(
    data,
    class = 'cd_outlier',
    admin_level = admin_level,
    region = region
  )
}

#' District-Level Outlier Summary by Year
#'
#' Calculates the yearly percentage of districts without extreme outliers for each health indicator.
#' Flags are based on the Hampel X84 method (beyond 5 MAD from median).
#'
#' @param .data A `cd_data` object with outlier flags (`_outlier5std` columns).
#' @param region Optional. Filter results to a specific `adminlevel_1`.
#'
#' @details
#' - Aggregates by `district` and `year`, using the max flag per group.
#' - Computes percent of non-outliers per indicator.
#' - Returns yearly summaries including:
#'   - Per-indicator non-outlier rates
#'   - `mean_out_all`: all indicators
#'   - `mean_out_four`: excludes IPD
#'
#' All results are rounded to two decimals.
#'
#' @return A `cd_district_outliers_summary` tibble.
#'
#' @examples
#' \dontrun{
#' calculate_district_outlier_summary(data)
#' }
#'
#' @export
calculate_district_outlier_summary <- function(.data, region = NULL) {
  district <- year <- . <- NULL

  check_cd_data(.data)

  all_indicators <- get_all_indicators()
  ipd <- get_indicator_groups()[['ipd']]
  four_indicators <- paste0(setdiff(all_indicators, ipd), '_outlier5std')

  outlier_cols <- paste0(all_indicators, '_outlier5std')

  data <- .data %>%
    calculate_outlier_core(indicators = all_indicators, admin_level = 'district') %>%
    filter(if(!is.null(region)) adminlevel_1 == region else TRUE) %>%
    summarise(across(all_of(outlier_cols), robust_max), .by = c(district, year)) %>%
    summarise(across(all_of(outlier_cols), mean, na.rm = TRUE), .by = year) %>%
    mutate(
      mean_out_all = rowMeans(pick(all_of(outlier_cols)), na.rm = TRUE),
      mean_out_four = rowMeans(pick(all_of(four_indicators)), na.rm = TRUE),
      across(c(all_of(outlier_cols), starts_with("mean_out_")), ~ round((1 - .x) * 100, 2))
    )

  new_tibble(
    data,
    class = 'cd_district_outliers_summary'
  )
}

#' Identify Monthly Outliers for a Single Indicator
#'
#' Flags extreme monthly values for a given immunization indicator using the Hampel method.
#'
#' @param .data A `cd_data` object with monthly data.
#' @param indicator A single indicator name to assess.
#' @param admin_level `'adminlevel_1'` or `'district'`.
#' @param region Optional. Filter results to a specific `adminlevel_1`.
#'
#' @details
#' - Aggregates by `year`, `month`, and administrative unit.
#' - Computes median and MAD (Median Absolute Deviation).
#' - Flags outliers when values are ±5×MAD from median.
#'
#' @return A `cd_outlier_list` tibble with:
#' - Grouping columns
#' - Raw values for `indicator`
#' - Median (`<indicator>_med`)
#' - MAD (`<indicator>_mad`)
#' - Outlier flag (`<indicator>_outlier5std`)
#'
#' @examples
#' \dontrun{
#' list_outlier_units(cd_data, indicator = 'penta1', admin_level = 'district')
#' }
#'
#' @export
list_outlier_units <- function(.data,
                               indicator,
                               admin_level = c('adminlevel_1', 'district'),
                               region = NULL) {
  check_cd_data(.data)
  indicator <- arg_match(indicator, get_all_indicators())
  admin_level <- arg_match(admin_level)

  admin_level_cols <- get_admin_columns(admin_level, region)
  admin_level_cols <- c(admin_level_cols, 'year', 'month')

  x <- .data %>%
    calculate_outlier_core(indicators = indicator, admin_level = admin_level, region) %>%
    select(any_of(c(admin_level_cols, indicator, paste0(indicator, c('_med', '_mad', '_outlier5std')))))

  new_tibble(
    x,
    class = 'cd_outlier_list',
    indicator = indicator,
    admin_level = admin_level,
    region = region
  )
}


#' Calculate Outlier Flags for Reporting Indicators
#'
#' Computes monthly averages for selected indicators and flags outliers based on
#' a 5-standard-deviation rule. Aggregation is done by year, month, and the specified
#' administrative level.
#'
#' @param .data A `cd_data` object containing the raw reporting data.
#' @param indicators A character vector of indicator names to evaluate.
#' @param admin_level Administrative level to group by. Must be one of:
#'   `"national"`, `"adminlevel_1"`, or `"district"`.
#' @param region Optional. Restricts computation to a single adminlevel_1 region.
#'   Only valid when `admin_level` is `"adminlevel_1"`.
#'
#' @return A tibble with:
#'   - Grouping columns (`year`, `month`, and admin columns)
#'   - Monthly means for each indicator
#'   - Logical flags marking outliers based on the 5-standard-deviation rule
#'
#' @noRd
calculate_outlier_core <- function(.data, indicators, admin_level = c('national', 'adminlevel_1', 'district'), region = NULL) {
  check_cd_data(.data)
  check_required(indicators)

  admin_level <- arg_match(admin_level)
  group_vars <- get_admin_columns(admin_level, region)

  .data %>%
    filter(if (!is.null(region)) adminlevel_1 == region else TRUE) %>%
    summarise(across(any_of(indicators), sum, na.rm = TRUE), .by = c(group_vars, 'year', 'month')) %>%
    add_outlier5std_column(indicators = indicators, group_by = group_vars)
}
