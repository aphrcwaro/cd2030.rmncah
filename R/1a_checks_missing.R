#' Summarize Data Completeness by Year
#'
#' Computes yearly percentage of non-missing values for each indicator, along with
#' overall completeness summaries.
#'
#' @param .data A `cd_data` object.
#' @param admin_level One of `'national'`, `'adminlevel_1'`, or `'district'`.
#'
#' @return A `cd_completeness_summary` tibble:
#'   - One row per year and group
#'   - `mis_<indicator>` columns with % non-missing values
#'   - `mean_mis_all` summarizing all indicators
#'
#' @examples
#' \dontrun{
#' calculate_completeness_summary(data)
#' }
#'
#' @export
calculate_completeness_summary <- function(.data,
                                           admin_level = c('national', 'adminlevel_1', 'district'),
                                           region = NULL) {
  year <- . <- NULL

  check_cd_data(.data)
  admin_level <- arg_match(admin_level)
  admin_level_cols <- get_admin_columns(admin_level, region)

  allindicators <- get_all_indicators()

  data <- .data %>%
    calculate_completeness_core(allindicators, region) %>%
    summarise(across(starts_with("mis_"), mean, na.rm = TRUE), .by = c(admin_level_cols, "year")) %>%
    mutate(
      mean_mis_all = rowMeans(select(., any_of(starts_with('mis_'))), na.rm = TRUE),
      across(c(starts_with('mis_'), starts_with('mean_mis_')), ~ round((1 - .x) * 100, 2))
    )

  new_tibble(
    data,
    class = 'cd_completeness_summary',
    admin_level = admin_level,
    region = region
  )
}

#' District-Level Completeness Summary
#'
#' Calculates yearly % of districts with complete data for each indicator.
#'
#' @param .data A `cd_data` object.
#'
#' @return A `cd_district_completeness_summary` tibble:
#'   - One row per year
#'   - `% non-missing` per indicator
#'   - `mean_mis_all`, `mean_mis_four` summaries
#'
#' @examples
#' \dontrun{
#' calculate_district_completeness_summary(data)
#' }
#'
#' @export
calculate_district_completeness_summary <- function(.data, region = NULL) {
  year <- district <- . <- NULL

  check_cd_data(.data)

  indicators <- get_all_indicators()
  ipd_indicators <- get_indicator_groups()['ipd']
  four_indicators <- paste0('mis_', indicators[which(!indicators %in% ipd_indicators)])

  data <- .data %>%
    calculate_completeness_core(indicators, region) %>%
    summarise(across(starts_with('mis_'), mean, na.rm = TRUE), .by = c(year, district)) %>%
    summarise(across(starts_with('mis_'), ~ mean(.x != 0, na.rm = TRUE)), .by = year) %>%
    mutate(
      mean_mis_all = rowMeans(select(., any_of(starts_with('mis_'))), na.rm = TRUE),
      mean_mis_four = rowMeans(select(., any_of(four_indicators)), na.rm = TRUE),
      across(c(starts_with('mis_'), starts_with('mean_mis_')), ~ round((1 - .x) * 100, 2))
    )

  new_tibble(
    data,
    class = 'cd_missing_district'
  )
}


#' @export
list_missing_units <- function(.data,
                               indicator,
                               region = NULL) {
  check_cd_data(.data)
  indicator <- arg_match(indicator, get_all_indicators())

  # admin_level_cols <- get_admin_columns('adminlevel_1', region)
  # admin_level_cols <- c(admin_level_cols, 'year', 'month')

  x <- .data %>%
    calculate_completeness_core(indicator, region) %>%
    # summarise(
    #   across(starts_with('mis_'), mean, na.rm  = TRUE),
    #   .by = admin_level_cols
    # ) %>%
    # mutate(
    #   across(starts_with('mis_'), ~ round((1 - .x) * 100, 0))
    # ) %>%
    filter(!!sym(paste0('mis_', indicator)) == 1) %>%
    select(adminlevel_1, district, year, month)


  new_tibble(
    x,
    class = 'cd_missing_list',
    indicator = indicator
  )
}

#' Core Function to Calculate Completeness Metrics
#'
#' Computes completeness (non-missing rate) for selected indicators grouped by admin level.
#'
#' @param .data A `cd_data` object
#' @param indicators Character vector of indicator names
#' @param region Optional. Only used if `admin_level = "adminlevel_1"`
#'
#' @return A tibble with one row per group and columns for each `mis_<indicator>`
#'
#' @noRd
calculate_completeness_core <- function(.data,
                                        indicators,
                                        region = NULL) {
  check_cd_data(.data)
  check_required(indicators)

  .data %>%
    add_missing_column(indicators) %>%
    filter(if (!is.null(region)) adminlevel_1 == region else TRUE)
}

