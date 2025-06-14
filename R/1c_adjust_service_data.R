#' Adjust Service Data for Coverage Analysis
#'
#' The `adjust_service_data` function processes DHIS-2 health service data, correcting
#' for incomplete reporting, applying k-factor adjustments for accurate scaling, and managing
#' outliers for consistency in data analysis. This standardization supports reliable
#' comparisons in immunization coverage and health service utilization studies.
#'
#' @param .data A `cd_data` dataframe, typically containing health facility data
#'   from DHIS-2 with monthly service counts by district.
#' @param adjustment A character string specifying the type of adjustment to apply:
#'   - **"default"**: Applies a default k-factor of 0.25 across all indicator groups.
#'   - **"custom"**: Uses user-defined `k_factors` for different indicator groups, with
#'       values between 0 and 1.
#'   - **"none"**: Returns the data without any adjustments.
#' @param k_factors A named numeric vector of custom k-factor values between 0 and 1
#'   for each indicator group (e.g., `c(anc = 0.3, idelv = 0.2, ...)`). Used only if
#'   `adjustment = "custom"`.
#'
#' @details
#' This function prepares service data through a series of steps to ensure data quality and consistency:
#'
#'   1. **Validation**: Checks the structure of `.data` and ensures that the `adjustment`
#'      argument is valid. For `custom` adjustments, `k_factors` must be specified and contain valid values.
#'
#'   2. **k-Factor Defaults**: Default k-factor values are set to 0.25 for each indicator group,
#'      unless overridden by user-provided values in `k_factors`.
#'
#'   3. **Reporting Completeness**: Flags any district-year reporting rates below 75% and imputes
#'      missing data using district-level medians to account for reporting inconsistencies.
#'
#'   4. **k-Factor Scaling**: Adjusts service counts based on the k-factor and reporting rate using
#'      the following scaling formula:
#'      \deqn{AdjustedValue = Value \times \left(1 + \left(\frac{1}{ReportingRate/100} - 1\right) \times k\right)}
#'      where \code{ReportingRate} is the reporting rate in percent, and \code{k} is the k-factor for the indicator group.
#'
#'   5. **Outlier Detection**: Identifies and flags extreme outliers using Hampel's X84 method, marking
#'      values that exceed 5 Median Absolute Deviations (MAD) from the median.
#'
#'   6. **Data Imputation**: Replaces remaining missing values with district-level medians to ensure
#'      data completeness.
#'
#' @return A `cd_adjusted_data` object containing adjusted service data, where outliers are flagged
#'   and managed, and missing values are imputed.
#'
#' @seealso [new_countdown()] for creating `cd_data` objects and [generate_adjustment_values()]
#' for generating adjustment summaries.
#'
#' @examples
#' \dontrun{
#' # Default adjustment
#' adjusted_data <- adjust_service_data(data, adjustment = "default")
#'
#' # Custom adjustment with specific k-factors
#' custom_k <- c(
#'   anc = 0.3, idelv = 0.2, pnc = 0.35, vacc = 0.4,
#'   opd = 0.3, ipd = 0.25
#' )
#' adjusted_data_custom <- adjust_service_data(data,
#'   adjustment = "custom",
#'   k_factors = custom_k
#' )
#'
#' # No adjustment
#' unadjusted_data <- adjust_service_data(data, adjustment = "none")
#' }
#'
#' @export
adjust_service_data <- function(.data,
                                adjustment = c("default", "custom", "none"),
                                k_factors = NULL) {
  district <- year <- month <- NULL

  check_cd_data(.data)

  adjustment <- arg_match(adjustment)

  k_defaults <- c(anc = 0.25, idelv = 0.25, pnc = 0.25, vacc = 0.25, opd = 0.25, ipd = 0.25)

  if (adjustment == "none" || (adjustment == 'custom' && all(k_factors == 0))) {
    cd_info(c("i" = "No adjustment applied. Data returned as-is."))
    return(new_countdown(.data, "cd_adjusted_data"))
  }

  if (adjustment == "custom") {
    if (is.null(k_factors) || any(k_factors < 0 | k_factors > 1)) {
      cd_abort(c("x" = "k_factors must be a numeric vector with values between 0 and 1 for each indicator group."))
    }

    common_names <- intersect(names(k_defaults), names(k_factors))
    k_defaults[common_names] <- k_factors[common_names]
  }

  indicator_groups <- get_indicator_groups()
  all_indicators <- get_indicator_without_opd_ipd()
  last_year <- robust_max(.data$year)

  merged_data <- .data %>%
    mutate(
      across(
        all_of(all_indicators),
        ~ get(paste0(names(keep(indicator_groups, ~ cur_column() %in% .x)), "_rr")),
        .names = "{.col}_rr"
      ),
    ) %>%
    mutate(
      across(
        all_of(paste0(all_indicators, "_rr")),
        ~ if_else(. < 75 | is.na(.), median(.[. >= 75 & . <= 100], na.rm = TRUE), .)
      ),
      .by = district
    ) %>%
    arrange(district, year, month) %>%
    mutate(
      across(
        all_of(all_indicators),
        ~ {
          # Identify the main indicator group for the current sub-indicator
          group <- names(keep(indicator_groups, ~ cur_column() %in% .x))

          # Retrieve the rate column for the current group directly within cur_data()
          rate <- get(paste0(cur_column(), "_rr"))

          # Retrieve the k-value from the k_defaults list based on the group
          k_value <- k_defaults[[group]]

          # Apply the adjustment formula if the rate is not missing or zero
          if_else(
            !is.na(rate) & rate != 0,
            round(. * (1 + (1 / (rate / 100) - 1) * k_value), 1),
            .
          )
        }
      )
    ) %>%
    add_outlier5std_column(all_indicators) %>%
    mutate(
      across(
        all_of(all_indicators),
        ~ {
          outlier <- get(paste0(cur_column(), "_outlier5std"))
          med <- round(median(if_else(outlier != 1, ., NA_real_), na.rm = TRUE), 0)

          if_else(outlier == 1, robust_max(med), .)
        }
      ),
      across(
        all_of(all_indicators),
        ~ {
          med <- round(median(if_else(!is.na(.), ., NA_real_), na.rm = TRUE), 0)
          max_med <- robust_max(med)
          if_else(
            is.na(.) & !is.na(max_med),
            max_med,
            .
          )
        }
      ),
      .by = c(district, year)
    ) %>%
    select(-any_of(paste0(all_indicators, "_rr")))

  new_countdown(merged_data, "cd_adjusted_data")
}
