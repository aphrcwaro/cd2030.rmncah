#' Add Outlier Flags Based on 5-MAD Bounds
#'
#' `add_outlier5std_column` calculates and appends outlier flags for specified
#' indicators in a dataset. An outlier is defined as a value falling outside the
#' range defined by the median ± 5 times the Median Absolute Deviation (MAD).
#' This method is applied on a per-group basis, allowing flexibility for grouping
#' by columns like `district`.
#'
#' @param .data A `cd_data` tibble containing health indicator data.
#' @param indicators A character vector specifying the names of the indicator
#'   columns to analyze for outliers.
#' @param group_by A character string or vector specifying the column(s) to group
#'   by when calculating the median and MAD (default is `'district'`).
#'
#' @return A tibble with additional columns for each indicator, named
#'   `{indicator}_outlier5std`, containing a value of `1` if the observation is
#'   an outlier and `0` otherwise.
#'
#' @details
#' - **Median and MAD Calculation**:
#'   For each indicator, the median and MAD are calculated using data from years
#'   prior to the most recent year in the dataset.
#'   The last year is excluded to prevent potential contamination of the baseline
#'   statistics.
#' - **Outlier Definition**:
#'   A value is flagged as an outlier if it falls outside the range:
#'   Lower Bound = median - 5 * MAD
#'   Lower Bound = median + 5 * MAD
#' - **Grouping**:
#'   Calculations are performed separately for each group specified by the
#'   `group_by` parameter.
#' - **Generated Columns**:
#'   For each indicator, the function generates an outlier flag column named
#'   `{indicator}_outlier5std`. The column contains `1` if the value is an outlier
#'   and `0` otherwise.
#'
#' @examples
#' \dontrun{
#' # Add missing value flags for all indicators
#' add_outlier5std_column(data,
#'   indicators = "indicator1",
#'   group_by = "district"
#' )
#' }
#'
#' @seealso
#' [add_mad_med_columns()] for computing and appending the median and MAD columns.
#'
#' @export
add_outlier5std_column <- function(.data, indicators, group_by = "district") {
  district <- NULL

  # check_cd_data(.data)
  check_required(.data)
  check_required(indicators)
  check_required(group_by)

  .data %>%
    add_mad_med_columns(indicators, group_by) %>%
    mutate(
      # Step 2: Calculate outlier flags based on bounds
      across(
        all_of(indicators),
        ~ {
          med <- get(paste0(cur_column(), "_med"))
          mad <- get(paste0(cur_column(), "_mad"))

          lower_bound <- round(med - 5 * mad, 1)
          upper_bound <- round(med + 5 * mad, 1)

          if_else(!is.na(.) & (. < lower_bound | . > upper_bound), 1, 0)
        },
        .names = "{.col}_outlier5std"
      ),
      .by = {{ group_by }}
    )
}

#' Add Median and MAD Columns for Indicators
#'
#' `add_mad_med_columns` calculates and appends the median and Median Absolute
#' Deviation (MAD) for specified indicators in a dataset. These statistics are
#' calculated per group, allowing the user to dynamically group data (e.g., by
#' `district`) and exclude data from the most recent year in the dataset.
#'
#' @param .data A `cd_data` tibble containing health indicator data.
#' @param indicators A character vector specifying the names of the indicator
#'   columns for which the median and MAD should be calculated.
#' @param group_by A character string or vector specifying the column(s) to group
#'   by when calculating the median and MAD (default is `'district'`).
#'
#' @return A tibble with additional columns for each indicator, named
#'   `{indicator}_med` and `{indicator}_mad`, containing the calculated median
#'   and MAD, respectively.
#'
#' @details
#' - The median and MAD are calculated for each group specified by `group_by`.
#' - Only data from years prior to the most recent year in the dataset are considered.
#' - Missing values in the calculated statistics are replaced using `robust_max()`,
#'   ensuring that meaningful fallback values are provided.
#'
#' @examples
#' \dontrun{
#' # Add median and MAD columns for 'indicator1'
#' add_mad_med_columns(data,
#'   indicators = "indicator1",
#'   group_by = "district"
#' )
#' }
#'
#' @seealso
#'
#' - [add_outlier5std_column()] for generating outlier flags based on these columns.
#' - [robust_max()] for calculating max value
#'
#'
#' @export
add_mad_med_columns <- function(.data, indicators, group_by = "district") {
  # Check that the input data and required columns are valid
  # check_cd_data(.data)
  check_required(.data)
  check_required(indicators)

  # Validate group_by, year, and indicators columns
  if ((!is.null(group_by) && !all(group_by %in% colnames(.data))) ||
    !"year" %in% colnames(.data) ||
    !all(indicators %in% colnames(.data))) {
    cd_abort(c("x" = "{.arg .data} must contain the columns specified by {.arg group_by} and {.field indicators}"))
  }

  # Determine the last year in the data
  last_year <- robust_max(.data$year)

  # Add median and MAD columns
  .data %>%
    mutate(
      across(
        all_of(indicators),
        list(
          med = ~ {
            values <- if_else(year < last_year, ., NA_real_)
            med <- median(values, na.rm = TRUE)
            if_else(is.na(med), robust_max(.), med) |> round(1)
          },
          mad = ~ {
            values <- if_else(year < last_year, ., NA_real_)
            mad_val <- mad(values, na.rm = TRUE)
            if_else(is.na(mad_val), robust_max(.), mad_val) |> round(1)
          }
        ),
        .names = "{.col}_{.fn}"
      ),
      .by = {{ group_by }}
    )
}

#' Add Missing Value Flags to Health Indicators
#'
#' `add_missing_column` assesses data quality by identifying missing values for
#' specified health indicators. It generates new columns for each indicator,
#' flagging values as 'Missing' (`1`) or 'Non-Missing' (`0`) to support downstream
#' quality control and analysis.
#'
#' @param .data A data frame containing health indicator data.
#' @param indicators A character vector specifying the names of the indicator
#'   columns to analyze for outliers.
#'
#' @details
#' - **Missingness Assessment**:
#'   Each indicator is processed to flag missing values (`NA`) with `1` (missing)
#'   or `0` (non-missing). The resulting columns are prefixed with `mis_` followed
#'   by the original indicator name (e.g., `mis_indicator1`).
#' - **Group Preservation**:
#'   The output retains the `district` and `year` columns to ensure grouping information
#'   remains intact.
#'
#' @return
#' A data frame containing:
#' - **Missingness Flags**:
#'   For each indicator, a column prefixed with `mis_` indicating missingness (`1` for
#'   missing, `0` otherwise).
#' - `district` and `year` columns to maintain grouping information.
#'
#' @examples
#' \dontrun{
#' # dd missing value flags for all indicators
#' add_missing_column(data)
#' }
#'
#' @export
add_missing_column <- function(.data, indicators) {
  district <- year <- NULL

  # Check that the data is valid
  # check_cd_data(.data)
  check_required(.data)
  check_required(indicators)

  if (!all(indicators %in% colnames(.data))) {
    cd_abort(c("x" = "{.arg {.data} must contain the columns specified by {.field indicators}"))
  }

  # Add missingness flags for each indicator
  .data %>%
    mutate(
      across(all_of(indicators), ~ if_else(is.na(.), 1, 0), .names = "mis_{.col}")
    )
}

#' Robust Maximum Value Calculation
#'
#' `robust_max` calculates the maximum value of a numeric vector while handling
#' missing values. If all values in the vector are `NA`, the function returns `NA`
#' instead of raising an error.
#'
#' @param x A numeric vector.
#' @param fallback argument allows you to specify a value to return when all elements are NA
#'
#' @return The maximum value of `x`, or `NA` if all values are `NA`.
#'
#' @examples
#' # Example usage
#' robust_max(c(1, 2, 3, NA)) # Returns 3
#' robust_max(c(NA, NA)) # Returns NA
#' robust_max(c(-Inf, 0, 10)) # Returns 10
#'
#' @export
robust_max <- function(x, fallback = NA) {
  finite_values <- x[is.finite(x)]

  if (length(finite_values) == 0) {
    return(fallback)
  } else {
    max(finite_values)
  }
}
