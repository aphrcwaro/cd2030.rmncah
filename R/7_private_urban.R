#' Prepare Private Sector Plot Data
#'
#' Processes reshaped private sector data and c-section correction estimates
#' to produce a tidy dataset for plotting public/private sector shares.
#'
#' @param .data A data frame of class `"cd_private_sector_data"` returned by
#'   [load_private_sector_data()]. Must include `share_csection`, `r_raw`, and `sector` columns.
#' @param csection_data A data frame of class `"cd_csection_estimates"` returned by
#'   [load_csection_estimates()]. Must match the level of `.data` (`"national"` or `"area"`).
#'
#' @return A tibble of class `"cd_private_sector_plot_data"` with attributes:
#'   - `level`: `"national"` or `"area"`
#'   - Columns reshaped from wide to include `r_raw_Public`, `r_raw_Private`, etc.
#'   - Computed columns:
#'     - `privateShare`: percent share of private sector
#'     - `totalPrevalence`: total prevalence (public + private)
#'
#' @details
#' This function:
#'
#' - Joins private sector data with correction estimates using `iso`, `year`, and `indic`.
#' - For `"csection"` rows, applies adjustment: `r_raw = estimate * share_csection`.
#' - Drops intermediate columns and reshapes data to wide format.
#' - Calculates private sector share and total prevalence.
#'
#' Used as input to [plot.cd_private_sector_plot_data()] for visualisation.
#'
#' @examples
#' \dontrun{
#' dt <- load_private_sector_data("National estimates.dta", country_iso = "KEN", level = "national")
#' cs <- load_csection_estimates("csection_national.dta", country_iso = "KEN", level = "national")
#' plot_data <- prepare_private_sector_plot_data(dt, cs)
#' }
#'
#' @export
prepare_private_sector_plot_data <- function(.data, csection_data) {
  check_cd_class(.data, 'cd_private_sector_data')
  check_cd_class(csection_data, 'cd_csection_estimates')

  data_level <- attr_or_abort(.data, 'level')
  csection_level <- attr_or_abort(csection_data, 'level')

  if (data_level != csection_level) {
    cd_abort(c('x' = 'Both {arg .data} and {.arg csection_data} should be of the same level.'))
  }

  by_cols <- c('iso', "year", "indic")
  if (data_level == "area") by_cols <- c(by_cols, "area")

  correction_col <- if (data_level == "national") "national" else "area_est"

  .data %>%
    left_join(csection_data, by = by_cols) %>%
    mutate(
      r_raw = if_else(indic == "csection", !!sym(correction_col) * share_csection, r_raw)
    ) %>%
    select(-num_csection, -total_csection, -share_csection, -all_of(correction_col)) %>%
    pivot_wider(
      names_from = sector,
      values_from = c(r_raw, se_raw, ll, ul, pop, N),
      names_glue = "{.value}_{sector}"
    ) %>%
    mutate(
      privateShare = r_raw_Private / (r_raw_Public + r_raw_Private) * 100,
      totalPrevalence = r_raw_Private + r_raw_Public
    ) %>%
    new_tibble(class = "cd_private_sector_plot_data", level = data_level)
}
