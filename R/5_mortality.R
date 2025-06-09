#' Create Mortality Summary Object
#'
#' Combines national and admin1 summaries and formats as a custom class for plotting.
#'
#' @param .data An object of class `cd_data`.
#'
#' @return A `cd_mortality_rare` tibble.
#'
#' @export
create_mortality_summary <- function(.data) {
  check_cd_data(.data)

  scatter <- bind_rows(
    summarised_data(.data, 'adminlevel_1'),
    summarised_data(.data, 'national')
  ) %>%
    mutate(adminlevel_1 = if_else(is.na(adminlevel_1), 'National', adminlevel_1))

  new_tibble(
    scatter,
    class = 'cd_mortality_summary'
  )
}

#' Extract Latest Mortality Ratios for UN Comparison
#'
#' Joins computed national mortality indicators with UN estimates, returns latest value per indicator.
#'
#' @param .data The main mortality dataset.
#' @param mortality_data A `cd_un_mortality` object.
#'
#' @return A tibble of class `cd_mortality_ratio` with latest available values and UN means.
#'
#' @export
create_mortality_ratios <- function(.data, mortality_data) {
  check_cd_class(.data, 'cd_mortality_summary')
  check_un_mortality(mortality_data)

  un_national <- .data %>%
    filter(adminlevel_1 == 'National') %>%
    select(year, sbr_inst, mmr_inst) %>%
    mutate(
      mean_mmr = mean(mmr_inst, na.rm = TRUE),
      mean_sbr = mean(sbr_inst, na.rm = TRUE)
    ) %>%
    left_join(mortality_data, join_by(year)) %>%
    select(-contains('inst'), -contains('nmr'))

  un_national %>%
    select(-contains('mean')) %>%
    pivot_longer(cols = -year, names_to = 'indicator', values_to = 'values') %>%
    group_by(indicator) %>%
    filter(!is.na(values)) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    left_join(un_national %>% select(year, contains('mean')), join_by(year)) %>%
    new_tibble(class = 'cd_mortality_ratio')
}

#' Filter and Prepare Mortality Rate Data for Mapping
#'
#' Filters a `cd_mortality_summary` object by country, indicator, and year. Optionally joins
#' subnational mapping data and renames geometry for spatial plotting.
#'
#' @param .data A `cd_mortality_summary` object created by [create_mortality_summary()].
#' @param country_iso A character string. ISO3 code of the country.
#' @param indicator Character. Mortality indicator to filter (`"mmr"` or `"sbr"`). Defaults to `"mmr"`.
#' @param plot_year Optional integer or vector of years to filter.
#' @param subnational_map Optional. A data frame to join with the shapefile to add metadata.
#'
#' @return A tibble of class `cd_mortality_summary_filtered`, ready for geospatial plotting.
#'
#' @details
#' This function:
#' - Selects the specified mortality indicator (`mmr_inst` or `sbr_inst`)
#' - Filters by `plot_year` if provided
#' - Merges mortality rates with spatial geometry using internal or custom subnational mappings
#' - Ensures geometry is correctly renamed for mapping (`geometry`)
#'
#' @examples
#' \dontrun{
#' filter_mortality_rate(
#'   mortality_data,
#'   country_iso = "NGA",
#'   indicator = "sbr",
#'   plot_year = 2021:2023
#' )
#' }
#'
#' @export
filter_mortality_summary <- function(.data, country_iso, indicator = c('mmr', 'sbr'), plot_year = NULL, subnational_map = NULL) {

  check_cd_class(.data, expected_class = 'cd_mortality_summary')
  check_required(country_iso)
  indicator <- arg_match(indicator)
  indicator <- paste0(indicator, '_inst')

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
    filter(if (is.null(plot_year)) TRUE else year %in% plot_year) %>%
    select(any_of(c('adminlevel_1', 'district', 'year', indicator)), starts_with('geom')) %>%
    rename_with(~ 'geometry', starts_with('geom'))

  new_tibble(
    merged_data,
    class = 'cd_mortality_summary_filtered',
    indicator = indicator
  )
}

#' Summarise Mortality and Birth Data by Administrative Level
#'
#' Aggregates key mortality and birth statistics by year and admin level,
#' and computes key indicators like MMR, SBR, and neonatal death rate.
#'
#' @param .data A data frame containing `instlivebirths`, `total_stillbirth`, `maternal_deaths`, `neonatal_deaths`, and `year`.
#' @param admin_level One of `"national"`, `"adminlevel_1"`, or `"district"`.
#'
#' @return A summarised data frame with calculated rates.
#'
#' @noRd
summarised_data <- function(.data, admin_level = c('national', 'adminlevel_1', 'district')) {
  check_cd_data(.data)
  admin_level <- arg_match(admin_level)
  admin_level_col <- get_admin_columns(admin_level)

  .data %>%
    summarise(
      across(c(instlivebirths, total_stillbirth, maternal_deaths, neonatal_deaths), sum, na.rm = TRUE),
      .by = all_of(c(admin_level_col, 'year'))
    ) %>%
    mutate(
      sbr_inst = 1000 * total_stillbirth/(instlivebirths+total_stillbirth),
      mmr_inst = 100000 * maternal_deaths/instlivebirths,
      nn_inst = 1000 * neonatal_deaths/instlivebirths,
      ratio_md_sb = total_stillbirth/maternal_deaths,
      mmr_low = if_else(mmr_inst < 25, 1, 0),
      sbr_low = if_else(sbr_inst < 6, 1, 0)
    ) %>%
    relocate(all_of(c(admin_level_col, 'year')))
}

#' Summarize Completeness-Adjusted Mortality Ratio
#'
#' Computes and summarizes adjusted mortality ratio estimates based on completeness
#' correction factors for different completeness-to-incompleteness ratios (CI ratios).
#'
#' @param .data A data frame of class `cd_mortality_ratio`, containing UN estimates
#'   and observed ratios.
#' @param plot_type Character. The type of mortality ratio to summarize. Must be one of:
#'   - `"mmr"`: Maternal mortality ratio
#'   - `"sbr"`: Stillbirth rate
#' @param lbr_mean Numeric. Mean live birth registration completeness, used for
#'   scaling completeness ratios. Default is `0`.
#'
#' @return A tibble of class `cd_mortality_ratio_summarised` with columns:
#'   - `ciratio`: The completeness-to-incompleteness ratio used.
#'   - `name`: Label for the UN estimate type (`lower bound`, `best estimate`, `upper bound`).
#'   - `rat`: The adjusted mortality ratio value.
#'
#' @examples
#' \dontrun{
#' summarise_completeness_ratio(mortality_data, plot_type = "mmr", lbr_mean = 65)
#' }
#'
#' @export
summarise_completeness_ratio <- function(.data, plot_type = c('mmr', 'sbr'), lbr_mean = 0) {

  check_cd_class(.data, expected_class = 'cd_mortality_ratio')

  plot_type <- arg_match(plot_type)

  mean_col <- paste0('mean_', plot_type)
  lower_bound <- paste0('UN ', str_to_upper(plot_type),' lower bound')
  upper_bound <- paste0('UN ', str_to_upper(plot_type),' upper bound')
  best_estimates <- paste0('UN ', str_to_upper(plot_type),' best estimate')

  data <- .data %>%
    filter(str_detect(indicator, plot_type)) %>%
    pivot_wider(names_from = indicator, values_from = values) %>%
    select(year, contains(plot_type)) %>%
    crossing(
      ciratio = c(0.5, 1, 1.5, 2)
    ) %>%
    pivot_longer(col = starts_with(plot_type)) %>%
    mutate(
      rat = ((!!sym(mean_col) * 100) / (value / (ciratio - (ciratio - 1) * (lbr_mean / 100))))
    ) %>%
    select(ciratio, name, rat) %>%
    mutate(
      name = case_match(
        name,
        paste0(name, '_lb') ~ lower_bound,
        paste0(name, '_ub') ~ upper_bound,
        name ~ best_estimates,
        .ptype = factor(levels = c(lower_bound, best_estimates, upper_bound))
      ),
      rat = round(rat, 1)
    ) %>%
    arrange(name) %>%
    pivot_wider(names_from = name, values_from = rat)

  new_tibble(
    data,
    class = 'cd_mortality_ratio_summarised',
    plot_type = plot_type
  )
}

# Extract summary statistics from UN data using tidy logic
extract_un_summary <- function(un_data) {
  un_data %>%
    pivot_longer(cols = -year, names_to = 'indicator', values_to = 'values') %>%
    group_by(indicator) %>%
    filter(!is.na(values)) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    mutate(
      level = case_when(
        str_detect(indicator, 'lb') ~ '::Lower bound of the estimate',
        str_detect(indicator, 'ub') ~ '::Upper bound of the estimate',
        .default = 'UN best estimate, 2020/2022'
      ),
      indicator = str_to_upper(str_remove(indicator, "_(lb|ub)$"))
    ) %>%
    arrange(year) %>%
    pivot_wider(
      names_from = c(indicator, year),
      values_from = values,
      names_glue = '{indicator}({year})'
    )
}

aggregate_indicator <- function(.data, indicators) {
  .data %>%
    pivot_longer(cols = any_of(indicators), names_to = 'indicator', values_to = 'val') %>%
    summarise(val = sum(val, na.rm = TRUE), .by = c(year, indicator)) %>%
    pivot_wider(names_from = year, values_from = val) %>%
    relocate(indicator)
}



derive_ratio <- function(.data, admin_level = c('national', 'adminlevel_1', 'district')) {
  admin_level <- arg_match(admin_level)
  admin_level_col <- get_admin_columns(admin_level)

  summarised_data(dat, admin_level) %>%
    select(all_of(admin_level_col), year, mmr_inst, sbr_inst, nn_inst, ratio_md_sb) %>%
    pivot_longer(cols = -all_of(c(admin_level_col, 'year')), values_to = 'values', names_to = 'indicator') %>%
    pivot_wider(names_from = year, values_from = values)
}

threshold_metric <- function(.data, admin_level = c('adminlevel_1', 'district')) {
  admin_level <- arg_match(admin_level)

  .data %>%
    summarised_data(admin_level) %>%
    summarise(
      mmr_lowr = mean(mmr_low, na.rm = TRUE) * 100,
      sbr_lowr = mean(sbr_low, na.rm = TRUE) * 100,
      .by = year
    ) %>%
    pivot_longer(cols = -year, values_to = 'values', names_to = 'indicator') %>%
    pivot_wider(names_from = year, values_from = values)
}

calculate_summary <- function(.data) {
  check_cd_data(.data)

  bind_rows(
    # To add stillbirth rate for the latest year
    aggregate_indicator(dat, c('instlivebirths', 'total_stillbirth', 'maternal_deaths', 'neonatal_deaths')),
    derive_ratio(dat),
    threshold_metric(dat)
  )
}
