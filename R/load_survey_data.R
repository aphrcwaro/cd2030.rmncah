#' Load UN Estimates
#'
#' Loads and processes UN estimates from a `.dta` file or in-memory data.
#' Filters by country and year, renames columns, and assigns a custom class.
#'
#' @param path Optional. File path to a `.dta` file.
#' @param .data Optional. A preloaded data frame.
#' @param country_iso Character. ISO3 code of the country.
#' @param start_year, end_year Integers. Year range to include.
#'
#' @return A tibble of class `cd_un_estimates`.
#' @export
load_un_estimates <- function(path = NULL, .data = NULL, country_iso, start_year, end_year) {
  year <- iso3 <- country <- countrycode <- iso2 <- NULL

  check_required(country_iso)
  check_required(start_year)
  check_required(end_year)

  data <- load_data_or_file(path, .data)
  data %>%
    filter(year >= start_year & year <= end_year, iso3 == country_iso) %>%
    rename_with(~ paste0("un_", .), -c(country, year, countrycode, iso3, iso2)) %>%
    new_tibble(class = "cd_un_estimates")
}

#' Load and Filter UN Mortality Estimates
#'
#' Loads UN mortality data from a file path or existing data frame, filters it by the given country ISO3 code,
#' and returns cleaned mortality data sorted by year.
#'
#' @param path Optional. Path to an Excel or CSV file containing UN mortality estimates.
#' @param .data Optional. A pre-loaded data frame with UN mortality estimates.
#' @param country_iso Required. A 3-letter ISO country code used to filter the dataset.
#'
#' @return A tibble of class `cd_un_mortality`.
#'
#' @examples
#' \dontrun{
#'   load_un_mortality_data("data/un_mortality.csv", country_iso = "KEN")
#'   load_un_mortality_data(.data = df, country_iso = "UGA")
#' }
#'
#' @export
load_un_mortality_data <- function(path = NULL, .data = NULL, country_iso) {
  check_required(country_iso)
  data <- load_data_or_file(path, .data)

  data %>%
    filter(isocode == country_iso) %>%
    arrange(year) %>%
    select(-countryname, -unicefregion, -isocode) %>%
    new_tibble(class = "cd_un_mortality")
}

#' Load and Process Survey Data
#'
#' Loads national or subnational survey data, cleans labels and codes, and applies
#' country-specific adjustments for admin-level naming.
#'
#' @param path Optional. File path to a `.dta` file.
#' @param .data Optional. A preloaded data frame.
#' @param country_iso Character. Country ISO3 code.
#' @param admin_level Character. One of `'national'` or `'adminlevel_1'`.
#'
#' @return A tibble of class `cd_survey_data`.
#' @export
load_survey_data <- function(path = NULL, .data = NULL, country_iso, admin_level = c("national", "adminlevel_1")) {
  year <- iso <- adminlevel_1 <- admin1_code <- NULL

  check_required(country_iso)
  data <- load_data_or_file(path, .data, country_iso)
  admin_level <- arg_match(admin_level)

  data <- data %>%
    select(-contains("24_35"))

  if (admin_level == "adminlevel_1") {
    data <- data %>%
      # mutate(across(matches('^r_|^se_|^ll_|^ul_'), ~ .)) %>%
      select(-any_of(c("adminlevel_1", "admin1_code"))) %>%
      separate_wider_regex(cols = level, patterns = c("[0-9]*", "\\s*", adminlevel_1 = ".*")) %>%
      filter(!(iso3 == "TZA" & adminlevel_1 %in% c(
        "Kaskazini Unguja", "Pemba North",
        "Kusini Pemba", "Pemba South", "Pemba",
        "Kusini Unguja", "Zanzibar North",
        "Mjini Magharibi", "Zanzibar South",
        "Rest Zanzibar", "Town West"
      ))) %>%
      mutate(
        # admin1_code = as.integer(admin1_code),
        adminlevel_1 = if_else(iso3 == "KEN", str_replace(adminlevel_1, "/", ""), adminlevel_1),
        adminlevel_1 = if_else(iso3 == "KEN", str_replace(adminlevel_1, "-", " "), adminlevel_1),
        adminlevel_1 = if_else(iso3 == "KEN", str_replace(adminlevel_1, " City", ""), adminlevel_1)
      )
  }

  new_tibble(
    data,
    class = "cd_survey_data"
  )
}

#' Load and Filter Equity Data
#'
#' Loads survey equity data, removes unnecessary variables, and filters by country.
#'
#' @param path Optional. File path to a `.dta` file.
#' @param .data Optional. A preloaded data frame.
#' @param country_iso Character. ISO3 country code to filter by.
#'
#' @return A tibble of class `cd_equity_data`.
#' @export
load_equity_data <- function(path = NULL, .data = NULL, country_iso) {
  year <- NULL

  check_required(country_iso)

  data <- load_data_or_file(path, .data, country_iso)

  data %>%
    separate_wider_regex(cols = level, patterns = c("[0-9]*", "\\s*", level = ".*")) %>%
    select(-contains("24_35")) %>%
    new_tibble(class = "cd_equity_data")
}

#' Load WUENIC Immunization Data
#'
#' Loads and processes WUENIC estimates, computes dropout and zero-dose indicators,
#' and standardizes naming conventions.
#'
#' @param path Optional. File path to a `.dta` file.
#' @param .data Optional. A preloaded data frame.
#' @param country_iso Character. ISO3 country code.
#'
#' @return A tibble of class `cd_wuenic_data`.
#' @export
load_wuenic_data <- function(path = NULL, .data = NULL, country_iso) {
  check_required(country_iso)

  data <- load_data_or_file(path, .data)

  data %>%
    filter(iso == country_iso) %>%
    # Standardize column names
    rename_with(~ str_replace(., "^cov_cov", "cov")) %>%
    rename_with(~ str_replace(., "^wuenic_(.*)", "\\1_wuenic")) %>%
    rename_with(~ str_replace(., "^(.*)_wuenic$", "cov_\\1_wuenic")) %>%
    rename_with(~ str_replace(., "^cov_cov", "cov")) %>%
    # Drop existing derived columns
    select(-matches("undervax|dropout|zerodose|cov_dropout_(measles|penta|penta3mcv1|penta1mcv1)_wuenic")) %>%
    # Compute immunization indicators
    mutate(
      # Zero-dose indicator
      cov_zerodose_wuenic = 100 - cov_penta1_wuenic,

      # Undervax indicator
      cov_undervax_wuenic = 100 - cov_penta3_wuenic,

      # Dropout indicators
      cov_dropout_penta13_wuenic = ((cov_penta1_wuenic - cov_penta3_wuenic) / cov_penta1_wuenic) * 100,
      cov_dropout_measles12_wuenic = ((cov_measles1_wuenic - cov_measles2_wuenic) / cov_measles1_wuenic) * 100,
      cov_dropout_penta3mcv1_wuenic = ((cov_penta3_wuenic - cov_measles1_wuenic) / cov_penta3_wuenic) * 100,
      cov_dropout_penta1mcv1_wuenic = ((cov_penta1_wuenic - cov_measles1_wuenic) / cov_penta1_wuenic) * 100
    ) %>%
    new_tibble(class = "cd_wuenic_data")
}

#' Load Data from File or Use Provided Data
#'
#' Loads data from a `.dta` file or uses provided data directly. Optionally
#' assigns a custom class to the returned tibble.
#'
#' @param path Optional. File path to a `.dta` file.
#' @param .data Optional. A preloaded data frame or tibble.
#' @param country_iso Optional. Character. ISO3 for the country.
#'
#' @return A tibble, optionally with custom class.
#' @noRd
load_data_or_file <- function(path = NULL, .data = NULL, country_iso = NULL) {
  if (!is.null(path)) {
    check_file_path(path)
    ext <- tools::file_ext(path)
    if (tolower(ext) != "dta") {
      cd_abort(c("x" = "Only {.val .dta} files are supported."))
    }
    .data <- haven::read_dta(path)
    if (!is.null(country_iso)) {
      .data <- .data %>% mutate(iso3 = country_iso)
    }
  }

  check_required(.data)

  .data %>%
    filter(if (is.null(country_iso)) TRUE else iso3 == country_iso)
}

#' Load FPET Data from CSV File or Use Provided Data
#'
#' `load_fpet_data` loads Family Planning Estimation Tool (FPET) data from a `.csv`
#' file, or uses a pre-supplied dataset. It assigns a country ISO code if specified,
#' and returns a tibble with a custom class `cd_fpet_data`.
#'
#' @param path Optional `character`. File path to a `.csv` file. If provided,
#'   data is read from the file.
#' @param .data Optional `data.frame` or `tibble`. Used directly if provided
#'   instead of loading from file.
#' @param country_iso Optional `character`. ISO3 country code. Used to tag or
#'   filter the dataset.
#'
#' @return A tibble with class `cd_fpet_data`. If `country_iso` is provided, the
#'   data is filtered by `iso3 == country_iso`.
#'
#' @examples
#' \dontrun{
#' # Load FPET data from file
#' fpet <- load_fpet_data(path = "path/to/fpet.csv", country_iso = "NGA")
#'
#' # Use existing data
#' fpet <- load_fpet_data(.data = some_fpet_data, country_iso = "ETH")
#' }
#'
#' @export
load_fpet_data <- function(path = NULL, .data = NULL, country_iso = NULL) {

  if (!is.null(path)) {
    check_file_path(path)
    ext <- tools::file_ext(path)
    if (tolower(ext) != "csv") {
      cd_abort(c("x" = "Only {.val .csv} files are supported."))
    }
    .data <- read_csv(path)
    if (!is.null(country_iso)) {
      .data <- .data %>% mutate(iso3 = country_iso)
    }
  }

  check_required(.data)

  .data %>%
    rename(country = 'Country/Population') %>%
    filter(if (is.null(country_iso)) TRUE else iso3 == country_iso) %>%
    new_tibble(class = "cd_fpet_data")
}

#' Load and Prepare Private Sector Data
#'
#' Loads raw prevalence data for public/private sector analysis and applies
#' internal preprocessing for c-section share calculation.
#'
#' @param path Optional. File path to a `.dta` file. Required if `.data` is not provided.
#' @param .data Optional. A pre-loaded dataset. Used directly if provided.
#' @param country_iso Required. ISO3 country code to filter the data.
#' @param level Either `"national"` or `"area"` indicating the data level.
#'
#' @return A tibble of class `"cd_private_sector_data"` with a `"level"` attribute.
#'
#' @details
#' This function:
#' - Loads and filters data for the specified `country_iso`.
#' - Assigns `"Public"` or `"Private"` to the `sector` column based on `indic` suffix.
#' - Removes sector suffix from `indic`.
#' - Computes number of c-sections (`num_csection`) and sector share of c-sections (`share_csection`)
#'   within groupings defined by `iso` or `iso + area`.
#' - If level is `"area"`, parses `level` string into `area = "Urban"` or `"Rural"`.
#'
#' @section Output columns:
#' - `indic`: cleaned indicator name (without "pub"/"priv")
#' - `sector`: `"Public"` or `"Private"`
#' - `share_csection`: share of c-sections attributable to each sector
#' - `area`: (only if `level == "area"`)
#'
#' @seealso [load_csection_estimates()], [prepare_private_sector_plot_data()]
#'
#' @examples
#' \dontrun{
#' dt <- load_private_sector_data(
#'   path = "National estimates.dta",
#'   country_iso = "KEN",
#'   level = "national"
#' )
#' }
#'
#' @export
load_private_sector_data  <- function(path = NULL, .data = NULL, country_iso, level = c('national', 'area')) {
  check_required(country_iso)
  level <- arg_match(level)
  level_cols <- switch (
    level,
    national = NULL,
    area = 'level'
  )

  data <- load_data_or_file(path, .data)

  if (level == 'national' && 'level' %in% names(data)) {
    cd_abort(c('x' = 'Area data loaded with the {.arg {level}} level.'))
  }

  if (level == 'area' && !'level' %in% names(data)) {
    cd_abort(c('x' = 'National data loaded with the {.arg {level}} level.'))
  }

  data <- data %>%
    filter(iso == country_iso) %>%
    mutate(
      sector = case_when(
        str_ends(indic, 'pub') ~ 'Public',
        str_ends(indic, 'priv') ~ 'Private'
      ),
      indic = str_remove(indic, '(pub|priv)$'),
      num_csection = if_else(indic == 'csection', r_raw * N, NA_real_)
    ) %>%
    mutate(
      total_csection = sum(if_else(indic == 'csection', num_csection, 0), na.rm = TRUE),
      share_csection = num_csection / total_csection,
      .by = level_cols
    )

  if (level == 'area') {
    data <- data %>%
      separate_wider_regex(level, patterns = c("\\d*", "\\s*", area = ".*"))
  }

  data %>%
    new_tibble(class = "cd_private_sector_data", level = level)
}

#' Load C-section Share Estimates
#'
#' Loads c-section correction estimates for national or area-level adjustment.
#'
#' @param path Optional. File path to a `.dta` file. Required if `.data` is not provided.
#' @param .data Optional. A pre-loaded dataset. Used directly if provided.
#' @param country_iso Required. ISO3 country code to filter the data.
#' @param level Either `"national"` or `"area"` indicating the data level.
#'
#' @return A tibble of class `"cd_csection_estimates"` with a `"level"` attribute.
#'
#' @details
#' - Filters data for the specified `country_iso`.
#' - Validates whether the data level matches `"national"` or `"area"`.
#' - For `"area"` level, parses the `level` column to extract `area`.
#' - Returns only relevant columns for merging (`iso`, `year`, `indic`, and either `national` or `area_est`).
#'
#' @seealso [load_private_sector_data()], [prepare_private_sector_plot_data()]
#'
#' @examples
#' \dontrun{
#' cs <- load_csection_estimates(
#'   path = "csection_national.dta",
#'   country_iso = "KEN",
#'   level = "national"
#' )
#' }
#'
#' @export
load_csection_estimates <- function(path = NULL, .data = NULL, country_iso, level = c('national', 'area')) {
  check_required(country_iso)

  data <- load_data_or_file(path, .data)

  level <- arg_match(level)
  if (level == 'national' && 'level' %in% names(data)) {
    cd_abort(c('x' = 'Area data loaded with the {.arg {level}} level.'))
  }

  if (level == 'area' && !'level' %in% names(data)) {
    cd_abort(c('x' = 'National data loaded with the {.arg {level}} level.'))
  }

  data <- data %>%
    filter(iso == country_iso)

  if (level == 'area') {
    data <- data %>%
      separate_wider_regex(level, patterns = c("\\d*", "\\s*", area = ".*"))
  }

  data %>%
    select(iso, year, indic, any_of(c('area', 'national', 'area_est'))) %>%
    new_tibble(class = "cd_csection_estimates", level = level)
}

