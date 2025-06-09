#' Create a CacheConnection Object
#'
#' `init_CacheConnection` initializes a `CacheConnection` either from a provided `.rds` file path
#' or directly from a `cd_data` object. Only one of the arguments should be non-NULL.
#'
#' @param rds_path Optional character. Path to an RDS file to load the cache from.
#' @param countdown_data Optional `cd_data` object to initialize in-memory cache.
#'
#' @return An instance of the `CacheConnection` class.
#'
#' @export
init_CacheConnection <- function(rds_path = NULL, countdown_data = NULL) {
  CacheConnection$new(
    rds_path = rds_path,
    countdown_data = countdown_data
  )
}

#' Print Notes for a Specific Page and Object
#'
#' This function retrieves notes from the cache for a given `page_id` and `object_id`,
#' filters them for those marked as `include_in_report`, and prints them to the console.
#' If no notes are found, it prompts the user to enter notes for the given `page_id` and
#' `object_id`, optionally including additional parameters.
#'
#' @param cache An object (e.g., of class `CacheConnection`) that manages cached
#'   data, including notes.
#' @param page_id A character string specifying the page identifier for which to
#'   retrieve notes.
#' @param object_id An optional character string specifying the object identifier
#'   within the page. Defaults to `NULL`.
#' @param parameters An optional named list of additional parameters to filter or
#'   describe the notes.
#'
#' @return None. The function is used for its side effects (printing notes or
#'   prompting the user).
#'
#' @keywords internal
print_notes <- function(cache, page_id, object_id = NULL, parameters = NULL) {
  include_in_report <- note <- NULL

  # Retrieve the notes from the cache
  page_notes <- cache$get_notes(page_id, object_id, parameters) %>%
    filter(include_in_report == TRUE, !is.null(note))

  # Check if there are any notes
  if (nrow(page_notes) > 0) {
    page_notes %>%
      pull(note) %>%
      walk(~ cat(paste0(.x, '\n\n')))
  } else {
    # Format the parameters for the message
    parameters_str <- if (!is.null(parameters)) {
      paste0(' (parameters: ', paste(names(parameters), parameters, sep = ' = ', collapse = ', '), ')')
    } else {
      ''
    }

    cat(paste0('**--- Enter notes related to ', page_id, ' ', object_id, parameters_str, '. ---**\n\n'))
  }
}

#' CacheConnection Class
#'
#' @description
#' An R6 class that handles persistent or in-memory caching of data used in the Countdown 2030
#' analysis and reporting workflows. It supports tracking of various internal data objects,
#' reactive updates (for Shiny apps), note-taking for report annotations, and saving/loading
#' from `.rds` files.
#'
#' @docType class
#' @name CacheConnection
#' @format An [R6::R6Class] generator object.
#' @keywords internal
CacheConnection <- R6::R6Class(
  'CacheConnection',
  public = list(

    #' @description Initialize a CacheConnection instance.
    #' @param rds_path Path to the RDS file (can be NULL).
    #' @param countdown_data Countdown data of class `cd_data`.
    initialize = function(rds_path = NULL, countdown_data = NULL) {
      if (is.null(rds_path) && is.null(countdown_data)) {
        cd_abort(c('x' = 'Both {.arg rds_path} and {.arg countdown_data} cannot be null.'))
      }

      if (!is.null(rds_path) && !is.null(countdown_data)) {
        cd_abort(c('x' = 'Only one can have a value: {.arg rds_path} and {.arg countdown_data}.'))
      }

      if (!is.null(countdown_data)) {
        check_cd_data(countdown_data)
      }

      # Initialize in-memory data using the template
      private$.in_memory_data <- private$.data_template
      private$.in_memory_data$countdown_data <- countdown_data
      private$.in_memory_data$rds_path <- rds_path
      private$.in_memory_data$survey_source <- NULL

      if (!is.null(rds_path)) {
        self$load_from_disk()
      }
    },

    #' Load data from disk.
    #' @return None. Updates internal state.
    load_from_disk = function() {
      check_file_path(private$.in_memory_data$rds_path)

      loaded_data <- readRDS(private$.in_memory_data$rds_path)
      required_fields <- names(private$.data_template)
      missing_fields <- setdiff(required_fields, names(loaded_data))
      if (length(missing_fields) > 0) {
        cd_abort(c('x' = paste(
          'The following required fields are missing in the RDS file:',
          paste(missing_fields, collapse = ', ')
        )))
      }
      private$.in_memory_data <- loaded_data
    },

    #' Save data to disk (only if changed and RDS path is not NULL)
    #' @return None. Updates file.
    save_to_disk = function() {
      if (private$.has_changed && !is.null(private$.in_memory_data$rds_path)) { # Key change here
        saveRDS(private$.in_memory_data, private$.in_memory_data$rds_path)
        private$.has_changed <- FALSE
      }
    },

    #' @description Append a note to a page.
    #' @param page_id Page ID.
    #' @param object_id Object ID.
    #' @param note Note text.
    #' @param parameters Named list of parameters.
    #' @param include_in_report Logical flag.
    #' @param include_plot_table Logical flag.
    #' @param single_entry Logical flag for uniqueness.
    append_page_note = function(page_id, object_id, note, parameters = list(), include_in_report = FALSE, include_plot_table = FALSE, single_entry = FALSE) {
      # Validate inputs
      stopifnot(is.character(page_id), is.character(object_id), is.character(note))
      stopifnot(is.list(parameters), is.logical(include_in_report))

      # Create a new note
      new_note <- tibble(
        page_id = page_id,
        object_id = object_id,
        note = note,
        parameters = list(parameters), # Wrap in list for storage
        include_in_report = include_in_report, # Single logical value
        include_plot_table = include_plot_table
      )

      if (single_entry) {
        filtered_notes <- private$.in_memory_data$page_notes %>%
          filter(!(page_id == !!page_id & object_id == !!object_id))
      } else {
        filtered_notes <- private$.in_memory_data$page_notes %>%
          filter(
            !(page_id == !!page_id & object_id == !!object_id &
              map_lgl(parameters, ~ identical(.x, !!parameters)))
          )
      }

      # Append the new note to the page_notes tibble
      private$.in_memory_data$page_notes <- bind_rows(filtered_notes, new_note)

      # Mark data as changed and save to disk
      private$.has_changed <- TRUE
      self$save_to_disk()
    },

    #' @description Retrieve notes for a given page/object.
    #' @param page_id Page ID.
    #' @param object_id Object ID.
    #' @param parameters Named list of parameters.
    get_notes = function(page_id, object_id = NULL, parameters = NULL) {
      private$.in_memory_data$page_notes %>%
        filter(if (is.null(!!object_id)) TRUE else object_id == !!object_id) %>%
        filter(if (is.null(!!parameters)) TRUE else map_lgl(parameters, ~ identical(.x, !!parameters)))
    },

    #' @description Run coverage calculation using stored model parameters.
    #' @param admin_level Administrative level ("adminlevel_1" or "district").
    #' @param region Optional region filter.
    calculate_indicator_coverage = function(admin_level, region = NULL) {
      check_required(admin_level)

      if (!self$check_inequality_params) {
        cd_abort(c('x' = 'One or more parameters is missing for {.fun calculate_indicator_coverage}'))
      }

      rates <- self$national_estimates

      calculate_indicator_coverage(
        .data = self$adjusted_data,
        admin_level = admin_level,
        un_estimates = self$un_estimates,
        region = region,
        sbr = rates$sbr,
        nmr = rates$nmr,
        pnmr = rates$pnmr,
        anc1survey = rates$anc1,
        dpt1survey = rates$penta1,
        survey_year = self$survey_year,
        twin = rates$twin_rate,
        preg_loss = rates$preg_loss
      )
    },

    #' @description Run inequality calculation using stored model parameters.
    #' @param admin_level Administrative level ("adminlevel_1" or "district").
    #' @param region Optional region filter.
    calculate_inequality = function(admin_level, region = NULL) {
      check_required(admin_level)

      if (!self$check_inequality_params) {
        cd_abort(c('x' = 'One or more parameters is missing for {.fun calculate_inequality}'))
      }

      rates <- self$national_estimates

      calculate_inequality(
        .data = self$adjusted_data,
        admin_level = admin_level,
        un_estimates = self$un_estimates,
        region = region,
        sbr = rates$sbr,
        nmr = rates$nmr,
        pnmr = rates$pnmr,
        anc1survey = rates$anc1,
        dpt1survey = rates$penta1,
        survey_year = self$survey_year,
        twin = rates$twin_rate,
        preg_loss = rates$preg_loss
      )
    },

    #' @description Run coverage calculation using stored model parameters.
    #' @param admin_level Administrative level ("adminlevel_1" or "district").
    calculate_coverage = function(admin_level) {
      check_required(admin_level)

      if (!self$check_coverage_params) {
        cd_abort(c('x' = 'One or more parameters is missing for {.fun calculate_coverage}'))
      }

      rates <- self$national_estimates
      survey_data <- if (admin_level == 'national') self$national_survey else self$regional_survey

      calculate_coverage(
        .data = self$adjusted_data,
        admin_level = admin_level,
        survey_data = survey_data,
        wuenic_data = self$wuenic_estimates,
        un_estimates = self$un_estimates,
        sbr = rates$sbr,
        nmr = rates$nmr,
        pnmr = rates$pnmr,
        twin = rates$twin_rate,
        preg_loss = rates$preg_loss,
        anc1survey = rates$anc1,
        dpt1survey = rates$penta1,
        survey_year = self$survey_year,
        subnational_map = self$survey_mapping
      )
    },

    #' @description Run coverage calculation using stored model parameters.
    #' @param admin_level Administrative level ("adminlevel_1" or "district").
    get_mapping_data = function(admin_level) {
      check_required(admin_level)

      if (!self$check_inequality_params) {
        cd_abort(c('x' = 'One or more parameters is missing for {.fun calculate_inequality}'))
      }

      rates <- self$national_estimates

      get_mapping_data(
        .data = self$adjusted_data,
        admin_level = admin_level,
        un_estimates = self$un_estimates,
        sbr = rates$sbr,
        nmr = rates$nmr,
        pnmr = rates$pnmr,
        twin = rates$twin_rate,
        preg_loss = rates$preg_loss,
        anc1survey = rates$anc1,
        dpt1survey = rates$penta1,
        survey_year = self$survey_year,
        subnational_map = self$map_mapping
      )
    },

    #' @description Run coverage calculation using stored model parameters.
    calculate_health_system_comparison = function() {
      if (!self$check_inequality_params) {
        cd_abort(c('x' = 'One or more parameters is missing for {.fun calculate_indicator_coverage}'))
      }

      rates <- self$national_estimates

      calculate_health_system_comparison(
        .data = self$adjusted_data,
        sbr = rates$sbr,
        nmr = rates$nmr,
        pnmr = rates$pnmr,
        anc1survey = rates$anc1,
        dpt1survey = rates$penta1,
        survey_year = self$survey_year,
        twin = rates$twin_rate,
        preg_loss = rates$preg_loss
      )
    },

    #' @description Creates mortality summary
    create_mortality_summary = function() {
      if (!self$check_mortality_params) {
        cd_abort(c('x' = 'One or more parameters is missing for {.fun create_mortality_summary}'))
      }
      create_mortality_summary(self$adjusted_data)
    },

    #' @description creates mortality ratios from the mortality summary
    #' @param .data A `cd_mortality_summary` object
    create_mortality_ratios = function(.data) {
      if (!self$check_mortality_params) {
        cd_abort(c('x' = 'One or more parameters is missing for {.fun create_mortality_summary}'))
      }
      check_cd_class(.data, expected_class = 'cd_mortality_summary')
      .data %>%
        create_mortality_ratios(self$un_mortality_estimates)
    },

    #' @description generates the mean institutional livebirths
    lbr_mean = function() {
      indicator <- paste0('cov_instlivebirths_', self$maternal_denominator)
      self$calculate_indicator_coverage('national') %>%
        select(year, all_of(indicator)) %>%
        summarise(lbr_mean = mean(!!sym(indicator))) %>%
        pull(lbr_mean)
    },

    #' @description creates mortality ratios completeness summary
    #' @param .data A `cd_mortality_ratio` object
    #' @param indicator The indicator to generate the summary
    summarise_completeness_ratio = function(.data, indicator) {
      check_cd_class(.data, expected_class = 'cd_mortality_ratio')
      .data %>%
        summarise_completeness_ratio(indicator, self$lbr_mean())
    },

    #' @description Return the appropriate summary based on the indicator type to plot.
    #' @param .data A `cd_mortality_summary` object.
    #' @param indicator Character. Indicator name.
    #' @param map_years the years to include in a map
    #' @return Character. Either the maternal or vaccination denominator.
    filter_mortality_summary = function(.data, indicator, map_years = NULL) {
      check_cd_class(.data, expected_class = 'cd_mortality_summary')
      years <- if (is.null(map_years)) self$mapping_years else map_years
      .data %>%
        filter_mortality_summary(self$country_iso, indicator, years, self$map_mapping)
    },

    #' @description Computed service utilization for various indicators.
    #' @param admin_level The level to aggregate data at.
    compute_service_utilization = function(admin_level) {
      if (is.null(self$adjusted_data)) {
        cd_abort(c('x' = 'Adjusted data is required'))
      }
      check_required(admin_level)

      self$adjusted_data %>%
        compute_service_utilization(admin_level)
    },

    #' @description Return the appropriate summary based on the indicator type to plot.
    #' @param .data A `cd_service_utilization` object.
    #' @param indicator Character. Indicator name.
    #' @param map_years the years to include in a map
    filter_service_utilization = function(.data, indicator, map_years = NULL) {
      check_cd_class(.data, expected_class = 'cd_service_utilization')
      years <- if (is.null(map_years)) self$mapping_years else map_years
      .data %>%
        filter_service_utilization(self$country_iso, indicator, years, self$map_mapping)
    },

    #' @description Return the appropriate denominator based on the indicator type.
    #' @param indicator Character. Indicator name.
    #' @return Character. Either the maternal or vaccination denominator.
    get_denominator = function(indicator) {
      if (is_maternal_indicator(indicator)) {
        self$maternal_denominator
      } else {
        self$denominator
      }
    },

    #' @description Return a reactive wrapper (for Shiny).
    reactive = function() {
      # Ensure the reactive stuff is initialized.
      if (is.null(private$.reactiveDep)) {
        private$.reactiveDep <- reactiveValues() # Initialize as an empty reactiveValues
        for (field_name in names(private$.data_template)) {
          private$.reactiveDep[[field_name]] <- 0 # Create a reactive tracker for each field
        }
      }
      reactive({
        private$depend_all()
        self
      })
    },

    #' @description Set the language.
    #' @param value A scalar string representing 2 digit string.
    set_language = function(value) private$setter('language', value, ~ is_scalar_character(.x) && nchar(.x) == 2),

    #' @description Set cache RDS file path.
    #' @param value New file path.
    set_cache_path = function(value) {
      path_set <- private$setter('rds_path', value, ~ !is.null(.x) && length(.x) > 0)
      if (path_set) {
        private$.has_changed <- TRUE
        self$save_to_disk()
      }
    },

    #' @description Set the countdown data.
    #' @param value A `cd_data` object.
    set_countdown_data = function(value) private$setter('countdown_data', value, check_cd_data),

    #' @description Set adjusted data.
    #' @param value A `cd_data` object.
    set_adjusted_data = function(value) {
      check_cd_data(value)
      private$.adjusted_data <- value
    },

    #' @description Set performance threshold.
    #' @param value A numeric scalar.
    set_performance_threshold = function(value) private$setter('performance_threshold', value, is_scalar_integerish),

    #' @description Set years to exclude.
    #' @param value Numeric vector.
    set_excluded_years = function(value) private$setter('excluded_years', value, is.numeric),

    #' @description Set K-factors.
    #' @param value Named numeric vector.

    set_k_factors = function(value) private$setter('k_factors', value, ~ is.numeric(.x) && all(c('anc', 'idelv', 'vacc', 'opd', 'ipd') %in% names(.x))),

    #' @description Set adjusted flag.
    #' @param value Logical scalar.
    set_adjusted_flag = function(value) private$setter('adjusted_flag', value, ~ is.logical(.x) && length(.x) == 1),

    #' @description Set survey estimates.
    #' @param value Named numeric vector.
    set_survey_estimates = function(value) {
      print(value)
      if (!is.numeric(value) || !all(c('anc1', 'anc4', 'penta1', 'penta3', 'measles1', 'bcg', 'ideliv', 'lbw', 'csection') %in% names(value))) {
        cd_abort(c('x' = 'Survey must be a numeric vector containing {.val anc1}, {.val penta1} and {.val penta3}'))
      }
      private$update_field('survey_estimates', value)
    },

    #' @description Set national estimates.
    #' @param value Named list.
    set_national_estimates = function(value) private$setter('national_estimates', value, is.list),

    #' @description Set year of survey estimates.
    #' @param value Character scalar.
    set_survey_source = function(value) private$setter('survey_source', value, is_scalar_character),

    #' @description Set year of survey estimates.
    #' @param value Integer year.
    set_survey_year = function(value) private$setter('survey_year', value, is_scalar_integerish),

    #' @description Set start year of surveys.
    #' @param value Integer year.
    set_start_survey_year = function(value) private$setter('start_survey_year', value, is_scalar_integerish),

    #' @description Set denominator type.
    #' @param value Character scalar.
    set_denominator = function(value) private$setter('denominator', value, is_scalar_character),

    #' @description Set denominator type.
    #' @param value Character scalar.
    set_maternal_denominator = function(value) private$setter('maternal_denominator', value, is_scalar_character),

    #' @description Set selected region.
    #' @param value Character scalar.
    set_selected_admin_level_1 = function(value) private$setter('selected_admin_level_1', value, is_scalar_character),

    #' @description Set selected district.
    #' @param value Character scalar.
    set_selected_district = function(value) private$setter('selected_district', value, is_scalar_character),

    #' @description Set mapping years.
    #' @param value Integer vector.
    set_mapping_years = function(value) private$setter('selected_mapping_years', value, is_integerish),

    #' @description Set FPET data.
    #' @param value Data frame.
    set_fpet_data = function(value) private$setter('fpet_data', value, check_fpet_data),

    #' @description Set UN estimates.
    #' @param value Data frame.
    set_un_estimates = function(value) private$setter('un_estimates', value, check_un_estimates_data),

    #' @description Set UN mortality estimates
    #' @param value Data frame.
    set_un_mortality_estimates = function(value) private$setter('un_mortality_estimates', value, check_un_mortality_data),

    #' @description Set WUENIC estimates.
    #' @param value Data frame.
    set_wuenic_estimates = function(value) private$setter('wuenic_estimates', value, check_wuenic_data),

    #' @description Set national survey.
    #' @param value Data frame.
    set_national_survey = function(value) private$setter('national_survey', value, check_survey_data),

    #' @description Set regional survey.
    #' @param value Data frame.
    set_regional_survey = function(value) private$setter('regional_survey', value, ~ check_survey_data(.x, 'adminlevel_1')),

    #' @description Set WIQ survey.
    #' @param value Data frame.
    set_wiq_survey = function(value) private$setter('wiq_survey', value, check_equity_data),

    #' @description Set area-level survey.
    #' @param value Data frame.
    set_area_survey = function(value) private$setter('area_survey', value, check_equity_data),

    #' @description Set education-level survey.
    #' @param value Data frame.
    set_education_survey = function(value) private$setter('education_survey', value, check_equity_data),

    #' @description Set survey mapping table.
    #' @param value Data frame.
    set_survey_mapping = function(value) private$setter('survey_mapping', value, is.data.frame),

    #' @description Set map overlay mapping.
    #' @param value Data frame.
    set_map_mapping = function(value) private$setter('map_mapping', value, is.data.frame)
  ),
  active = list(
    #' @field language Get the UI language.
    language = function(value) private$getter('language', value),

    #' @field cache_path Get cache path.
    cache_path = function(value) private$getter('rds_path', value),

    #' @field countdown_data Get countdown data.
    countdown_data = function(value) private$getter('countdown_data', value),

    #' @field country Get country name.
    country = function(value) {
      if (missing(value)) {
        if (is.null(self$countdown_data)) {
          return(NULL)
        }

        return(attr_or_abort(self$countdown_data, 'country'))
      }

      cd_abort(c('x' = '{.field country} is readonly.'))
    },

    #' @field country_iso Get country ISO3 code.
    country_iso = function(value) {
      if (missing(value)) {
        if (is.null(self$countdown_data)) {
          return(NULL)
        }

        return(attr_or_abort(self$countdown_data, 'iso3'))
      }

      cd_abort(c('x' = '{.field iso3} is readonly.'))
    },

    #' @field default_national_estimates Get the national rates.
    default_national_estimates = function(value) {
      if (missing(value)) {
        if (is.null(self$countdown_data)) {
          return(NULL)
        }

        return(attr_or_abort(self$countdown_data, 'national_rates'))
      }

      cd_abort(c('x' = '{.field default_national_estimates} is readonly.'))
    },

    #' @field adjusted_data Gets adjusted data.
    adjusted_data = function(value) {
      if (missing(value)) {
        if (self$adjusted_flag && !is.null(private$.adjusted_data)) {
          return(private$.adjusted_data)
        }
        return(NULL)
      }
      check_cd_data(value)
      private$.adjusted_data <- value
    },

    #' @field data_with_excluded_years Get data with excluded years removed.
    data_with_excluded_years = function(value) {
      if (missing(value)) {
        private$depend('excluded_years')
        excluded_years <- self$excluded_years
        data <- self$countdown_data %>%
          filter(if (length(excluded_years) > 0) !year %in% excluded_years else TRUE)
        return(data)
      }

      cd_abort(c('x' = '{.field data_with_excluded_years} is readonly.'))
    },

    #' @field performance_threshold Gets performance threshold.
    performance_threshold = function(value) private$getter('performance_threshold', value),

    #' @field excluded_years Gets excluded years.
    excluded_years = function(value) private$getter('excluded_years', value),

    #' @field k_factors Gets k-factors.
    k_factors = function(value) private$getter('k_factors', value),

    #' @field adjusted_flag Gets adjusted flag.
    adjusted_flag = function(value) private$getter('adjusted_flag', value),

    #' @field survey_estimates Gets survey estimates.
    survey_estimates = function(value) private$getter('survey_estimates', value),

    #' @field national_estimates Gets national estimates.
    national_estimates = function(value) {
      if (missing(value)) {
        private$depend('national_estimates')
        survey <- self$survey_estimates
        return(c(
          private$.in_memory_data$national_estimates,
          list(
            anc1 = unname(survey['anc1']) / 100,
            penta1 = unname(survey['penta1']) / 100
          )
        ))
      }

      if (!is.list(value)) {
        cd_abort(c('x' = 'Analysis values must be a list.'))
      }
      private$update_field('national_estimates', value)
    },

    #' @field survey_years Get survey years.
    survey_years = function(value) {
      survey <- private$getter('national_survey', value)
      if (is.null(survey)) {
        return(NULL)
      }
      survey %>%
        distinct(year) %>%
        arrange(year) %>%
        pull(year)
    },

    #' @field survey_source Gets survey source of information.
    survey_source = function(value) private$getter('survey_source', value),

    #' @field survey_year Gets survey year of survey estimates.
    survey_year = function(value) private$getter('survey_year', value),

    #' @field start_survey_year Gets start survey year.
    start_survey_year = function(value) private$getter('start_survey_year', value),

    #' @field denominator Gets denominator.
    denominator = function(value) private$getter('denominator', value),

    #' @field maternal_denominator Gets denominator.
    maternal_denominator = function(value) private$getter('maternal_denominator', value),

    #' @field selected_admin_level_1 Gets selected region.
    selected_admin_level_1 = function(value) private$getter('selected_admin_level_1', value),

    #' @field selected_district Gets selected district.
    selected_district = function(value) private$getter('selected_district', value),

    #' @field mapping_years Gets mapping years.
    mapping_years = function(value) private$getter('selected_mapping_years', value),

    #' @field fpet_data Gets UN estimates.
    fpet_data = function(value) private$getter('fpet_data', value),

    #' @field un_estimates Gets UN estimates.
    un_estimates = function(value) private$getter('un_estimates', value),

    #' @field un_mortality_estimates Gets UN mortality estimates.
    un_mortality_estimates = function(value) private$getter('un_mortality_estimates', value),

    #' @field wuenic_estimates Gets WUENIC estimates.
    wuenic_estimates = function(value) private$getter('wuenic_estimates', value),

    #' @field national_survey Gets national survey.
    national_survey = function(value) {
      survey <- private$getter('national_survey', value)
      if (is.null(survey)) {
        return(NULL)
      }

      private$filter_survey(survey)
    },

    #' @field regional_survey Gets regional survey.
    regional_survey = function(value) {
      survey <- private$getter('regional_survey', value)
      if (is.null(survey)) {
        return(NULL)
      }

      private$filter_survey(survey)
    },

    #' @field wiq_survey Gets WIQ survey.
    wiq_survey = function(value) {
      survey <- private$getter('wiq_survey', value)
      if (is.null(survey)) {
        return(NULL)
      }

      private$filter_survey(survey)
    },

    #' @field area_survey Gets area survey.
    area_survey = function(value) {
      survey <- private$getter('area_survey', value)
      if (is.null(survey)) {
        return(NULL)
      }

      private$filter_survey(survey)
    },

    #' @field education_survey Gets  education survey.
    education_survey = function(value) {
      survey <- private$getter('education_survey', value)
      if (is.null(survey)) {
        return(NULL)
      }

      private$filter_survey(survey)
    },

    #' @field survey_mapping Gets survey mapping.
    survey_mapping = function(value) private$getter('survey_mapping', value),

    #' @field map_mapping Gets map mapping.
    map_mapping = function(value) private$getter('map_mapping', value),

    #' @field check_inequality_params checks if inputs for inequality calculations are available
    check_inequality_params = function() {
      !is.null(self$adjusted_data) &&
        !is.null(self$survey_year) &&
        !is.null(self$un_estimates) &&
        all(!is.na(self$national_estimates))
    },

    #' @field check_coverage_params checks if inputs for coverage calculations are available
    check_coverage_params = function() {
      self$check_inequality_params &&
        !is.null(self$wuenic_estimates) &&
        !is.null(self$national_survey) &&
        !is.null(self$regional_survey)
    },

    #' @field check_mortality_params checks if inputs for mortality calculations are available
    check_mortality_params = function() {
      !is.null(self$adjusted_data) &&
        !is.null(self$un_mortality_estimates)
    }
  ),
  private = list(
    .data_template = list(
      language = 'en',
      rds_path = NULL,
      countdown_data = NULL,
      performance_threshold = 90,
      excluded_years = numeric(),
      k_factors = c(anc = 0, idelv = 0, vacc = 0, opd = 0, ipd = 0),
      adjusted_flag = FALSE,
      survey_year = NULL,
      survey_estimates = c(anc1 = NA, penta1 = NA, penta3 = NA, measles1 = NA, bcg = NA, anc4 = NA, ideliv = NA, lbw = NA, csection = NA),
      national_estimates = list(nmr = NA, pnmr = NA, twin_rate = 0.015, preg_loss = 0.03, sbr = NA),
      start_survey_year = NULL,
      survey_source = NULL,
      denominator = 'penta1',
      maternal_denominator = 'anc1',
      selected_admin_level_1 = NULL,
      selected_district = NULL,
      selected_mapping_years = NULL,
      # palette = c(coverage = 'Greens', dropout = 'Reds'),
      un_estimates = NULL,
      un_mortality_estimates = NULL,
      wuenic_estimates = NULL,
      national_survey = NULL,
      regional_survey = NULL,
      wiq_survey = NULL,
      area_survey = NULL,
      education_survey = NULL,
      survey_mapping = NULL,
      map_mapping = NULL,
      fpet_data = NULL,
      page_notes = tibble::tibble(
        page_id = character(),
        object_id = character(),
        note = character(),
        parameters = list(),
        include_in_report = logical(),
        include_plot_table = logical()
      )
    ),
    .in_memory_data = NULL,
    .adjusted_data = NULL,
    .has_changed = FALSE,
    .reactiveDep = NULL,
    #' Update a field (with change tracking)
    update_field = function(field_name, value) {
      if (!identical(private$.in_memory_data[[field_name]], value)) {
        private$.in_memory_data[[field_name]] <<- value
        private$.has_changed <<- TRUE
        private$trigger(field_name)
      }
    },
    getter = function(field_name, value) {
      if (is_missing(value)) {
        private$depend(field_name)
        return(private$.in_memory_data[[field_name]])
      }

      cd_abort(c('x' = '{.field field_name} is readonly'))
    },
    filter_survey = function(survey) {
      check_required(survey)
      start_year <- self$start_survey_year
      survey %>%
        filter(if (is.null(start_year)) TRUE else year >= start_year)
    },
    setter = function(field_name, value, validation_exp = NULL) {
      check_required(field_name)
      check_required(value)

      validate_fn <- if (!is.null(validation_exp)) {
        if (rlang::is_formula(validation_exp)) {
          rlang::as_function(validation_exp)
        } else if (rlang::is_function(validation_exp)) {
          validation_exp
        } else {
          cd_abort(c('x' = '{.arg validation} must be a function or a formula.'))
        }
      } else {
        function(x) TRUE
      }
      if (!validate_fn(value)) {
        cd_abort(c('x' = 'Invalid value for field {.field field_name}.'))
      }
      private$update_field(field_name, value)
    },
    depend = function(field_name) {
      if (!is.null(private$.reactiveDep[[field_name]])) {
        private$.reactiveDep[[field_name]]
      }
      invisible()
    },
    trigger = function(field_name) {
      if (!is.null(private$.reactiveDep[[field_name]])) {
        private$.reactiveDep[[field_name]] <- isolate(private$.reactiveDep[[field_name]] + 1)
      }
    },
    depend_all = function() {
      for (field_name in names(private$.data_template)) {
        private$depend(field_name) # Establish dependency for each field
      }
      invisible()
    }
  )
)
