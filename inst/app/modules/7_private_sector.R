privateSectorUI <- function(id, i18n) {
  ns <- NS(id)

  contentDashboard(
    dashboardId = ns('private_sector'),
    dashboardTitle = i18n$t('title_private_sector'),
    i18n = i18n,

    include_report = TRUE,

    optionsHeader = contentOptions(
      title = i18n$t('title_analysis_options'),
      column(12,
        directoryInput(
          ns('directory_select'),
          label = i18n$t('title_upload_private'),
          buttonLabel = i18n$t('btn_browse_or_drop'),
          accept = '.dta'
        )
      )
    ),

    tabBox(
      title = i18n$t('title_private_sector'),
      width = 12,

      tabPanel(title = i18n$t("opt_private_national"), downloadCoverageUI(ns('national'))),
      tabPanel(title = i18n$t("opt_private_area"), downloadCoverageUI(ns('area')))
    )
  )
}

privateSectorServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      selected_dir_box <- messageBoxServer('selected_dir_feedback', 'msg_survey_not_set', i18n = i18n)
      state <- reactiveValues(loaded = FALSE)

      data <- reactive({
        req(cache())
        cache()$countdown_data
      })

      country_iso <- reactive({
        req(cache())
        cache()$country_iso
      })

      national_sector <- reactive({
        req(cache(), cache()$sector_national_estimates, cache()$csection_national_estimates)
        prepare_private_sector_plot_data(cache()$sector_national_estimates, cache()$csection_national_estimates)
      })

      area_sector <- reactive({
        req(cache(), cache()$sector_area_estimates, cache()$csection_area_estimates)
        prepare_private_sector_plot_data(cache()$sector_area_estimates, cache()$csection_area_estimates)
      })

      observeEvent(input$directory_select, {
        req(cache(), input$directory_select)

        required_files <- c(
          'Area estimates.dta',
          'csection_area.dta',
          'National estimates.dta',
          'csection_national.dta'
        )

        uploaded_files <- input$directory_select %>%
          filter(name %in% required_files)

        missing_files <- setdiff(required_files, uploaded_files$name)

        selected_dir_box$update_message('msg_files_uploaded', 'success')

        # Log missing files and exit if any are not found
        if (length(missing_files) > 0) {
          files <- paste(missing_files, collapse = ', ')
          selected_dir_box$add_message('msg_missing_required_files', 'error', list(files = files))
          return()  # Exit early if required files are missing
        }

        uploaded_files %>%
          split(seq_len(nrow(.))) %>%
          walk(~ {

            tryCatch({
              file_path <- .x$datapath

              tryCatch(
                cd2030.rmncah:::check_file_path(file_path),
                error = function(e) {
                  selected_dir_box$add_message(clean_error_message(e), 'error')
                  return()
                }
              )

              if (grepl('^Area estimates', .x$name)) {
                area_estimates <- load_private_sector_data(path = file_path, country_iso = country_iso(), level = 'area')
                cache()$set_sector_area_estimates(area_estimates)
                new_log <- 'log_loaded_national_survey'
              } else if (grepl('^National estimates', .x$name)) {
                nat_estimates <- load_private_sector_data(path = file_path, country_iso = country_iso(), level = 'national')
                cache()$set_sector_national_estimates(nat_estimates)
                new_log <- 'log_loaded_regional_survey'
              } else if (grepl('^csection_area', .x$name)) {
                area_cs <- load_csection_estimates(path = file_path, country_iso = country_iso(), level = 'area')
                cache()$set_csection_area_estimates(area_cs)
                new_log <- 'log_loaded_area_survey'
              } else if (grepl('^csection_national', .x$name)) {
                nat_cs <- load_csection_estimates(path = file_path, country_iso = country_iso(), level = 'national')
                cache()$set_csection_national_estimates(nat_cs)
                new_log <- 'log_loaded_maternal_education_survey'
              } else {
                new_log <- 'log_unknown_file_pattern'
              }

              selected_dir_box$add_message(new_log, 'success', list(filename = .x$name))
            },
            error = function(e) {
              clean_message <- clean_error_message(e)
              selected_dir_box$add_message('log_file_processing_error', 'error', list(filename = .x$name, error = clean_message))
            })
          })
      })

      observeEvent(data(), {
        req(data())
        state$loaded <- FALSE
        if (is.null(cache()$national_survey)) {
          shinyjs::reset('directory_select')
        }
      })

      observe({
        req(data(), !state$loaded)

        state$loaded <- TRUE

        if (all(!is.null(cache()$sector_national_estimates), !is.null(cache()$sector_area_estimates),
                !is.null(cache()$csection_national_estimates), !is.null(cache()$csection_ares_estimates))) {
          selected_dir_box$update_message('msg_cache_loading', 'success')
        } else {
          selected_dir_box$add_message('msg_awaiting_upload', 'info')
        }

        if (!is.null(cache()$sector_national_estimates)) {
          selected_dir_box$add_message('msg_cache_loaded_national', 'success')
        }
        if (!is.null(cache()$sector_area_estimates)) {
          selected_dir_box$add_message('msg_cache_loaded_regional', 'success')
        }
        if (!is.null(cache()$csection_national_estimates)) {
          selected_dir_box$add_message('msg_cache_loaded_wealth_index', 'success')
        }
        if (!is.null(cache()$csection_ares_estimates)) {
          selected_dir_box$add_message('msg_cache_loaded_area', 'success')
        }
      })

      downloadCoverageServer(
        id = 'national',
        filename = reactive(paste0('national_sector')),
        data_fn = national_sector,
        i18n = i18n,
        sheet_name = reactive(i18n$t('opt_sbr_inst'))
      )

      downloadCoverageServer(
        id = 'area',
        filename = reactive(paste0('area_sector')),
        data_fn = area_sector,
        i18n = i18n,
        sheet_name = reactive(i18n$t('opt_sbr_inst'))
      )

      contentHeaderServer(
        'private_sector',
        cache = cache,
        path = 'private-sector-analyses',
        i18n = i18n
      )
    }
  )
}
