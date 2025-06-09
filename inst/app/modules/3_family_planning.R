familyPlanningUI <- function(id, i18n) {
  ns <- NS(id)

  contentDashboard(
    dashboardId = ns('national_coverage'),
    dashboardTitle = i18n$t("title_fpet"),
    i18n = i18n,

    include_report = TRUE,

    optionsHeader = contentOptions(
      title = i18n$t('title_analysis_options'),
      column(
        12,
        fileInput(
          inputId = ns('fpet_file'),
          label = i18n$t('btn_upload_fpet_data'),
          buttonLabel = i18n$t('btn_browse_or_drop'),
          placeholder = 'Supported formats: .csv',
          accept = c('.csv')
        ),

        messageBoxUI(ns('feedback'))
      )
    ),

    box(
      title = i18n$t("title_fpet"),
      width = 12,
      downloadCoverageUI(ns('fpet'))
    )
  )
}

familyPlanningServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      messageBox <- messageBoxServer('feedback', i18n = i18n)
      state <- reactiveValues(loaded = FALSE)

      observeEvent(data(), {
        req(data())
        state$loaded <- FALSE
        if (is.null(cache()$fpet_data)) {
          shinyjs::reset('fpet_file')
        }
      })

      observeEvent(input$fpet_file, {
        req(cache(), input$fpet_file)

        file_path <- input$fpet_file$datapath
        file_name <- input$fpet_file$name
        file_type <- tools::file_ext(file_name)

        valid_types <- c('csv')
        if (!file_type %in% valid_types) {
          messageBox$update_message('error_upload_failed_unsupported_format', 'error')
          return(NULL)
        }

        tryCatch({
          dt <- load_fpet_data(file_path)
          cache()$set_fpet_data(dt)
          messageBox$update_message('msg_upload_success', 'success', list(file_name = file_name))
        },
        error = function(e) {
          clean_message <- clean_error_message(e)
          messageBox$update_message('error_upload_failed', 'error', list(clean_message = clean_message))
        })
      })

      data <- reactive({
        req(cache(), cache()$fpet_data)
        cache()$fpet_data %>%
          generate_fpet_summary()
      })

      observe({
        req(data(), !state$loaded)
        state$loaded <- TRUE
        if (is.null(input$un_data$name) && !is.null(cache()$fpet_data)) {
          messageBox$update_message('msg_cache_loaded', 'success')
        } else {
          messageBox$update_message('msg_awaiting_upload', 'info')
        }
      })

      downloadCoverageServer(
        id = 'fpet',
        filename = reactive('fpet'),
        data_fn = data,
        sheet_name = reactive(i18n$t("title_fpet")),
        i18n = i18n
      )

      contentHeaderServer(
        'national_coverage',
        cache = cache,
        objects = pageObjectsConfig(input),
        md_title = i18n$t("title_coverage"),
        md_file = '2_calculate_ratios.md',
        i18n = i18n
      )
    }
  )
}
