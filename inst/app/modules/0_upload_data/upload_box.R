uploadBoxUI <- function(id, i18n) {
  ns <- NS(id)

  box(
    title = i18n$t('title_upload_data'),
    status = 'primary',
    solidHeader = TRUE,
    width = 12,
    fluidRow(
      column(3, h4(icon('upload'), i18n$t('btn_upload'))),
      column(2, offset = 7, helpButtonUI(ns('upload_data'), name = i18n$t('btn_help')), align = 'right')
    ),
    fluidRow(
      column(
        12,
        fileInput(
          inputId = ns('hfd_file'),
          label = i18n$t('btn_upload_hfd_data'),
          buttonLabel = i18n$t('btn_browse_or_drop'),
          placeholder = 'Supported formats: .xls, .xlsx, .dta, .rds',
          accept = c('.xls', '.xlsx', '.dta', '.rds')
        ),

        messageBoxUI(ns('feedback'))
      )
    ),
    fluidRow(
      column(4, downloadButtonUI(ns('download_data')))
    )
  )
}

uploadBoxServer <- function(id, i18n) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      messageBox <- messageBoxServer('feedback', i18n = i18n)

      cache <- eventReactive(input$hfd_file, {
        req(input$hfd_file)

        file_path <- input$hfd_file$datapath
        file_name <- input$hfd_file$name
        file_type <- tools::file_ext(file_name)

        valid_types <- c('xls', 'xlsx', 'dta', 'rds')
        if (!file_type %in% valid_types) {
          messageBox$update_message('error_upload_failed_unsupported_format', 'error')
          return(NULL)
        }

        tryCatch({
          dt <- if (file_type %in% c('xls', 'xlsx')) {
            load_excel_data(file_path)
          } else if (file_type == 'dta') {
            load_data(file_path)
          } else {
            NULL
          }

          rds_path <- if (file_type == 'rds') {
            file_path
          } else {
            NULL
          }

          messageBox$update_message('msg_upload_success', 'success', list(file_name = file_name))

          cache_instance <- init_CacheConnection(
            countdown_data = dt,
            rds_path = rds_path
          )$reactive()

          cache_instance()
        },
        error = function(e) {
          clean_message <- clean_error_message(e)
          messageBox$update_message('error_upload_failed', 'error', list(clean_message = clean_message))
          NULL
        })
      })

      downloadButtonServer(
        id = 'download_data',
        filename = reactive('master_dataset'),
        extension = reactive('dta'),
        i18n = i18n,
        content = function(file) {
          haven::write_dta(cache()$countdown_data, file)
        },
        data = cache,
        label = 'btn_download_master_dataset'
      )

      helpButtonServer(
        id = 'upload_data',
        path = 'loading-data',
        cache = cache
      )

      return(cache)
    }
  )
}
