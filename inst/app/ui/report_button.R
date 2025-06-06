reportButtonUI <- function(id, label) {
  ns <- NS(id)
  actionButton(
    inputId = ns('generate_report'),
    label = label,
    icon = icon('file-alt'),
    class ='btn bg-olive btn-flat btn-sm'
  )
}

reportButtonServer <- function(id, cache, report_name, i18n, adminlevel_1) {
  stopifnot(is.reactive(cache))
  stopifnot(is.reactive(report_name))
  stopifnot(is.reactive(adminlevel_1))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns
      rv <- reactiveValues(generating = FALSE, future = NULL, file_path = NULL)

      country <- reactive({
        req(cache())
        cache()$country
      })

      report_file_name <- reactive({
        req(country(), report_name())
        paste0(country(), '_', report_name(), '_countdown_report')
      })

      extension <- reactive({
        req(input$format)

        switch(input$format,
               'word_document' = 'docx',
               'pdf_document' = 'pdf',
               'html_document' = 'html')
      })

      observeEvent(input$generate_report, {
        req(cache())

        showModal(
          modalDialog(
            title = i18n$t("title_download_options"),
            selectizeInput(
              ns('format'), i18n$t("title_select_format"),
              choices = c('Word' = 'word_document', 'PDF' = 'pdf_document')
            ),
            footer = tagList(
              modalButton(i18n$t("btn_cancel")),
              actionButton(ns('start_generate'), i18n$t("btn_generate_report"), class = 'btn bg-olive btn-flat')
            )
          )
        )
      })

      observeEvent(input$start_generate, {
        req(input$format)

        params <- list(
          format = input$format
        )

        removeModal()
        showModal(
          modalDialog(
            title = tagList(
              div(
                class = 'text-center',
                h4(i18n$t("msg_generating_report"))
              )
            ),
            div(
              class = 'text-center',
              icon('file-alt', class = 'fa-5x text-primary mb-3'), # Add a large icon
              p(i18n$t("msg_report_generating"), class = 'lead'),
              div(class = 'spinner-border text-primary', role = 'status', span(class = 'sr-only', i18n$t("msg_loading")))
            ),
            footer = NULL,
            easyClose = FALSE
          )
        )

        rv$generating <- TRUE
        rv$future <- future({
          temp_file <- tempfile(fileext = paste0('.', extension()))
          generate_report(
            cache = cache(),
            output_file = temp_file,
            report_name = report_name(),
            adminlevel_1 = adminlevel_1(),
            output_format = params$format
          )
          temp_file
        }, globals = list(cache = cache, params = params, generate_report = generate_report,
                          extension = extension, adminlevel_1 = adminlevel_1, report_name = report_name))

        rv$future %...>% {
          rv$generating <- FALSE
          rv$file_path <- .
          removeModal()

          # Update modal to show download button
          showModal(
            modalDialog(
              title = i18n$t("msg_download_ready"),
              div(
                class = 'text-center',
                icon('check-circle', class = 'fa-5x text-success mb-3'),
                h4(paste0(i18n$t("msg_success"), '!')),
                p(i18n$t("msg_report_generated")),
                p(i18n$t("msg_report_generated_dialog"))
              ),
              footer = tagList(
                fluidRow(
                  column(6, downloadButtonUI(ns('download_data'))),
                  column(6, modalButton(i18n$t("btn_dismiss")))
                )
              ),
              easyClose = FALSE # Prevent accidental dismissal by clicking outside the modal
            )
          )
        } %...!% {
          print(.)
          # Handle errors
          rv$generating <- FALSE
          removeModal()
          showModal(
            modalDialog(
              title = i18n$t("msg_error"),
              i18n$t("error_report_generation"),
              easyClose = TRUE,
              footer = modalButton(i18n$t("btn_ok"))
            )
          )
        }
      })

      downloadButtonServer(
        id = 'download_data',
        filename = report_file_name,
        extension = extension,
        i18n = i18n,
        content = function(file, path) {
          req(path)
          file.copy(path, file)
        },
        data = reactive(rv$file_path),
        label = "btn_download_report",
        message = "msg_generating_report"
      )
    }
  )
}
