dataAjustmentUI <- function(id, i18n) {
  ns <- NS(id)

  k_factor_options <- c(0, 0.25, 0.5, 0.75)
  tagList(
    contentHeader(ns('data_adjustment'), i18n$t("title_adjustment"), i18n = i18n),
    contentBody(
      fluidRow(
        column(
          8,
          offset = 2,
          box(
            title = i18n$t("title_factors"),
            status = 'primary',
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              column(2, selectizeInput(ns('k_anc'),
                                       label = i18n$t("title_anc_factor"),
                                       choices = k_factor_options)),
              column(2, selectizeInput(ns('k_delivery'),
                                       label = i18n$t("title_delivery_factor"),
                                       choices = k_factor_options)),
              column(2, selectizeInput(ns('k_vaccines'),
                                       label = i18n$t("title_vaccines_factor"),
                                       choices = k_factor_options)),
              column(2, selectizeInput(ns('k_opd'),
                                       label = i18n$t("title_opd_factor"),
                                       choices = k_factor_options)),
              column(2, selectizeInput(ns('k_ipd'),
                                       label = i18n$t("title_ipd_factor"),
                                       choices = k_factor_options))
            )
          )
        ),
        column(
          8,
          offset = 2,
          box(
            title = 'Adjust',
            status = 'danger',
            width = 12,
            solidHeader = TRUE,

            fluidRow(
              column(12, tags$p(
                i18n$t("msg_adjustment_info"),
                style = 'color: red; font-weight: bold; margin-bottom: 15px;'
              ))
            ),

            fluidRow(
              column(8, offset = 2, actionButton(ns('adjust_data'),
                                                 label = i18n$t("btn_adjust_data"),
                                                 icon = icon('wrench'),
                                                 style = 'background-color: #FFEB3B;font-weight: 500;width:100%; margin-top: 15px;')),
              column(8, offset = 2, messageBoxUI(ns('feedback'))),
              column(8, offset = 2, downloadButtonUI(ns('download_data')))
            )
          )
        )
      )
    )
  )
}

dataAdjustmentServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      state <- reactiveValues(loaded = FALSE)
      messageBox <- messageBoxServer('feedback', i18n = i18n, default_message = 'msg_dataset_not_adjusted')

      data <- reactive({
        req(cache())
        cache()$data_with_excluded_years
      })

      modified_data <- reactive({
        req(cache())
        cache()$adjusted_data
      })

      adjusted_flag <- reactive({
        req(cache())
        cache()$adjusted_flag
      })

      k_factors <- reactive({
        req(cache())
        cache()$k_factors
      })

      observeEvent(data(), {
        req(data())
        state$loaded <- FALSE
      })

      observeEvent(c(input$k_anc, input$k_delivery, input$k_vaccines, input$k_opd, input$k_ipd), {
        req(cache())

        k <- k_factors()
        k['anc'] <- as.numeric(input$k_anc)
        k['idelv'] <- as.numeric(input$k_delivery)
        k['vacc'] <- as.numeric(input$k_vaccines)
        k['opd'] <- as.numeric(input$k_opd)
        k['ipd'] <- as.numeric(input$k_ipd)

        cache()$set_k_factors(k)
      })

      observe({
        req(cache(), !state$loaded)

        k <- k_factors()
        updateSelectInput(session, 'k_anc', selected = as.character(unname(k['anc'])))
        updateSelectInput(session, 'k_delivery', selected = as.character(unname(k['idelv'])))
        updateSelectInput(session, 'k_vaccines', selected = as.character(unname(k['vacc'])))
        updateSelectInput(session, 'k_opd', selected = as.character(unname(k['opd'])))
        updateSelectInput(session, 'k_ipd', selected = as.character(unname(k['ipd'])))

        state$loaded <- TRUE
      })

      observeEvent(input$adjust_data, {
        req(cache())
        messageBox$update_message('msg_adjusting', 'info')
        cache()$set_adjusted_flag(FALSE)
        cache()$set_adjusted_flag(TRUE)
      })

      observeEvent(adjusted_flag(), {
        req(data())
        if (adjusted_flag()) {
          dt <- data() %>%
            adjust_service_data(adjustment = 'custom', k_factors = k_factors())

          cache()$set_adjusted_data(dt)

          messageBox$update_message('msg_data_adjusted', 'success')
        } else {
          messageBox$update_message('msg_dataset_not_adjusted', 'info')
        }
      })

      downloadButtonServer(
        id = 'download_data',
        filename = reactive('master_adj_dataset'),
        extension = reactive('dta'),
        i18n = i18n,
        content = function(file) {
          haven::write_dta(modified_data(), file)
        },
        data = adjusted_flag,
        label = "btn_download_adjusted_dataset"
      )

      contentHeaderServer(
        'data_adjustment',
        cache = cache,
        path = 'numerator-adjustments',
        section = 'sec-dqa-adjust-outputs',
        i18n = i18n
      )
    }
  )
}
