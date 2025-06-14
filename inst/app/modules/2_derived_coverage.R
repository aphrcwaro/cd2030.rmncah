derivedCoverageUI <- function(id, i18n) {
  ns <- NS(id)
  tagList(
    contentHeader(ns('derived_coverage'), i18n$t("title_derived_coverage"), i18n = i18n),
    contentBody(
      box(
        title = i18n$t("title_analysis_options"),
        status = 'primary',
        width = 12,
        solidHeader = TRUE,
        fluidRow(
          column(3, adminLevelInputUI(ns('admin_level'), i18n, include_national = TRUE)),
          column(3, uiOutput(ns('region_ui')))
        )
      ),
      tabBox(
        title = i18n$t("title_derived_coverage"),
        width = 12,
        tabPanel(
          title = i18n$t("opt_penta1"),
          fluidRow(
            column(12, plotCustomOutput(ns('penta1'))),
            column(12, tagList(
              column(4, downloadButtonUI(ns('penta1_plot'))),
              column(4, downloadButtonUI(ns('penta1_data')))
            ))
          )
        ),

        tabPanel(
          title = i18n$t("opt_penta3"),
          fluidRow(
            column(12, plotCustomOutput(ns('penta3'))),
            column(12, tagList(
              column(4, downloadButtonUI(ns('penta3_plot'))),
              column(4, downloadButtonUI(ns('penta3_data')))
            ))
          )
        ),

        tabPanel(
          title = i18n$t("opt_measles"),
          fluidRow(
            column(12, plotCustomOutput(ns('measles1'))),
            column(12, tagList(
              column(4, downloadButtonUI(ns('measles1_plot'))),
              column(4, downloadButtonUI(ns('measles1_data')))
            ))
          )
        ),

        tabPanel(
          i18n$t("opt_custom_check"),
          fluidRow(
            column(3, selectizeInput(ns('indicator'), label = i18n$t("title_indicator"),
                                     choices = c('Select' = '', get_indicator_without_opd_ipd())))
          ),
          fluidRow(
            column(12, plotCustomOutput(ns('custom'))),
            column(12, tagList(
              column(4, downloadButtonUI(ns('custom_plot'))),
              column(4, downloadButtonUI(ns('custom_data')))
            ))
          )
        )
      )
    )
  )
}

derivedCoverageServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      admin_level <- adminLevelInputServer('admin_level')
      region <- regionInputServer('region', cache, admin_level, i18n)

      survey_year <- reactive({
        req(cache())
        cache()$survey_year
      })

      populations <- reactive({
        req(cache(), cache()$check_inequality_params, admin_level())
        cache()$calculate_indicator_coverage(admin_level())
      })

      penta1_data <- reactive({
        req(populations(), survey_year())
        calculate_derived_coverage(populations(), 'penta1', survey_year())
      })

      penta3_data <- reactive({
        req(populations(), survey_year())
        calculate_derived_coverage(populations(), 'penta3', survey_year())
      })

      measles1_data <- reactive({
        req(populations(), survey_year())
        calculate_derived_coverage(populations(), 'measles1', survey_year())
      })

      custom_data <- reactive({
        req(populations(), survey_year(), input$indicator)
        calculate_derived_coverage(populations(), input$indicator, survey_year())
      })

      output$region_ui <- renderUI({
        req(admin_level())
        if (admin_level() != 'national') {
          regionInputUI(ns('region'), i18n)
        }
      })

      output$penta1 <- renderCustomPlot({
        req(penta1_data())
        if (admin_level() != 'national') {
          req(region())
        }
        plot(penta1_data(), region = region())
      })

      output$penta3 <- renderCustomPlot({
        req(penta3_data())
        if (admin_level() != 'national') req(region())
        plot(penta3_data(), region = region())
      })

      output$measles1 <- renderCustomPlot({
        req(measles1_data())
        if (admin_level() != 'national') req(region())
        plot(measles1_data(), region = region())
      })

      output$custom <- renderCustomPlot({
        req(custom_data())
        if (admin_level() != 'national') req(region())
        plot(custom_data(), region = region())
      })

      downloadPlot(
        id = 'penta1_plot',
        filename = reactive('penta1_derived_coverage'),
        data = penta1_data,
        i18n = i18n,
        plot_function = function() plot(penta1_data(), region = region())
      )

      downloadExcel(
        id = 'penta1_data',
        filename = reactive('penta1_derived_coverage'),
        data = penta3_data,
        i18n = i18n,
        excel_write_function = function(wb) {
          sheet_name_1 <- i18n$t('sheet_penta1_derived_coverage')
          addWorksheet(wb, sheet_name_1)
          writeData(wb, sheet = sheet_name_1, penta1_data(), startCol = 1, startRow = 1)
        }
      )

      downloadPlot(
        id = 'penta3_plot',
        filename = reactive('penta3_derived_coverage'),
        data = penta3_data,
        i18n = i18n,
        plot_function = function() plot(penta3_data(), region = region())
      )

      downloadExcel(
        id = 'penta3_data',
        filename = reactive('penta3_derived_coverage'),
        data = penta3_data,
        i18n = i18n,
        excel_write_function = function(wb) {
          sheet_name_1 <- i18n$t('sheet_penta3_derived_coverage')
          addWorksheet(wb, sheet_name_1)
          writeData(wb, sheet = sheet_name_1, penta3_data(), startCol = 1, startRow = 1)
        }
      )

      downloadPlot(
        id = 'measles1_plot',
        filename = reactive('measles1_derived_coverage'),
        data = measles1_data,
        i18n = i18n,
        plot_function = function() plot(measles1_data(), region = region())
      )

      downloadExcel(
        id = 'measles1_data',
        filename = reactive('measles1_derived_coverage'),
        data = measles1_data,
        i18n = i18n,
        excel_write_function = function(wb) {
          sheet_name_1 <- i18n$t('sheet_measles1_derived_coverage')
          addWorksheet(wb, sheet_name_1)
          writeData(wb, sheet = sheet_name_1, measles1_data(), startCol = 1, startRow = 1)
        }
      )

      downloadPlot(
        id = 'custom_plot',
        filename = reactive(paste0(input$indicator, '_derived_coverage')),
        data = custom_data,
        i18n = i18n,
        plot_function = function() plot(custom_data(), region = region())
      )

      downloadExcel(
        id = 'custom_data',
        filename = reactive(paste0(input$indicator, '_derived_coverage')),
        data = custom_data,
        i18n = i18n,
        excel_write_function = function(wb) {
          sheet_name_1 <- str_glue_data(list(indicator = input$indicator), i18n$t('sheet_custom_derived_coverage'))
          addWorksheet(wb, sheet_name_1)
          writeData(wb, sheet = sheet_name_1, custom_data(), startCol = 1, startRow = 1)
        }
      )

      contentHeaderServer(
        'derived_coverage',
        cache = cache,
        path = 'denominator-assessment',
        section = 'sec-derived-coverage',
        i18n = i18n
      )
    }
  )
}
