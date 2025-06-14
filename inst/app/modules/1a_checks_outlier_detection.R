outlierDetectionUI <- function(id, i18n) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('outlier_detection'), i18n$t('title_outlier'), i18n = i18n),
    contentBody(
      box(
        title = 'Outlier Options',
        status = 'primary',
        solidHeader = TRUE,
        width = 12,
        fluidRow(
          column(3, selectizeInput(ns('indicator'),
                                   label = i18n$t('title_indicator'),
                                   choice = c('Select Indicator' = '', get_all_indicators()))),
          column(3, adminLevelInputUI(ns('admin_level'), i18n)),
          column(3, regionInputUI(ns('region'), i18n))
        )
      ),
      tabBox(
        title = tags$span(icon('chart-line'), i18n$t('title_indicators_with_outlier')),
        width = 12,

        tabPanel(
          title = i18n$t('title_heat_map'),
          fluidRow(
            column(12, withSpinner(plotlyOutput(ns('district_outlier_heatmap')))),
            column(3, downloadButtonUI(ns('district_outlier_heatmap_plot'))),
            column(3, downloadButtonUI(ns('download_data')))
          )
        ),

        tabPanel(
          title = i18n$t('title_indicator_bar_graph'),
          fluidRow(
            column(12, plotCustomOutput(ns('indicator_bar_graph'))),
            column(3, downloadButtonUI(ns('indicator_bar_graph_plot')))
          )
        ),

        tabPanel(
          title = i18n$t('title_region_bar_graph'),
          fluidRow(
            column(12, plotCustomOutput(ns('region_bar_graph'))),
            column(3, downloadButtonUI(ns('region_bar_graph_plot')))
          )
        )
      ),
      box(
        title = i18n$t('title_district_outliers'),
        status = 'primary',
        collapsible = TRUE,
        width = 6,
        fluidRow(
          column(3, selectizeInput(ns('year'), label = i18n$t('title_year'), choice = NULL)),
          column(3, offset = 6, downloadButtonUI(ns('download_outliers'))),
          column(12, withSpinner(reactableOutput(ns('district_outlier_summary'))))
        )
      ),
      box(
        title = i18n$t('title_district_trends'),
        status = 'primary',
        collapsible = TRUE,
        width = 6,
        fluidRow(
          column(6, regionInputUI(ns('region_trend'), i18n)),
          column(12, withSpinner(plotCustomOutput(ns('district_trend'))))
        )
      )
    )
  )
}

outlierDetectionServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      admin_level <- adminLevelInputServer('admin_level')
      region <- regionInputServer('region', cache, admin_level, i18n, allow_select_all = TRUE, show_district = FALSE)

      admin_level_trend <- reactive({
        req(admin_level())
        get_plot_admin_column(admin_level(), region())
      })

      region_trend <- regionInputServer('region_trend', cache, admin_level_trend, i18n, selected_region = region)

      data <- reactive({
        req(cache())
        cache()$countdown_data
      })

      outlier_summary <- reactive({
        req(data(), admin_level())

        data() %>%
          calculate_outliers_summary(admin_level(), region())
      })

      outlier_districts <- reactive({
        req(data(), admin_level(), input$indicator, input$year)

        data() %>%
          list_outlier_units(input$indicator, admin_level(), region()) %>%
          filter(year == as.integer(input$year), if (!is.null(region())) adminlevel_1 == region() else TRUE)
      })

      observe({
        req(data())

        years <- data() %>%
          distinct(year) %>%
          arrange(desc(year)) %>%
          pull(year)

        updateSelectizeInput(session, 'year', choices = years)
      })

      output$district_outlier_summary <- renderReactable({
        req(outlier_districts())

        outlier_units <- outlier_districts() %>%
          filter(!!sym(paste0(input$indicator, '_outlier5std')) == 1) %>%
          select(-!!sym(paste0(input$indicator, '_outlier5std')))

        outlier_units %>%
          reactable(
            filterable = FALSE,
            minRows = 10,
            groupBy = 'adminlevel_1',
            columns = list(
              year = colDef(show = FALSE),
              month = colDef(
                aggregate = 'count',
                format = list(
                  aggregated = colFormat(suffix = ' month(s)')
                )
              )
            ),
            defaultColDef = colDef(
              cell = function(value) {
                if (!is.numeric(value)) {
                  return(value)
                }
                format(round(value), nsmall = 0)
              }
            )
          )
      })

      output$district_outlier_heatmap <- renderPlotly({
        req(outlier_summary())
        ggplotly(plot(outlier_summary(), 'heat_map', input$indicator))
      })

      output$region_bar_graph <- renderCustomPlot({
        req(outlier_summary(), input$indicator)
        plot(outlier_summary(), 'region', input$indicator)
      })

      output$indicator_bar_graph <- renderCustomPlot({
        req(outlier_summary())
        plot(outlier_summary(), 'indicator', input$indicator)
      })

      output$district_trend <- renderCustomPlot({
        req(outlier_districts(), region_trend())
        plot(outlier_districts(), region_trend())
      })

      downloadPlot(
        id = 'district_outlier_heatmap_plot',
        filename = reactive('district_outlier_heatmap_plot'),
        data = outlier_summary,
        i18n = i18n,
        plot_function = function(data) {
          plot(data, 'heat_map', input$indicator)
        }
      )

      downloadPlot(
        id = 'indicator_bar_graph_plot',
        filename = reactive('indicator_bar_graph_plot'),
        data = outlier_summary,
        i18n = i18n,
        plot_function = function(data) {
          plot(data, 'indicator', input$indicator)
        }
      )

      downloadPlot(
        id = 'region_bar_graph_plot',
        filename = reactive('region_bar_graph_plot'),
        data = outlier_summary,
        i18n = i18n,
        plot_function = function(data) {
          plot(data, 'region', input$indicator)
        }
      )

      downloadExcel(
        id = 'download_data',
        filename = reactive('checks_outlier_detection'),
        data = data,
        i18n = i18n,
        excel_write_function = function(wb, data) {
          completeness_rate <- data() %>% calculate_outliers_summary()
          district_completeness_rate <- data() %>% calculate_district_outlier_summary()

          sheet_name_1 <- i18n$t('title_outliers')
          addWorksheet(wb, sheet_name_1)
          writeData(wb, sheet = sheet_name_1, x = i18n$t('table_outliers'), startCol = 1, startRow = 1)
          writeData(wb, sheet = sheet_name_1, x = completeness_rate, startCol = 1, startRow = 3)

          # Check if sheet exists; if not, add it
          sheet_name_2 <- i18n$t('sheet_district_outliers')
          addWorksheet(wb, sheet_name_2)
          writeData(wb, sheet = sheet_name_2, x = i18n$t('table_district_outliers'), startRow = 1, startCol = 1)
          writeData(wb, sheet = sheet_name_2, x = district_completeness_rate, startCol = 1, startRow = 3)
        }
      )

      downloadExcel(
        id = 'download_outliers',
        filename = reactive(paste0('checks_outlier_districts_', input$indicator, '_', input$year)),
        data = outlier_districts,
        i18n = i18n,
        excel_write_function = function(wb, data) {
          district_outliers_sum <- outlier_districts()

          sheet_name_1 <- i18n$t('title_district_extreme_outlier')
          addWorksheet(wb, sheet_name_1)
          writeData(wb, sheet = sheet_name_1, x = str_glue(i18n$t('title_district_extreme_outlier_gen')), startCol = 1, startRow = 1)
          writeData(wb, sheet = sheet_name_1, x = district_outliers_sum, startCol = 1, startRow = 3)
        },
        label = 'btn_download_outlier'
      )

      contentHeaderServer(
        'outlier_detection',
        cache = cache,
        path = 'numerator-assessment',
        section = 'sec-dqa-outlier',
        i18n = i18n
      )
    }
  )
}
