dataCompletenessUI <- function(id, i18n) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('data_completeness'), i18n$t('title_completeness'), i18n = i18n),
    contentBody(
      box(
        title = i18n$t('title_completeness_options'),
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
        title = tags$span(icon('chart-line'), i18n$t('title_completeness_indicators')),
        width = 12,

        tabPanel(
          title = i18n$t('title_heat_map'),
          fluidRow(
            column(12, withSpinner(plotCustomOutput(ns('district_missing_heatmap')))),
            column(4, align = 'right', downloadButtonUI(ns('download_data')))
          )
        ),

        tabPanel(
          title = i18n$t('title_complete_indicators'),
          fluidRow(
            column(12, h5(i18n$t('title_districts_with_complete_data'))),
            column(12, reactableOutput(ns('complete_vaccines')))
          )
        ),

        tabPanel(
          title = i18n$t('title_missing_indicators_by_region'),
          fluidRow(
            column(12, plotCustomOutput(ns('incomplete_region')))
          )
        )
      ),
      box(
        title = i18n$t('title_districts_with_missing_data'),
        status = 'primary',
        width = 6,
        fluidRow(
          column(3, selectizeInput(ns('year'), label = i18n$t('title_year'), choice = NULL)),
          column(3, offset = 6, downloadButtonUI(ns('download_incompletes')))
        ),
        fluidRow(
          column(12, withSpinner(reactableOutput(ns('incomplete_district'))))
        )
      )
    )
  )
}

dataCompletenessServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      admin_level <- adminLevelInputServer('admin_level')
      region <- regionInputServer('region', cache, admin_level, i18n, allow_select_all = TRUE, show_district = FALSE)

      data <- reactive({
        req(cache())
        cache()$countdown_data
      })

      completeness_summary <- reactive({
        req(data(), admin_level())

        data() %>%
          calculate_completeness_summary(admin_level = admin_level(), region = region())
      })

      incomplete_district <- reactive({
        req(data(), input$indicator, input$year)

        list_missing_units(data(), input$indicator, region()) %>%
          filter(year == as.integer(input$year))
      })

      observe({
        req(data())

        years <- data() %>%
          distinct(year) %>%
          arrange(desc(year)) %>%
          pull(year)

        updateSelectizeInput(session, 'year', choices = years)
      })

      output$incomplete_district <- renderReactable({
        req(incomplete_district())

        incomplete_district() %>%
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

      output$district_missing_heatmap <- renderCustomPlot({
        req(completeness_summary())
        # ggplotly(
          plot(completeness_summary(), input$indicator)
        # )
      })

      output$complete_vaccines <- renderReactable({
        req(data(), input$indicator)

        data() %>%
          calculate_district_completeness_summary(region()) %>%
          select(year, contains(input$indicator)) %>%
          reactable(
            filterable = FALSE,
            minRows = 10
          )
      })

      output$incomplete_region <- renderCustomPlot({

        req(completeness_summary())

        all_indicators <- get_all_indicators()
        completeness_summary() %>%
          mutate(across(starts_with('mis_'), ~ 100 - .x)) %>%
          group_by(!!sym(admin_level())) %>%
          select(year, any_of(admin_level()), where(~ any(.x < 100, na.rm = TRUE))) %>%
          pivot_longer(cols = starts_with('mis_'),
                       names_prefix = 'mis_',
                       names_to = 'indicator') %>%
          mutate(facet_label = paste0(!!sym(admin_level()), ': ', indicator)) %>%
          ggplot(aes(y = value, x = year, colour = indicator)) +
          geom_line() +
          geom_point() +
          facet_wrap(~ facet_label) +
          scale_y_continuous(NULL, expand = c(0,0)) +
          theme(
            panel.background = element_blank(),
            strip.background = element_blank(),
            # strip.text = element_text(size = 12)
            panel.grid.major = element_line(colour = 'gray95'),
            axis.ticks = element_blank(),
            legend.position = 'none'
          )

      })

      downloadExcel(
        id = 'download_data',
        filename = reactive('checks_reporting_rate'),
        data = data,
        i18n = i18n,
        excel_write_function = function(wb, data) {
          completeness_rate <- data() %>% calculate_completeness_summary()
          district_completeness_rate <- data() %>% calculate_district_completeness_summary()

          sheet_name_1 <- i18n$t('title_missing')
          addWorksheet(wb, sheet_name_1)
          writeData(wb, sheet = sheet_name_1, x = i18n$t('table_complete_monthly'), startCol = 1, startRow = 1)
          writeData(wb, sheet = sheet_name_1, x = completeness_rate, startCol = 1, startRow = 3)

          # Check if sheet exists; if not, add it
          sheet_name_2 <- i18n$t('sheet_districts_missing')
          addWorksheet(wb, sheet_name_2)
          writeData(wb, sheet = sheet_name_2, x = i18n$t('table_districts_missing'), startRow = 1, startCol = 1)
          writeData(wb, sheet = sheet_name_2, x = district_completeness_rate, startCol = 1, startRow = 3)
        },
        label = 'btn_download_districts'
      )

      downloadExcel(
        id = 'download_incompletes',
        filename = reactive(paste0('checks_incomplete_districts_', input$indicator, '_', input$year)),
        data = incomplete_district,
        i18n = i18n,
        excel_write_function = function(wb, data) {
          sheet_name_1 <- i18n$t('title_districts_with_missing_data_1')
          addWorksheet(wb, sheet_name_1)
          writeData(wb, sheet = sheet_name_1, x = str_glue(i18n$t('title_districts_with_missing_indicator')), startCol = 1, startRow = 1)
          writeData(wb, sheet = sheet_name_1, x = data, startCol = 1, startRow = 3)
        },
        label = 'btn_download_districts'
      )

      contentHeaderServer(
        'data_completeness',
        cache = cache,
        path = 'numerator-assessment',
        section = 'sec-dqa-data-completeness',
        i18n = i18n
      )
    }
  )
}
