equityUI <- function(id, i18n) {
  ns <- NS(id)

  contentDashboard(
    dashboardId = ns('national_inequality'),
    dashboardTitle = i18n$t("title_equity_assessment"),
    i18n = i18n,

    optionsHeader = contentOptions(
      title = i18n$t("title_analysis_options"),
      column(3, selectizeInput(ns('type'), label = i18n$t("title_equity_type"),
                               choices = c('Area' = 'area',
                                           'Maternal Education' = 'meduc',
                                           'Wealth Quintile' = 'wiq')))
    ),

    include_report = TRUE,

    tabBox(
      title = i18n$t("title_equity_analysis"),
      width = 12,

      tabPanel(
        title = i18n$t("opt_penta3"),
        fluidRow(
          column(12, plotCustomOutput(ns('penta3'))),
          column(3, downloadButtonUI(ns('penta3_download')))
        )
      ),

      tabPanel(
        title = i18n$t("opt_mcv1"),
        fluidRow(
          column(12, plotCustomOutput(ns('measles1'))),
          column(3, downloadButtonUI(ns('measles1_download')))
        )
      ),

      tabPanel(
        i18n$t("opt_custom_check"),
        fluidRow(
          column(3, selectizeInput(ns('indicator'),
                                   label = i18n$t("title_indicator"),
                                   choices =  c('Select' = '', get_all_indicators())))
        ),
        fluidRow(
          column(12, plotCustomOutput(ns('custom_check'))),
          column(3, downloadButtonUI(ns('custom_download')))
        )
      )

    )
  )
}

equityServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      wiq <- reactive({
        req(cache())
        cache()$wiq_survey
      })

      meduc <- reactive({
        req(cache())
        cache()$education_survey
      })

      area <- reactive({
        req(cache())
        cache()$area_survey
      })

      penta3_equiplot <- reactive({
        req(wiq(), area(), meduc(), input$type)

        switch(input$type,
               'area' = equiplot_area(area(), 'penta3'),
               'meduc' = equiplot_education(meduc(), 'penta3'),
               'wiq' = equiplot_wealth(wiq(), 'penta3'))
      })

      measles1_equiplot <- reactive({
        req(wiq(), area(), meduc(), input$type)

        switch(input$type,
               'area' = equiplot_area(area(), 'measles1'),
               'meduc' = equiplot_education(meduc(), 'measles1'),
               'wiq' = equiplot_wealth(wiq(), 'measles1'))
      })

      custom_equiplot <- reactive({
        req(wiq(), area(), meduc(), input$type, input$indicator)

        switch(input$type,
               'area' = equiplot_area(area(), input$indicator),
               'meduc' = equiplot_education(meduc(), input$indicator),
               'wiq' = equiplot_wealth(wiq(), input$indicator))
      })

      output$penta3 <- renderCustomPlot({
        req(penta3_equiplot())
        penta3_equiplot()
      })

      output$measles1 <- renderCustomPlot({
        req(measles1_equiplot())
        measles1_equiplot()
      })

      output$custom_check <- renderCustomPlot({
        req(custom_equiplot())
        custom_equiplot()
      })

      downloadPlot(
        id = 'penta3_download',
        filename = reactive(paste0('penta3_', input$type, '_equity')),
        data = penta3_equiplot,
        i18n = i18n,
        plot_function = function(plot) plot
      )

      downloadPlot(
        id = 'measles1_download',
        filename = reactive(paste0('measles1_', input$type, '_equity')),
        data = measles1_equiplot,
        i18n = i18n,
        plot_function = function(plot) plot
      )

      downloadPlot(
        id = 'custom_download',
        filename = reactive(paste0(input$indicator, '_', input$type, '_equity')),
        data = custom_equiplot,
        i18n = i18n,
        plot_function = function(plot) plot
      )

      contentHeaderServer(
        'national_inequality',
        cache = cache,
        path = 'national-inequality',
        section = 'interpretation-of-equiplots',
        i18n = i18n
      )
    }
  )
}
