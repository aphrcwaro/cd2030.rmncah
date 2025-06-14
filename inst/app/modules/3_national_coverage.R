nationalCoverageUI <- function(id, i18n) {
  ns <- NS(id)

  contentDashboard(
    dashboardId = ns('national_coverage'),
    dashboardTitle = i18n$t('title_national_coverage'),
    i18n = i18n,

    optionsHeader = contentOptions(
      title = i18n$t('title_analysis_options'),
      column(3, denominatorInputUI(ns('denominator'), i18n))
    ),

    tabBox(
      title = i18n$t('title_national_coverage'),
      width = 12,

      tabPanel(title = i18n$t("opt_anc4"), downloadCoverageUI(ns('anc4'))),
      tabPanel(title = i18n$t("opt_ideliv"), downloadCoverageUI(ns('ideliv'))),
      tabPanel(title = i18n$t("opt_lbw"), downloadCoverageUI(ns('lbw'))),
      tabPanel(title = i18n$t("opt_penta1"), downloadCoverageUI(ns('penta1'))),
      tabPanel(title = i18n$t("opt_mcv1"), downloadCoverageUI(ns('measles1'))),
      tabPanel(
        title = i18n$t("opt_custom_check"),
        fluidRow(
          column(3, selectizeInput(ns('indicator'), label = i18n$t("title_indicator"),
                                   choices = c('Select' = '', get_indicator_without_opd_ipd())))
        ),
        downloadCoverageUI(ns('custom'))
      )
    )
  )
}

nationalCoverageServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      denominatorInputServer('denominator', cache, i18n)

      coverage <- reactive({
        req(cache(), cache()$check_coverage_params)
        cache()$calculate_coverage('national')
      })

      anc4_coverage <- reactive({
        req(coverage())
        coverage() %>%
          filter_coverage('anc4', denominator = cache()$get_denominator('anc4'))
      })

      ideliv_coverage <- reactive({
        req(coverage())
        coverage() %>%
          filter_coverage('instdeliveries', denominator = cache()$get_denominator('instdeliveries'))
      })

      lbw_coverage <- reactive({
        req(coverage())
        coverage() %>%
          filter_coverage('low_bweight', denominator = cache()$get_denominator('low_bweight'))
      })

      penta1_coverage <- reactive({
        req(coverage())
        coverage() %>%
          filter_coverage('penta1', denominator = cache()$get_denominator('penta1'))
      })

      measles1_coverage <- reactive({
        req(coverage())
        coverage() %>%
          filter_coverage('measles1', denominator = cache()$get_denominator('measles1'))
      })

      custom_coverage <- reactive({
        req(coverage())
        coverage() %>%
          filter_coverage(input$indicator, denominator = cache()$get_denominator(input$indicator))
      })

      downloadCoverageServer(
        id = 'anc4',
        filename = reactive(paste0('anc4_survey_', cache()$get_denominator('anc4'))),
        data_fn = anc4_coverage,
        sheet_name = reactive(i18n$t("title_anc4")),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'ideliv',
        filename = reactive(paste0('ideliv_survey_', cache()$get_denominator('instdeliveries'))),
        data_fn = ideliv_coverage,
        sheet_name = reactive(i18n$t("title_ideliv_coverage")),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'lbw',
        filename = reactive(paste0('lbw_survey_', cache()$get_denominator('low_bweight'))),
        data_fn = lbw_coverage,
        sheet_name = reactive(i18n$t("title_lbw_coverage")),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'penta1',
        filename = reactive(paste0('penta1_survey_', cache()$get_denominator('penta1'))),
        data_fn = penta1_coverage,
        sheet_name = reactive(i18n$t("title_penta1_coverage")),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'measles1',
        filename = reactive(paste0('measles1_survey_', cache()$get_denominator('measles1'))),
        data_fn = measles1_coverage,
        sheet_name = reactive(i18n$t("title_mcv1_coverage")),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'custom',
        filename = reactive(paste0(input$indicator, '_survey_', cache()$get_denominator(input$indicator))),
        data_fn = custom_coverage,
        sheet_name = reactive(paste(input$indicator, i18n$t("title_coverage"))),
        i18n = i18n
      )

      contentHeaderServer(
        'national_coverage',
        cache = cache,
        path = 'national-coverage',
        i18n = i18n
      )
    }
  )
}
