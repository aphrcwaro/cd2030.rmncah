subnationalInequalityUI <- function(id, i18n) {
  ns <- NS(id)

  contentDashboard(
    dashboardId = ns('subnational_inequality'),
    dashboardTitle = i18n$t('title_subnational_inequality'),
    i18n = i18n,

    optionsHeader = contentOptions(
      title = i18n$t('title_analysis_options'),
      column(3, denominatorInputUI(ns('denominator'), i18n)),
      column(3, regionInputUI(ns('region'), i18n))
    ),

    tabBox(
      title = i18n$t('title_subnational_inequality'),
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

subnationalInequalityServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      denominatorInputServer('denominator', cache, i18n)
      region <- regionInputServer('region', cache, reactive('adminlevel_1'), i18n)

      inequalities <- reactive({
        req(cache(), cache()$check_inequality_params, region())
        cache()$calculate_inequality(admin_level = 'adminlevel_1', region = region())
      })

      anc4_inequality <- reactive({
        req(inequalities())
        inequalities() %>%
          filter_inequality(indicator = 'anc4', denominator = cache()$get_denominator('anc4'))
      })

      ideliv_inequality <- reactive({
        req(inequalities())
        inequalities() %>%
          filter_inequality(indicator = 'instdeliveries', denominator = cache()$get_denominator('instdeliveries'))
      })

      lbw_inequality <- reactive({
        req(inequalities())
        inequalities() %>%
          filter_inequality(indicator = 'low_bweight', denominator = cache()$get_denominator('low_bweight'))
      })

      penta1_inequality <- reactive({
        req(inequalities())
        inequalities() %>%
          filter_inequality(indicator = 'penta1', denominator = cache()$get_denominator('penta1'))
      })

      measles1_inequality <- reactive({
        req(inequalities())
        inequalities() %>%
          filter_inequality(indicator = 'measles1', denominator = cache()$get_denominator('measles1'))
      })

      custom_inequality <- reactive({
        req(inequalities(), input$indicator)
        inequalities() %>%
          filter_inequality(indicator = input$indicator, denominator = cache()$get_denominator(input$indicator))
      })

      downloadCoverageServer(
        id = 'anc4',
        filename = reactive(paste0('anc4_', admin_level(), '_inequality_', cache()$get_denominator('anc4'))),
        data_fn = anc4_inequality,
        i18n = i18n,
        sheet_name = reactive(i18n$t("title_anc4_inequality"))
      )

      downloadCoverageServer(
        id = 'ideliv',
        filename = reactive(paste0('ideliv_', admin_level(), '_inequality_', cache()$get_denominator('instdeliveries'))),
        data_fn = ideliv_inequality,
        i18n = i18n,
        sheet_name = reactive(i18n$t("title_ideliv_inequality"))
      )

      downloadCoverageServer(
        id = 'lbw',
        filename = reactive(paste0('lbw_', admin_level(), '_inequality_', cache()$get_denominator('low_bweight'))),
        data_fn = lbw_inequality,
        i18n = i18n,
        sheet_name = reactive(i18n$t("title_lbw_inequality"))
      )

      downloadCoverageServer(
        id = 'measles1',
        filename = reactive(paste0('measles1_', admin_level(), '_inequality_', cache()$get_denominator('measles1'))),
        data_fn = measles1_inequality,
        i18n = i18n,
        sheet_name = reactive(i18n$t("title_mcv1_inequality"))
      )

      downloadCoverageServer(
        id = 'penta1',
        filename = reactive(paste0('penta1', admin_level(), '_inequality_', cache()$get_denominator('penta1'))),
        data_fn = penta1_inequality,
        i18n = i18n,
        sheet_name = reactive(i18n$t("title_penta1_inequality"))
      )

      downloadCoverageServer(
        id = 'custom',
        filename = reactive(paste0(input$indicator, '_', admin_level(), '_inequality_', cache()$get_denominator(input$indicator))),
        data_fn = custom_inequality,
        i18n = i18n,
        sheet_name = reactive(paste0(input$indicator, ' Inequality'))
      )

      contentHeaderServer(
        'subnational_inequality',
        cache = cache,
        path = 'subnational-inequality',
        i18n = i18n
      )
    }
  )
}
