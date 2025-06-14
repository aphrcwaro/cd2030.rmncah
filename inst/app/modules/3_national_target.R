nationalTargetUI <- function(id, i18n) {
  ns <- NS(id)

  contentDashboard(
    dashboardId = ns('low_reporting'),
    dashboardTitle = i18n$t('title_global_coverage'),
    i18n = i18n,

    optionsHeader = contentOptions(
      title = i18n$t('title_analysis_options'),
      column(3, adminLevelInputUI(ns('admin_level'), i18n)),
      column(3, denominatorInputUI(ns('denominator'), i18n))
    ),

    tabBox(
      title = i18n$t('title_global_coverage'),
      width = 12,

      tabPanel(title = i18n$t("opt_anc4"), downloadCoverageUI(ns('anc4'))),
      tabPanel(title = i18n$t("opt_ideliv"), downloadCoverageUI(ns('ideliv'))),
      tabPanel(title = i18n$t("opt_vaccine_coverage"), downloadCoverageUI(ns('vaccine')))
    ),

    box(
      title = i18n$t('title_district_low_reporting'),
      status = 'primary',
      collapsible = TRUE,
      width = 6,
      fluidRow(
        column(3, selectizeInput(ns('indicator'), label = i18n$t('title_indicator'), choice = get_indicator_without_opd_ipd())),
        column(3, offset = 6, downloadButtonUI(ns('download_regions'))),
        column(12, withSpinner(reactableOutput(ns('district_low_reporting'))))
      )
    )
  )
}

nationalTargetServer <- function(id, cache, i18n) {

  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      denominatorInputServer('denominator', cache, i18n)
      admin_level <- adminLevelInputServer('admin_level')
      region <- regionInputServer('region', cache, admin_level, i18n)

      indicator_coverage <- reactive({
        req(cache(), cache()$check_inequality_params, admin_level())
        cache()$calculate_indicator_coverage(admin_level())
      })

      anc4_threshold <- reactive({
        req(indicator_coverage(), cache()$maternal_denominator)
        indicator_coverage() %>%
          calculate_threshold(indicator = 'anc4', denominator = cache()$maternal_denominator)
      })

      ideliv_threshold <- reactive({
        req(indicator_coverage(), cache()$maternal_denominator)
        indicator_coverage() %>%
          calculate_threshold(indicator = 'instdeliveries', denominator = cache()$maternal_denominator)
      })

      vaccine_threshold <- reactive({
        req(indicator_coverage(), cache()$denominator)
        indicator_coverage() %>%
          calculate_threshold(indicator = 'vaccine', denominator = cache()$denominator)
      })

      downloadCoverageServer(
        id = 'anc4',
        filename = reactive(paste0('anc4_global_target_', cache()$maternal_denominator)),
        data_fn = anc4_threshold,
        sheet_name = reactive(i18n$t("opt_anc4")),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'ideliv',
        filename = reactive(paste0('ideliv_global_target_', cache()$maternal_denominator)),
        data_fn = ideliv_threshold,
        sheet_name = reactive(i18n$t("opt_ideliv")),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'vaccine',
        filename = reactive(paste0('vaccine_global_target_', cache()$denominator)),
        data_fn = vaccine_threshold,
        sheet_name = reactive(i18n$t("opt_vaccine_coverage")),
        i18n = i18n
      )

      district_coverage_rate <- reactive({
        req(indicator_coverage(), input$indicator)

        denominator <- cache()$get_denominator(input$indicator)

        indicator_coverage() %>%
          filter_high_performers(indicator = input$indicator, denominator = denominator)
      })

      output$district_low_reporting <- renderReactable({
        req(district_coverage_rate())
        district_coverage_rate() %>%
          reactable()
      })

      downloadExcel(
        id = 'download_regions',
        filename = reactive(paste0('district_high_coverage_rate', input$year)),
        data = district_coverage_rate,
        i18n = i18n,
        excel_write_function = function(wb, data) {
          sheet_name_1 <- i18n$t("title_districts_coverage_rate")
          addWorksheet(wb, sheet_name_1)
          writeData(wb, sheet = sheet_name_1, x = i18n$t("title_districts_coverage_rate"), startCol = 1, startRow = 1)
          writeData(wb, sheet = sheet_name_1, x = data, startCol = 1, startRow = 3)
        }
      )

      contentHeaderServer(
        'low_reporting',
        cache = cache,
        path = 'national-global-coverage',
        i18n = i18n
      )

    }
  )
}
