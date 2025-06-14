healthSystemComparisonUI <- function(id, i18n) {
  ns <- NS(id)

  contentDashboard(
    dashboardId = ns('health_system'),
    dashboardTitle = i18n$t('title_health_system_comparison'),
    i18n = i18n,

    include_report = TRUE,

    tabBox(
      title = i18n$t('title_health_system_comparison'),
      width = 12,

      tabPanel(title = i18n$t("opt_cov_instdeliveries_hstaff"), downloadCoverageUI(ns('cov_instdeliveries_hstaff'))),
      tabPanel(title = i18n$t("opt_ratio_opd_u5_hstaff"), downloadCoverageUI(ns('ratio_opd_u5_hstaff'))),
      tabPanel(title = i18n$t("opt_ratio_opd_u5_hos"), downloadCoverageUI(ns('ratio_opd_u5_hos'))),
      tabPanel(title = i18n$t("opt_ratio_ipd_u5_bed"), downloadCoverageUI(ns('ratio_ipd_u5_bed')))
    )
  )
}

healthSystemComparisonServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      comparison <- reactive({
        req(cache(), cache()$check_inequality_params)
        cache()$calculate_health_system_comparison()
      })

      downloadCoverageServer(
        id = 'cov_instdeliveries_hstaff',
        filename = reactive(paste0('cov_instdeliveries_hstaff_', cache()$maternal_denominator)),
        data_fn = comparison,
        indicator = 'cov_instdeliveries_hstaff',
        denominator = cache()$maternal_denominator,
        sheet_name = reactive('Sheet 1'),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'ratio_opd_u5_hstaff',
        filename = reactive('ratio_opd_u5_hstaff'),
        data_fn = comparison,
        indicator = 'ratio_opd_u5_hstaff',
        sheet_name = reactive('Sheet 1'),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'ratio_opd_u5_hos',
        filename = reactive('ratio_opd_u5_hos'),
        data_fn = comparison,
        indicator = 'ratio_opd_u5_hos',
        sheet_name = reactive('Sheet 1'),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'ratio_ipd_u5_bed',
        filename = reactive('ratio_ipd_u5_bed'),
        data_fn = comparison,
        indicator = 'ratio_ipd_u5_bed',
        sheet_name = reactive('Sheet 1'),
        i18n = i18n
      )

      contentHeaderServer(
        'health_system',
        cache = cache,
        path = 'health-systems-outputs',
        i18n = i18n
      )
    }
  )
}
