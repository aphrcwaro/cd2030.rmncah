subnationalServiceUtilizationUI <- function(id, i18n) {
  ns <- NS(id)

  contentDashboard(
    dashboardId = ns('service_utilization'),
    dashboardTitle = i18n$t("title_subnational_utilization"),
    i18n = i18n,

    optionsHeader = contentOptions(
      title = i18n$t("title_analysis_options"),
      column(3, regionInputUI(ns('region'), i18n))
    ),

    tabBox(
      title = i18n$t('title_subnational_utilization'),
      width = 12,

      tabPanel(title = i18n$t("opt_opd_visits"), downloadCoverageUI(ns('opd'))),
      tabPanel(title = i18n$t("opt_ipd_admissions"), downloadCoverageUI(ns('ipd'))),
      tabPanel(title = i18n$t("opt_under5"), downloadCoverageUI(ns('under5'))),
      tabPanel(title = i18n$t("opt_cfr"), downloadCoverageUI(ns('cfr'))),
      tabPanel(title = i18n$t("opt_deaths"), downloadCoverageUI(ns('deaths')))
    )
  )
}

subnationalServiceUtilizationServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      region <- regionInputServer('region', cache, reactive('adminlevel_1'), i18n)

      utilization_data <- reactive({
        req(cache(), cache()$adjusted_data, region())
        cache()$compute_service_utilization('adminlevel_1')
      })

      opd <- reactive({
        req(utilization_data())
        utilization_data() %>%
          filter_service_utilization_map('opd', region())
      })

      ipd <- reactive({
        req(utilization_data())
        utilization_data() %>%
          filter_service_utilization_map('ipd', region())
      })

      under5 <- reactive({
        req(utilization_data())
        utilization_data() %>%
          filter_service_utilization_map('under5', region())
      })

      under5 <- reactive({
        req(utilization_data())
        utilization_data() %>%
          filter_service_utilization_map('under5', region())
      })

      cfr <- reactive({
        req(utilization_data())
        utilization_data() %>%
          filter_service_utilization_map('cfr', region())
      })

      deaths <- reactive({
        req(utilization_data())
        utilization_data() %>%
          filter_service_utilization_map('deaths', region())
      })

      downloadCoverageServer(
        id = 'opd',
        filename = reactive('opd_utilization'),
        data_fn = opd,
        sheet_name = reactive(i18n$t("opt_opd_visits")),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'ipd',
        filename = reactive('ipd_utilization'),
        data_fn = ipd,
        sheet_name = reactive(i18n$t("opt_ipd_admissions")),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'under5',
        filename = reactive('under5_utilization'),
        data_fn = under5,
        sheet_name = reactive(i18n$t("opt_under5")),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'cfr',
        filename = reactive('cfr_utilization'),
        data_fn = cfr,
        sheet_name = reactive(i18n$t("opt_cfr")),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'deaths',
        filename = reactive('deaths_utilization'),
        data_fn = deaths,
        sheet_name = reactive(i18n$t("opt_deaths")),
        i18n = i18n
      )

      contentHeaderServer(
        'service_utilization',
        cache = cache,
        path = 'maternal-mortality',
        i18n = i18n
      )
    }
  )
}
