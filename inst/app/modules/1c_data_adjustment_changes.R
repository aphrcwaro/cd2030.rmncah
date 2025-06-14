adjustmentChangesUI <- function(id, i18n) {
  ns <- NS(id)

  contentDashboard(
    dashboardId = ns('adjustment'),
    dashboardTitle = i18n$t('title_adjustment_changes'),
    i18n = i18n,

    include_report = TRUE,

    tabBox(
      title = i18n$t('title_visualize_changes'),
      width = 12,

      tabPanel(title = i18n$t("opt_live_births"), downloadCoverageUI(ns('live_births'))),
      tabPanel(title = i18n$t("opt_penta1"), downloadCoverageUI(ns('penta1'))),
      tabPanel(title = i18n$t("opt_anc1"), downloadCoverageUI(ns('anc1'))),
      tabPanel(title = i18n$t("opt_ideliv"), downloadCoverageUI(ns('ideliv'))),
      tabPanel(title = i18n$t("opt_opd_under5"), downloadCoverageUI(ns('opd_total'))),
      tabPanel(
        title = i18n$t("opt_custom_check"),
        fluidRow(
          column(3, selectizeInput(ns('indicator'),
                                   label = i18n$t("title_indicator"),
                                   choices = c('Select Indicator' = '', get_all_indicators())))
        ),
        downloadCoverageUI(ns('custom'))
      )
    )
  )
}

adjustmentChangesServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      data <- reactive({
        req(cache())
        cache()$data_with_excluded_years
      })

      k_factors <- reactive({
        req(cache())

        if (cache()$adjusted_flag) {
          cache()$k_factors
        } else {
          c(anc = 0, ideliv = 0, vacc = 0)
        }
      })

      adjustments <- reactive({
        req(data())
        data() %>%
          generate_adjustment_values(adjustment = 'custom', k_factors = k_factors())
      })

      downloadCoverageServer(
        id = 'live_births',
        filename = reactive('live_births'),
        data_fn = adjustments,
        indicator = 'instlivebirths',
        title = i18n$t("figure_live_births_outlier"),
        sheet_name = reactive(i18n$t("opt_live_births")),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'penta1',
        filename = reactive('penta1'),
        data_fn = adjustments,
        indicator = 'penta1',
        title = i18n$t("figure_penta_outlier"),
        sheet_name = reactive(i18n$t("opt_penta1")),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'anc1',
        filename = reactive('anc1'),
        data_fn = adjustments,
        indicator = 'anc1',
        title = i18n$t("figure_anc1_outlier"),
        sheet_name = reactive(i18n$t("opt_anc1")),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'ideliv',
        filename = reactive('ideliv'),
        data_fn = adjustments,
        indicator = 'instdeliveries',
        title = i18n$t("figure_ideliv_outlier"),
        sheet_name = reactive(i18n$t("opt_ideliv")),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'opd_total',
        filename = reactive('opd_total'),
        data_fn = adjustments,
        indicator = 'opd_total',
        title = i18n$t("figure_opd_total_outlier"),
        sheet_name = reactive(i18n$t("opt_opd_under5")),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'custom',
        filename = reactive(paste0(input$indicator, '_plot')),
        data_fn = adjustments,
        indicator = input$indicator,
        sheet_name = reactive(i18n$t("opt_custom_check")),
        i18n = i18n
      )

      contentHeaderServer(
        'adjustment',
        cache = cache,
        path = 'numerator-adjustments',
        section = 'sec-dqa-adjust-outputs',
        i18n = i18n
      )
    }
  )
}
