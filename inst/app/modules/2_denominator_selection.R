denominatorSelectionUI <- function(id, i18n) {
  ns <- NS(id)

  contentDashboard(
    dashboardId = ns('denominator_selection'),
    dashboardTitle = i18n$t('title_denominator_selection'),
    i18n = i18n,

    optionsHeader = contentOptions(
      title = i18n$t('title_analysis_options'),
      column(3, denominatorInputUI(ns('denominator'), i18n)),
      column(3, denominatorInputUI(ns('maternal_denominator'), i18n))
    ),

    include_report = TRUE,

    tabBox(
      title = i18n$t('title_national_coverage'),
      width = 12,

      tabPanel(title = i18n$t("opt_anc4"), downloadCoverageUI(ns('anc4'))),
      tabPanel(title = i18n$t("opt_ideliv"), downloadCoverageUI(ns('ideliv'))),
      tabPanel(title = i18n$t("opt_lbw"), downloadCoverageUI(ns('lbw'))),
      tabPanel(title = i18n$t("opt_penta3"), downloadCoverageUI(ns('penta3'))),
      tabPanel(title = i18n$t("opt_mcv1"), downloadCoverageUI(ns('measles1'))),
      tabPanel(title = i18n$t("opt_bcg"), downloadCoverageUI(ns('bcg')))
    )
  )
}

denominatorSelectionServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      denominatorInputServer('denominator', cache, i18n, allowInput = TRUE)
      denominatorInputServer('maternal_denominator', cache, i18n, allowInput = TRUE, maternal = TRUE, label = 'title_maternal_denominator')

      survey_estimates <- reactive({
        req(cache())
        cache()$survey_estimates
      })

      indicator_coverage <- reactive({
        req(cache(), cache()$check_inequality_params)
        cache()$calculate_indicator_coverage('national')
      })

      anc4_coverage <- reactive({
        req(indicator_coverage())
        anc4_rate <- unname(survey_estimates()['anc4'])
        indicator_coverage() %>%
          filter_indicator_coverage('anc4', anc4_rate)
      })

      ideliv_coverage <- reactive({
        req(indicator_coverage())
        ideliv_rate <- unname(survey_estimates()['ideliv'])
        indicator_coverage() %>%
          filter_indicator_coverage('instdeliveries', ideliv_rate)
      })

      lbw_coverage <- reactive({
        req(indicator_coverage())
        lbw_rate <- unname(survey_estimates()['lbw'])
        indicator_coverage() %>%
          filter_indicator_coverage('low_bweight', lbw_rate)
      })

      penta3_coverage <- reactive({
        req(indicator_coverage())
        penta3_rate <- unname(survey_estimates()['penta3'])
        indicator_coverage() %>%
          filter_indicator_coverage('penta3', penta3_rate)
      })

      measles1_coverage <- reactive({
        req(indicator_coverage())
        measles1_rate <- unname(survey_estimates()['measles1'])
        indicator_coverage() %>%
          filter_indicator_coverage('measles1', measles1_rate)
      })

      bcg_coverage <- reactive({
        req(indicator_coverage())
        bcg_rate <- unname(survey_estimates()['bcg'])
        indicator_coverage() %>%
          filter_indicator_coverage('bcg', bcg_rate)
      })

      downloadCoverageServer(
        id = 'anc4',
        filename = reactive('anc4_denominator'),
        data_fn = anc4_coverage,
        sheet_name = reactive(i18n$t("title_anc4")),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'ideliv',
        filename = reactive('ideliv_denominator'),
        data_fn = ideliv_coverage,
        sheet_name = reactive(i18n$t("title_ideliv")),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'lbw',
        filename = reactive('lbw_denominator'),
        data_fn = lbw_coverage,
        sheet_name = reactive(i18n$t('title_lbw')),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'penta3',
        filename = reactive('penta3_denominator'),
        data_fn = penta3_coverage,
        sheet_name = reactive(i18n$t('title_penta3')),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'measles1',
        filename = reactive('measles1_denominator'),
        data_fn = measles1_coverage,
        sheet_name = reactive(i18n$t('title_measles1')),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'bcg',
        filename = reactive('bcg_denominator'),
        data_fn = bcg_coverage,
        sheet_name = reactive(i18n$t('title_bcg')),
        i18n = i18n
      )

      contentHeaderServer(
        'denominator_selection',
        cache = cache,
        path = 'denominator-assessment',
        section = 'sec-denominator-selection',
        i18n = i18n
      )
    }
  )
}
