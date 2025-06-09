mortalityCompletenessUI <- function(id, i18n) {
  ns <- NS(id)

  contentDashboard(
    dashboardId = ns('mortality'),
    dashboardTitle = i18n$t('title_mortality_completeness'),
    i18n = i18n,
    include_report = TRUE,

    tabBox(
      title = i18n$t('title_mortality_completeness'),
      width = 12,

      tabPanel(title = i18n$t("opt_mmr_ratio"), downloadCoverageUI(ns('mmr_ratio'))),
      tabPanel(title = i18n$t("opt_sbr_ratio"), downloadCoverageUI(ns('sbr_ratio')))
    )
  )
}

mortalityCompletenessServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      mortality_ratio <- reactive({
        req(cache(), cache()$check_mortality_params)
        cache()$create_mortality_summary() %>%
          cache()$create_mortality_ratios()
      })

      mmr_ratio <- reactive({
        req(mortality_ratio(), cache()$check_inequality_params)
        mortality_ratio() %>%
          cache()$summarise_completeness_ratio('mmr')
      })

      sbr_ratio <- reactive({
        req(mortality_ratio(), cache()$check_inequality_params)
        mortality_ratio() %>%
          cache()$summarise_completeness_ratio('sbr')
      })

      downloadCoverageServer(
        id = 'mmr_ratio',
        filename = reactive(paste0('mmr_ratio')),
        data_fn = mmr_ratio,
        i18n = i18n,
        sheet_name = reactive(i18n$t('opt_mmr_ratio'))
      )

      downloadCoverageServer(
        id = 'sbr_ratio',
        filename = reactive(paste0('sbr_ratio')),
        data_fn = sbr_ratio,
        i18n = i18n,
        sheet_name = reactive(i18n$t('opt_sbr_ratio'))
      )

      contentHeaderServer(
        'mortality',
        cache = cache,
        objects = pageObjectsConfig(input),
        md_title = i18n$t("title_national_coverage"),
        md_file = '2_reporting_rate.md',
        i18n = i18n
      )
    }
  )
}
