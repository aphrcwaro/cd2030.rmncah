mortalityUI <- function(id, i18n) {
  ns <- NS(id)

  contentDashboard(
    dashboardId = ns('mortality'),
    dashboardTitle = i18n$t('title_mortality_institutional'),
    i18n = i18n,

    tabBox(
      title = i18n$t('title_mortality_institutional'),
      width = 12,

      tabPanel(title = i18n$t("opt_mmr_inst"), downloadCoverageUI(ns('mmr_inst'))),
      tabPanel(title = i18n$t("opt_sbr_inst"), downloadCoverageUI(ns('sbr_inst'))),
      tabPanel(title = i18n$t("opt_ratio_md_sb"), downloadCoverageUI(ns('ratio_md_sb'))),
      tabPanel(title = i18n$t("opt_nn_inst"), downloadCoverageUI(ns('nn_inst')))
    )
  )
}

mortalityServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      mortality_summary <- reactive({
        req(cache(), cache()$check_mortality_params)
        cache()$create_mortality_summary()
      })

      downloadCoverageServer(
        id = 'mmr_inst',
        filename = reactive(paste0('mmr_inst_')),
        data_fn = mortality_summary,
        indicator = 'mmr_inst',
        i18n = i18n,
        sheet_name = reactive(i18n$t('opt_mmr_inst'))
      )

      downloadCoverageServer(
        id = 'ratio_md_sb',
        filename = reactive(paste0('ratio_md_sb')),
        data_fn = mortality_summary,
        indicator = 'ratio_md_sb',
        i18n = i18n,
        sheet_name = reactive(i18n$t('opt_ratio_md_sb'))
      )

      downloadCoverageServer(
        id = 'sbr_inst',
        filename = reactive(paste0('sbr_inst')),
        data_fn = mortality_summary,
        indicator = 'sbr_inst',
        i18n = i18n,
        sheet_name = reactive(i18n$t('opt_sbr_inst'))
      )

      downloadCoverageServer(
        id = 'nn_inst',
        filename = reactive(paste0('nn_inst')),
        data_fn = mortality_summary,
        indicator = 'nn_inst',
        i18n = i18n,
        sheet_name = reactive(i18n$t('opt_nn_inst'))
      )
    }
  )
}
