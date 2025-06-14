healthSystemSubnationalUI <- function(id, i18n) {
  ns <- NS(id)

  contentDashboard(
    dashboardId = ns('health_sys_national'),
    dashboardTitle = i18n$t('title_subnational_health_system'),
    i18n = i18n,

    tabBox(
      title = i18n$t('title_subnational_health_system'),
      width = 12,

      tabPanel(title = i18n$t("opt_ratio_fac_pop"), downloadCoverageUI(ns('ratio_fac_pop'))),
      tabPanel(title = i18n$t("opt_ratio_hos_pop"), downloadCoverageUI(ns('ratio_hos_pop'))),
      tabPanel(title = i18n$t("opt_ratio_hstaff_pop"), downloadCoverageUI(ns('ratio_hstaff_pop'))),
      tabPanel(title = i18n$t("opt_ratio_bed_pop"), downloadCoverageUI(ns('ratio_bed_pop')))
    )
  )
}

healthSystemSubnationalServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      nat_metrics <- reactive({
        req(cache(), cache()$adjusted_data)
        calculate_health_system_metrics(cache()$adjusted_data, 'national')
      })

      metric <- reactive({
        req(cache(), cache()$adjusted_data)
        calculate_health_system_metrics(cache()$adjusted_data, 'adminlevel_1') %>%
          select(adminlevel_1, year, total_pop, ratio_fac_pop, ratio_hos_pop, ratio_hstaff_pop, ratio_bed_pop, ratio_opd_pop, ratio_ipd_pop)
      })

      downloadCoverageServer(
        id = 'ratio_fac_pop',
        filename = reactive(paste0('ratio_fac_pop')),
        data_fn = metric,
        indicator = 'ratio_fac_pop',
        national_score = nat_metrics()$ratio_fac_pop,
        target = 2,
        i18n = i18n,
        sheet_name = reactive(i18n$t('opt_ratio_fac_pop'))
      )

      downloadCoverageServer(
        id = 'ratio_hos_pop',
        filename = reactive(paste0('ratio_hos_pop')),
        data_fn = metric,
        indicator = 'ratio_hos_pop',
        national_score = nat_metrics()$ratio_hos_pop,
        target = 2,
        i18n = i18n,
        sheet_name = reactive(i18n$t('opt_ratio_hos_pop'))
      )

      downloadCoverageServer(
        id = 'ratio_hstaff_pop',
        filename = reactive(paste0('ratio_hstaff_pop')),
        data_fn = metric,
        indicator = 'ratio_hstaff_pop',
        national_score = nat_metrics()$ratio_hstaff_pop,
        target = 23,
        i18n = i18n,
        sheet_name = reactive(i18n$t('opt_ratio_hstaff_pop'))
      )

      downloadCoverageServer(
        id = 'ratio_bed_pop',
        filename = reactive(paste0('ratio_bed_pop')),
        data_fn = metric,
        indicator = 'ratio_bed_pop',
        national_score = nat_metrics()$ratio_bed_pop,
        target = 25,
        i18n = i18n,
        sheet_name = reactive(i18n$t('opt_ratio_bed_pop'))
      )

      contentHeaderServer(
        'health_sys_national',
        cache = cache,
        path = 'health-systems-inputs',
        i18n = i18n
      )
    }
  )
}
