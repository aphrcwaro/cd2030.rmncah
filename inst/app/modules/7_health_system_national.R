healthSystemNationalUI <- function(id, i18n) {
  ns <- NS(id)

  contentDashboard(
    dashboardId = ns('health_sys_national'),
    dashboardTitle = i18n$t('title_national_health_system'),
    i18n = i18n,

    box(
      title = i18n$t('opt_health_system_density'),
      width = 12,
      fluidRow(
        column(12, plotCustomOutput(ns('density'))),
        column(3, downloadButtonUI(ns('density_plot_download'))),
        column(3, downloadButtonUI(ns('density_download')))
      )
    )
  )
}

healthSystemNationalServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      national_metrics <- reactive({
        req(cache(), cache()$adjusted_data)
        calculate_health_system_metrics(cache()$adjusted_data, 'national')
      })

      output$density <- renderCustomPlot({
        req(national_metrics())
        plot_national_health_metric(national_metrics(), 'density')
      })

      downloadPlot(
        id = 'density_plot_download',
        filename = reactive('density_plot_download'),
        data = national_metrics,
        plot_function = function(data) {
          plot_national_health_metric(data, 'density')
        },
        i18n = i18n
      )

      downloadExcel(
        id = 'density_download',
        filename = reactive('density_download'),
        data = national_metrics,
        excel_write_function = function(wb, data) {
          sheet_name <- i18n$t('opt_health_system_density')
          addWorksheet(wb, sheet_name)
          writeData(wb, sheet = sheet_name, x = data, startCol = 1, startRow = 1)
        },
        i18n = i18n
      )

      contentHeaderServer(
        'health_sys_national',
        cache = cache,
        path = 'health-systems-analyses',
        i18n = i18n
      )
    }
  )
}
