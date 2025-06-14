calculateRatiosUI <- function(id, i18n) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('ratios'), i18n$t("title_calculate_ratios"), i18n = i18n),
    contentBody(
      box(
        title = i18n$t("title_ratio_options"),
        status = 'primary',
        solidHeader = TRUE,
        width = 12,
        fluidRow(
          column(3, offset = 1, numericInput(ns('anc1_coverage'),
                                 i18n$t("title_anc1_coverage"),
                                 min = 0, max = 100, value = NA, step = 1)),
          column(3, numericInput(ns('penta1_coverage'),
                                 i18n$t("title_penta1_coverage"),
                                 min = 0, max = 100, value = NA, step = 1)),
          column(3, numericInput(ns('penta3_coverage'),
                                 i18n$t("title_penta3_coverage_pct"),
                                 min = 0, max = 100, value = NA, step = 1))
        )
      ),
      box(
        title = i18n$t("title_ratio_plots"),
        status = 'primary',
        width = 12,
        fluidRow(
          column(12, plotCustomOutput(ns('ratios_plot'))),
          column(4, downloadButtonUI(ns('ratio_plot_download')))
        )
      )
    )
  )
}

calculateRatiosServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      survey_estimates <- reactive({
        req(cache())
        cache()$survey_estimates
      })

      ratio_summary <- reactive({
        req(cache(), all(!is.na(survey_estimates()[c('anc1', 'penta1', 'penta3')])))
        calculate_ratios_summary(cache()$countdown_data,
                                 survey_coverage = survey_estimates())
      })

      observe({
        req(cache())

        estimates <- survey_estimates()
        if (is.null(cache()$survey_source) || cache()$survey_source == 'setup') {
          updateNumericInput(session, 'anc1_coverage', value = unname(estimates["anc1"]))
          updateNumericInput(session, 'penta1_coverage', value = unname(estimates["penta1"]))
          updateNumericInput(session, 'penta3_coverage', value = unname(estimates["penta3"]))
        }
      })

      # Causing a loop the national_rates.R
      observeEvent(c(input$anc1_coverage, input$penta1_coverage, input$penta3_coverage), {
        req(cache())

        if (!is.null(cache()$survey_source) && cache()$survey_source == 'setup') {
          return()
        }

        estimates <- cache()$survey_estimates
        estimates <- c(
          anc1 = as.numeric(input$anc1_coverage),
          penta1 = as.numeric(input$penta1_coverage),
          penta3 = as.numeric(input$penta3_coverage),
          measles1 = unname(estimates['measles1']),
          bcg = unname(estimates['bcg']),
          anc4 = unname(estimates['anc4']),
          ideliv = unname(estimates['ideliv']),
          lbw = unname(estimates['lbw']),
          csection = unname(estimates['csection'])
        )
        cache()$set_survey_estimates(estimates)
        cache()$set_survey_source('ratios')
      })

      output$ratios_plot <- renderCustomPlot({
        req(ratio_summary())
        plot(ratio_summary())
      })

      downloadPlot(
        id = 'ratio_plot_download',
        filename = reactive('ratio_plot'),
        data = ratio_summary,
        i18n = i18n,
        plot_function = function(data) {
          plot(data)
        }
      )

      contentHeaderServer(
        'ratios',
        cache = cache,
        path = 'numerator-assessment',
        section = 'sec-dqa-ratios',
        i18n = i18n
      )
    }
  )
}
