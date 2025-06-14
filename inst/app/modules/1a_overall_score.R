overallScoreUI <- function(id, i18n) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('data_quality'), i18n$t("title_overall"), i18n = i18n, include_report = TRUE),
    contentBody(
      box(
        title = i18n$t("title_overall_score_options"),
        status = 'primary',
        solidHeader = TRUE,
        width = 12,
        fluidRow(
          column(3, regionInputUI(ns('region'), i18n))
        )
      ),
      box(
        title = i18n$t("title_overall"),
        status = 'primary',
        width = 12,
        fluidRow(
          column(12, withSpinner(uiOutput(ns('overall_score'))))
        )
      )
    )
  )
}

overallScoreServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      region <- regionInputServer('region', cache, reactive('adminlevel_1'), i18n, allow_select_all = TRUE, show_district = FALSE)

      data <- reactive({
        req(cache())
        cache()$countdown_data
      })

      output$overall_score <- renderUI({
        req(data())

        years <- unique(data()$year)
        threshold <- cache()$performance_threshold

        dt <- data() %>%
          calculate_overall_score(threshold, region = region()) %>%
          mutate(
            type = case_when(
              no %in% c("1a", "1b", "1c") ~ i18n$t("title_monthly_completeness"),
              no %in% c("2a", "2b") ~ i18n$t("title_extreme_outliers"),
              no %in% c("3a", "3b",'3c', '3d', '4') ~ i18n$t("title_consistency_annual_reporting")
            )
          )

        dt_html <- dt %>%
          as_grouped_data(groups = 'type') %>%
          as_flextable() %>%
          bold(j = 1, i = ~ !is.na(type), bold = TRUE, part = "body") %>%
          bold(i = ~ is.na(type) & no =='4', bold = TRUE, part = "body") %>%
          bold(part = "header", bold = TRUE) %>%
          colformat_double(i = ~ is.na(type) & !no %in% c("3a", "3b"), j = as.character(years), digits = 0, big.mark = ",") %>%
          colformat_double(i = ~ is.na(type) & no %in% c("3a", "3b"), j = as.character(years), digits = 2) %>%
          bg(
            i = ~ is.na(type) & !no %in% c("3a", "3b"),
            j = as.character(years),
            bg = function(x) {
              result <- map_chr(as.list(x), ~ {
                if (is.na(.x) || is.null(.x)) {
                  return("transparent")
                } else if (.x >= threshold) {
                  return("seagreen")
                } else if (.x >= 70 && .x < threshold) {
                  return("yellow")
                } else if (.x < 70) {
                  return("red")
                } else {
                  return("transparent")
                }
              })
              return(result)
            },
            part = "body"
          ) %>%
          bg(
            i = ~ !is.na(type), part = "body",
            bg = 'lightgoldenrodyellow'
          ) %>%
          theme_vanilla() %>%
          autofit() %>%
          htmltools_value(ft.align = 'center')

        HTML(as.character(dt_html))
      })

      contentHeaderServer(
        'data_quality',
        cache = cache,
        path = 'numerator-assessment',
        section = 'sec-dqa-overall-score',
        i18n = i18n
      )

    }
  )
}
