removeYearsUI <- function(id, i18n) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('remove_years'), i18n$t("btn_remove_years"), i18n = i18n),
    contentBody(
      fluidRow(
        column(
          8,
          offset = 2,
          box(
            title = i18n$t("title_remove_years"),
            status = 'danger',
            width = 12,
            solidHeader = TRUE,
            fluidRow(
              column(12, tags$p(
                i18n$t("msg_removal_info"),
                style = "color: red; font-weight: bold; margin-bottom: 15px;"
              ))
            ),

            fluidRow(
              column(8, offset = 2, selectizeInput(ns('year_to_remove'),
                                                   label = i18n$t("title_select_removal_years"),
                                                   choices = NULL,
                                                   multiple = TRUE,
                                                   options = list(placeholder = 'Choose years to remove...'))),
            ),

            fluidRow(
              column(
                8,
                offset = 2,
                actionButton(ns('remove_year'),
                             label = i18n$t("btn_confirm_removal"),
                             icon = icon('wrench'),
                             style = 'background-color: #FFEB3B;font-weight: 500;width:100%; margin-top: 15px;')
              ),
              column(8, offset = 2, messageBoxUI(ns('remove_feedback')))

            )
          )
        )
      )
    )
  )
}

removeYearsServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      messageBox <- messageBoxServer('remove_feedback',
                                     i18n = i18n,
                                     default_message = 'msg_no_years_removed')

      data <- reactive({
        req(cache())
        cache()$countdown_data
      })

      excluded_years <- reactive({
        req(cache())
        cache()$excluded_years
      })

      observe({
        req(cache())

        years <- data() %>%
          distinct(year) %>%
          pull(year)

        updateSelectInput(session, 'year_to_remove', choices = years)
      })

      observeEvent(excluded_years(), {
        req(cache(), data())

        if (length(excluded_years()) > 0) {
          updateSelectInput(session, 'year_to_remove', selected = excluded_years())
          list_years <- paste(excluded_years(), collapse = ', ')
          messageBox$update_message('msg_removed_year',  'success', list(years = list_years))
        } else {
          messageBox$update_message('msg_no_years_removed', 'info')
        }
      })

      observeEvent(input$remove_year, {
        req(cache())
        cache()$set_excluded_years(as.numeric(input$year_to_remove))
      })

      contentHeaderServer(
        'remove_years',
        cache = cache,
        path = 'numerator-adjustments',
        section = 'sec-remove-years',
        i18n = i18n
      )
    }
  )
}
