dhis2BoxUI <- function(id, i18n) {
  ns <- NS(id)

  box(title = i18n$t("title_dhis2_import"),
      status = 'info',
      solidHeader = TRUE,
      width = 12,
      fluidRow(
        column(2, offset = 10, helpButtonUI(ns('dhis2_download'), name = i18n$t('btn_help')), align = 'right')
      ),
      fluidRow(
        column(6, selectizeInput(ns('country'), i18n$t("title_country"), choices = NULL)),
      ),
      fluidRow(
        column(12, dateRangeInput(ns('date'), i18n$t("title_period"),
                                  start = Sys.Date()-1825,
                                  end = Sys.Date(),
                                  format = 'MM yyyy',
                                  separator = i18n$t('title_to'),
                                  startview = 'year'))
      ),
      fluidRow(
        column(6, textInput(ns('username'), label = NULL, placeholder = 'DHIS2 User Name')),
        column(6, passwordInput(ns('password'), label = NULL, placeholder = 'DHIS2 Password'))
      ),
      fluidRow(
        column(4, actionButton(ns('login'), i18n$t("btn_login"), icon = icon('sign-in-alt'),
                               style = 'background-color: #FFEB3B;font-weight: 500;width:100%;')),
        column(12, messageBoxUI(ns('feedback')))
      ),
      fluidRow(
        column(4, downloadButtonUI(ns('master_file'))),
        column(4, downloadButtonUI(ns('excel_file')))
      )
  )
}

dhis2BoxServer <- function(id, i18n) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      messageBox <- messageBoxServer('feedback', i18n = i18n)

      dhis2_data <- reactiveVal()

      countries_df <- reactive({
        cd2030.rmncah:::countries %>%
          filter(!is.na(dhis2_url))
      })

      observe({
        countries <- countries_df()$iso3
        names(countries) <- countries_df()$country

        countries <- c(set_names('0', i18n$t("opt_select_country")), countries)
        updateSelectizeInput(session, 'country', choices = countries)
      })

      cache <- eventReactive(input$login, {

        if (input$country == '0' || length(input$country) == 0) {
          messageBox$update_message('error_country', 'error')
          return(NULL)
        }

        if (input$username == '' || length(input$username) == 0) {
          messageBox$update_message('error_username', 'error')
          return(NULL)
        }

        if (input$password == '' || length(input$password) == 0) {
          messageBox$update_message('error_password', 'error')
          return(NULL)
        }

        # updateActionButton(session, 'login', disabled = TRUE)

        messageBox$update_message('msg_dhis2_connect', 'info')

        server <- countries_df() %>%
          filter(iso3 == input$country) %>%
          pull(dhis2_url)

        merged_data <- tryCatch({
          khis_cred(username = input$username, password = input$password, server = server)

          messageBox$update_message('msg_fetching_data', 'info')

          start_date <- input$date[1]
          end_date <- input$date[2]

          dt <- get_dhis2_hfd(input$country, start_date, end_date, timeout = 300)

          khis_cred_clear()

          messageBox$update_message('msg_saving_data', 'info')

          dhis2_data(dt)

          dt <- cd2030::save_dhis2_master_data(dt)

          cache_instance <- init_CacheConnection(
            rds_path = dt,
            countdown_data = NULL
          )$reactive()

          messageBox$update_message('msg_download_success', 'success')

          return(cache_instance())
        },
        error = function(e) {
          clean_meesage <- clean_error_message(e)
          messageBox$update_message('error_download_error', 'success', list(clean_message = clean_message))

          return(NULL)
        })

        # updateActionButton(session, 'login', disabled = FALSE)

        return(merged_data)
      })

      downloadButtonServer(
        id = 'master_file',
        filename = reactive('master_dataset'),
        extension = reactive('dta'),
        i18n = i18n,
        content = function(file) {
          save_data(cache()$countdown_data, file)
        },
        data = cache,
        label = "btn_download_master_file"
      )

      downloadButtonServer(
        id = 'excel_file',
        filename = reactive('master_dataset'),
        extension = reactive('xlsx'),
        i18n = i18n,
        content = function(file) {
          save_dhis2_excel(dhis2_data(), file)
        },
        data = dhis2_data,
        label = "btn_download_excel_file"
      )

      helpButtonServer(
        id = 'dhis2_download',
        path = 'downloading-data',
        cache = cache
      )

      return(cache)
    }
  )
}
