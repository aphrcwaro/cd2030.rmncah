helpButtonUI <- function(id, name) {
  ns <- NS(id)
  actionButton(
    inputId = ns('help'),
    label = name,
    icon = shiny::icon('question'),
    class ='btn bg-aqua btn-flat btn-sm',
    style = 'margin-left:4px;'
  )
}

helpButtonServer <- function(id, path, section = NULL, cache) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      observeEvent(input$help, {
        req(cache(), input$help)

        lang_code <- switch(
          cache()$language,
          en = '',
          fr = 'fr',
          pt = 'pt'
        )

        # Construct the URL concisely
        url <- paste0(
          'https://aphrcwaro.github.io/rmncah_guide/',
          ifelse(lang_code == '', '', paste0(lang_code, '/')),
          'pages/',
          path,
          ifelse(lang_code == '', '', paste0('.', lang_code)),
          '.html'
        )
        if (!is.null(section)) {
          url <- paste0(url, '#', section)
        }
        browseURL(url)
      })
    }
  )
}
