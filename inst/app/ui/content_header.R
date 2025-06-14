contentHeader <- function(id, title, i18n, include_report = FALSE, include_notes = FALSE, include_help = TRUE) {
  ns <- NS(id)
  div(
    class = 'content-header',
    h1(title),
    div(
      class = 'right-buttons',
      if (include_report) reportButtonUI(ns('report'), label = i18n$t("btn_generate_report")),
      if (include_notes) documentationButtonUI(ns('add_notes'), i18n),
      if (include_help) helpButtonUI(ns('get_help'), name = i18n$t('btn_help'))
    )
  )
}

contentHeaderServer <- function(id, cache, path, section = NULL, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      reportButtonServer(
        id = 'report',
        cache = cache,
        report_name = reactive(id),
        i18n = i18n,
        adminlevel_1 = reactive(NULL)
      )

      helpButtonServer(
        id = 'get_help',
        path = path,
        section = section,
        cache = cache
      )

      documentationButtonServer(
        id = 'add_notes',
        cache = cache,
        document_objects = if (!is.null(objects)) objects[[id]] else NULL,
        page_id = id,
        page_name = md_title,
        i18n = i18n
      )
    }
  )
}
