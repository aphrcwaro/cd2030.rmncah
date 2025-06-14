source('modules/setup/national_rates.R')
source('modules/setup/file_upload.R')
source('modules/setup/survey_setup.R')

setupUI <- function(id, i18n) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('analysis_setup'), i18n$t("title_setup"), i18n = i18n),
    contentBody(
      nationalRatesUI(ns('national_rates'), i18n),
      surveySetupUI(ns('survey_setup'), i18n),
      fileUploadUI(ns('file_uploads'), i18n)
    )
  )
}

setupServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      nationalRatesServer('national_rates', cache)
      fileUploadServer('file_uploads', cache, i18n)
      surveySetupServer('survey_setup', cache, i18n)

      contentHeaderServer(
        'analysis_setup',
        cache = cache,
        path = 'denominator-assessment',
        section = 'sec-denom-analysis-setup',
        i18n = i18n
      )
    }
  )
}
