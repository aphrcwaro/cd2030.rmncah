# increase the uploading file size limit to 2000M, now our upload is not just about hfd file, it also include the saved data.
options(shiny.maxRequestSize = 2 * 1024*1024^2)
options(future.globals.maxSize = 3 * 1024 * 1024^2) # 2 GB
options(shiny.fullstacktrace = TRUE)
# options(shiny.error = browser)

# options(shiny.trace = TRUE)
# options(shiny.trace = FALSE)

pacman::p_load(
  shiny,
  shinydashboard,
  shinycssloaders,
  shinyFiles,
  shinyjs,
  cd2030.rmncah,
  dplyr,
  future,
  htmltools,
  openxlsx,
  khisr,
  plotly,
  purrr,
  promises,
  flextable,
  # forcats,
  lubridate,
  RColorBrewer,
  reactable,
  tidyr,
  markdown,
  # sf,
  shiny.i18n,
  stringr ,
  officer,
  officedown,
  waiter
  # update = TRUE
)

source('modules/page_objects_config.R')

source('ui/admin-level-input.R')
source('ui/content_body.R')
source('ui/content_dashboard.R')
source('ui/content_header.R')
source('ui/denominator-input.R')
source('ui/documentation_button.R')
source('ui/download/download_button.R')
source('ui/download/download_coverage.R')
source('ui/download/download_helper.R')
source('ui/download_report.R')
source('ui/help_button.R')
source('ui/message_box.R')
source('ui/render-plot.R')
source('ui/region-input.R')
source('ui/report_button.R')
source('ui/save_cache.R')

source('modules/introduction.R')

source('modules/0_upload_data.R')
source('modules/1a_checks_reporting_rate.R')
source('modules/1a_checks_outlier_detection.R')
source('modules/1a_consistency_check.R')
source('modules/1a_data_completeness.R')
source('modules/1a_calculate_ratios.R')
source('modules/1a_overall_score.R')

source('modules/1b_remove_years.R')

source('modules/1c_data_adjustment_changes.R')
source('modules/1c_data_adjustment.R')

source('modules/setup.R')

source('modules/2_denominator_assessment.R')
source('modules/2_denominator_selection.R')
source('modules/2_derived_coverage.R')

source('modules/3_national_coverage.R')
source('modules/3_national_inequality.R')
source('modules/3_subnational_mapping.R')
source('modules/3_national_target.R')
source('modules/3_equity.R')
source('modules/3_family_planning.R')

source('modules/4_subnational_coverage.R')
source('modules/4_subnational_inequality.R')
source('modules/4_subnational_target.R')

source('modules/5_mortality.R')
source('modules/5_mortality_completeness.R')
source('modules/5_mortality_mapping.R')

source('modules/6_national_service_utilization.R')
source('modules/6_subnational_service_utilization.R')

source('modules/7_health_system_national.R')
source('modules/7_health_system_subnational.R')
source('modules/7_health_system_comparison.R')
source('modules/7_private_sector.R')

i18n <- init_i18n(translation_json_path = 'translation/translation.json')
i18n$set_translation_language ('en')

ui <- dashboardPage(
  skin = 'blue',
  header = dashboardHeader(title = HTML(paste0('RMNCAH<sup>v', packageVersion('cd2030.rmncah'), '</sup>'))),
  sidebar = dashboardSidebar(
    usei18n(i18n),
    selectInput(
      inputId = 'selected_language',
      label = i18n$t('change_language'),
      choices = c('English' = 'en', 'French' = 'fr', 'Portuguese'= 'pt'),
      selected = i18n$get_key_translation()
    ),
    sidebarMenu(
      id = 'tabs',
      menuItem(i18n$t('title_intro'), tabName = 'introduction', icon = icon('info-circle')),
      menuItem(i18n$t('title_load_data'), tabName = 'upload_data', icon = icon('upload')),
      menuItem(i18n$t('title_quality'),
               tabName = 'quality_checks',
               icon = icon('check-circle'),
               startExpanded  = TRUE,
               menuSubItem(i18n$t('title_reporting'),
                           tabName = 'reporting_rate',
                           icon = icon('chart-bar')),
               menuSubItem(i18n$t('title_outlier'),
                           tabName = 'outlier_detection',
                           icon = icon('exclamation-triangle')),
               menuSubItem(i18n$t('title_consistency'),
                           tabName = 'consistency_check',
                           icon = icon('tasks')),
               menuSubItem(i18n$t('title_completeness'),
                           tabName = 'data_completeness',
                           icon = icon('check-square')),
               menuSubItem(i18n$t('title_calculate_ratios'),
                           tabName = 'calculate_ratios',
                           icon = icon('percent')),
               menuSubItem(i18n$t('title_overall'),
                           tabName = 'overall_score',
                           icon = icon('star'))
               ),
      menuItem(i18n$t('btn_remove_years'), tabName = 'remove_years', icon = icon('trash')),
      menuItem(i18n$t('title_adjustment'),
               tabName = 'data_adjustment_1',
               icon = icon('adjust'),
               menuSubItem(i18n$t('title_adjustment'),
                           tabName = 'data_adjustment',
                           icon = icon('adjust')),
               menuSubItem(i18n$t('title_adjustment_changes'),
                           tabName = 'data_adjustment_changes',
                           icon = icon('adjust'))
      ),
      menuItem(i18n$t('title_setup'), tabName = 'setup', icon = icon('sliders-h')),
      menuItem(i18n$t('title_denominator_selection'),
               tabName = 'denom_assess',
               icon = icon('calculator'),
               menuSubItem(i18n$t('title_denominator_assessment'),
                           tabName = 'denominator_assessment',
                           icon = icon('calculator')),
               menuSubItem(i18n$t('title_denominator_selection'),
                           tabName = 'denominator_selection',
                           icon = icon('filter')),
               menuSubItem(i18n$t('title_derived_coverage'),
                           tabName = 'derived_coverage',
                           icon = icon('chart-line'))
               ),
      menuItem(i18n$t('title_national_analysis'),
               tabName = 'national_analysis',
               icon = icon('flag'),
               menuSubItem(i18n$t('title_national_coverage'),
                           tabName = 'national_coverage',
                           icon = icon('flag')),
               menuSubItem(i18n$t('title_global_coverage'),
                           tabName = 'national_target',
                           icon = icon('user-slash')),
               menuSubItem(i18n$t('title_fpet'),
                           tabName = 'fpet_projection',
                           icon = icon('user-slash')),
               menuSubItem(i18n$t('title_national_inequality'),
                           tabName = 'national_inequality',
                           icon = icon('balance-scale-right')),
               menuSubItem(i18n$t('title_subnational_mapping'),
                           tabName = 'subnational_mapping',
                           icon = icon('map')),
               menuSubItem(i18n$t('title_equity_assessment'),
                           tabName = 'equity_assessment',
                           icon = icon('balance-scale'))
              ),
      menuItem(i18n$t('title_subnational_analysis'),
               tabName = 'subnational_analysis',
               icon = icon('globe-africa'),
               menuSubItem(i18n$t('title_subnational_coverage'),
                           tabName = 'subnational_coverage',
                           icon = icon('map-marked')),
               menuSubItem(i18n$t('title_subnational_inequality'),
                           tabName = 'subnational_inequality',
                           icon = icon('balance-scale-right')),
               menuSubItem(i18n$t('title_global_coverage'),
                           tabName = 'subnational_target',
                           icon = icon('user-slash'))
               ),
      menuItem(i18n$t('title_mortality'),
               tabName = 'mortality',
               icon = icon('balance-scale'),
               menuSubItem(i18n$t('title_mortality_institutional'),
                           tabName = 'mortality_institutional',
                           icon = icon('map-marked')),
               menuSubItem(i18n$t('title_mortality_mapping'),
                           tabName = 'mortality_mapping',
                           icon = icon('balance-scale-right')),
               menuSubItem(i18n$t('title_mortality_completeness'),
                           tabName = 'mortality_completeness',
                           icon = icon('user-slash'))
               ),
      menuItem(i18n$t('title_service_utilization'),
               tabName = 'service_utilization',
               icon = icon('balance-scale'),
               menuSubItem(i18n$t('title_national_utilization'),
                           tabName = 'national_utilization',
                           icon = icon('map-marked')),
               menuSubItem(i18n$t('title_subnational_utilization'),
                           tabName = 'subnational_utilization',
                           icon = icon('balance-scale-right'))
               ),
      menuItem(i18n$t('opt_health_system_performance'),
               tabName = 'system_performance',
               icon = icon('globe-africa'),
               menuSubItem(i18n$t('title_national_health_system'),
                           tabName = 'health_system_national',
                           icon = icon('map-marked')),
               menuSubItem(i18n$t('title_subnational_health_system'),
                           tabName = 'health_system_subnational',
                           icon = icon('balance-scale-right')),
               menuSubItem(i18n$t('title_health_system_comparison'),
                           tabName = 'health_system_comparison',
                           icon = icon('user-slash')),
               menuSubItem(i18n$t('title_private_sector'),
                           tabName = 'private_sector',
                           icon = icon('user-slash'))
      )
    )
  ),
  body = dashboardBody(
    useShinyjs(),

    useWaiter(),
    useHostess(),

    waiterShowOnLoad(
      color = '#eaf2f8',
      html = tagList(
        hostess_loader(
          'loader',
          preset = 'bubble',
          text_color = '#3c8dbc',
          class = 'label-center',
          center_page = TRUE,
          stroke_color  = "#3c8dbc"
        ),
        br(),
        tagAppendAttributes(
          style = 'margin-left: -75px',
          p(
            style = "color: #3c8dbc; font-weight: bold;",
            sample(
              c(
                'We are loading the app. Fetching stardust...',
                'The app is almost ready. Summoning unicorns...',
                'Hold on, the app is being loaded! Chasing rainbows...',
                'We are loading the app: teaching squirrels to water ski...',
                'App is loading! Counting clouds...'
                ),
              1
            )
          )
        )
      )
    ),

    tags$head(
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css'),
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'rmd-styles.css'),
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'bootstrap-icons.css'),
      tags$script(src = "jquery.slimscroll.min.js"),
      tags$script(HTML("
        $(function() {
          $('body, html, ' + '.wrapper').css({
            'height'    : 'auto',
            'min-height': '100%'
          });
          $('body').addClass('fixed');
          var windowHeight  = $(window).height();
          var footerHeight  = $('.main-footer').outerHeight() || 0;
          $('.content-wrapper').css('min-height', windowHeight - footerHeight);
          if ($('.main-sidebar').find('slimScrollDiv').length === 0) {
            $('.sidebar').slimScroll({
              height: (windowHeight - $('.main-header').height()) + 'px'
            });
          }
        });
      "))
    ),
    tabItems(
      tabItem(tabName = 'introduction', introductionUI('introduction', i18n = i18n)),
      tabItem(tabName = 'upload_data', uploadDataUI('upload_data', i18n = i18n)),
      tabItem(tabName = 'reporting_rate', reportingRateUI('reporting_rate', i18n = i18n)),
      tabItem(tabName = 'data_completeness', dataCompletenessUI('data_completeness', i18n = i18n)),
      tabItem(tabName = 'consistency_check', consistencyCheckUI('consistency_check', i18n = i18n)),
      tabItem(tabName = 'outlier_detection', outlierDetectionUI('outlier_detection', i18n = i18n)),
      tabItem(tabName = 'calculate_ratios', calculateRatiosUI('calculate_ratios', i18n = i18n)),
      tabItem(tabName = 'overall_score', overallScoreUI('overall_score', i18n = i18n)),
      tabItem(tabName = 'remove_years', removeYearsUI('remove_years', i18n = i18n)),
      tabItem(tabName = 'data_adjustment', dataAjustmentUI('data_adjustment', i18n = i18n)),
      tabItem(tabName = 'data_adjustment_changes', adjustmentChangesUI('data_adjustment_changes', i18n = i18n)),
      tabItem(tabName = 'setup', setupUI('setup', i18n = i18n)),
      tabItem(tabName = 'derived_coverage', derivedCoverageUI('derived_coverage', i18n = i18n)),
      tabItem(tabName = 'denominator_assessment', denominatorAssessmentUI('denominator_assessment', i18n = i18n)),
      tabItem(tabName = 'denominator_selection', denominatorSelectionUI('denominator_selection', i18n = i18n)),
      tabItem(tabName = 'national_coverage', nationalCoverageUI('national_coverage', i18n = i18n)),
      tabItem(tabName = 'subnational_coverage', subnationalCoverageUI('subnational_coverage', i18n = i18n)),
      tabItem(tabName = 'national_inequality', nationalInequalityUI('national_inequality', i18n = i18n)),
      tabItem(tabName = 'subnational_inequality', subnationalInequalityUI('subnational_inequality', i18n = i18n)),
      tabItem(tabName = 'national_target', nationalTargetUI('national_target', i18n = i18n)),
      tabItem(tabName = 'subnational_target', subnationalTargetUI('subnational_target', i18n = i18n)),
      tabItem(tabName = 'subnational_mapping', subnationalMappingUI('subnational_mapping', i18n = i18n)),
      tabItem(tabName = 'equity_assessment', equityUI('equity_assessment', i18n = i18n)),
      tabItem(tabName = 'mortality_institutional', mortalityUI('mortality_institutional', i18n = i18n)),
      tabItem(tabName = 'mortality_mapping', mortalityMappingUI('mortality_mapping', i18n = i18n)),
      tabItem(tabName = 'mortality_completeness', mortalityCompletenessUI('mortality_completeness', i18n = i18n)),
      tabItem(tabName = 'national_utilization', nationalServiceUtilizationUI('national_utilization', i18n = i18n)),
      tabItem(tabName = 'subnational_utilization', subnationalServiceUtilizationUI('subnational_utilization', i18n = i18n)),
      tabItem(tabName = 'health_system_national', healthSystemNationalUI('health_system_national', i18n = i18n)),
      tabItem(tabName = 'health_system_subnational', healthSystemSubnationalUI('health_system_subnational', i18n = i18n)),
      tabItem(tabName = 'health_system_comparison', healthSystemComparisonUI('health_system_comparison', i18n = i18n)),
      tabItem(tabName = 'fpet_projection', familyPlanningUI('fpet_projection', i18n = i18n)),
      tabItem(tabName = 'private_sector', privateSectorUI('private_sector', i18n = i18n))
    ),
    tags$script(src = 'script.js')
  )
)

server <- function(input, output, session) {
  hostess <- Hostess$new('loader', infinite = TRUE)
  hostess$start()

  introductionServer('introduction', selected_language = reactive(input$selected_language))
  cache <- uploadDataServer('upload_data', i18n)
  observeEvent(c(cache(), cache()$language), {
    req(cache())

    update_lang(cache()$language)

    # shinyjs::delay(500, {
    updateHeader(cache()$country, i18n)
    # })
  })

  observeEvent(input$selected_language, {
    if (!isTruthy(cache())) {
      update_lang(input$selected_language)
      updateSelectizeInput(session, input$selected_language)
    } else {
      cache()$set_language(input$selected_language)
      updateSelectizeInput(session, cache()$language)
    }
  })

  reportingRateServer('reporting_rate', cache, i18n)
  dataCompletenessServer('data_completeness', cache, i18n)
  consistencyCheckServer('consistency_check', cache, i18n)
  outlierDetectionServer('outlier_detection', cache, i18n)
  calculateRatiosServer('calculate_ratios', cache, i18n)
  overallScoreServer('overall_score', cache, i18n)
  removeYearsServer('remove_years', cache, i18n)
  dataAdjustmentServer('data_adjustment', cache, i18n)
  adjustmentChangesServer('data_adjustment_changes', cache, i18n)
  setupServer('setup', cache, i18n)
  derivedCoverageServer('derived_coverage', cache, i18n)
  denominatorAssessmentServer('denominator_assessment', cache, i18n)
  denominatorSelectionServer('denominator_selection', cache, i18n)
  nationalCoverageServer('national_coverage', cache, i18n)
  subnationalCoverageServer('subnational_coverage', cache, i18n)
  nationalInequalityServer('national_inequality', cache, i18n)
  subnationalInequalityServer('subnational_inequality', cache, i18n)
  nationalTargetServer('national_target', cache, i18n)
  subnationalTargetServer('subnational_target', cache, i18n)
  subnationalMappingServer('subnational_mapping', cache, i18n)
  equityServer('equity_assessment', cache, i18n)
  mortalityServer('mortality_institutional', cache, i18n)
  mortalityMappingServer('mortality_mapping', cache, i18n)
  mortalityCompletenessServer('mortality_completeness', cache, i18n)
  nationalServiceUtilizationServer('national_utilization', cache, i18n)
  subnationalServiceUtilizationServer('subnational_utilization', cache, i18n)
  healthSystemNationalServer('health_system_national', cache, i18n)
  healthSystemSubnationalServer('health_system_subnational', cache, i18n)
  healthSystemComparisonServer('health_system_comparison', cache, i18n)
  familyPlanningServer('fpet_projection', cache, i18n)
  privateSectorServer('private_sector', cache, i18n)
  downloadReportServer('download_report', cache, i18n)
  saveCacheServe('save_cache', cache, i18n)

  # session$onSessionEnded(stopApp)

  onFlushed(function() {
    hostess$close()
    waiter_hide()
  }, once = TRUE)

  updateHeader <- function(country, i18n) {

    header_title <- div(
      class = 'navbar-header',
      h4(HTML(paste0(country, ' &mdash; ', i18n$t('title_countdown'))), class = 'navbar-brand')
    )

    # Dynamically update the header
    header <- htmltools::tagQuery(
      dashboardHeader(
        title = HTML(paste0('RMNCAH<sup>v', packageVersion('cd2030.rmncah'), '</sup>')),
        saveCacheUI('save_cache'),
        downloadReportUI('download_report', i18n)
      )
    )
    header <- header$
      find('.navbar.navbar-static-top')$      # find the header right side
      append(header_title)$                     # inject dynamic content
      allTags()

    removeUI(selector = 'header.main-header', immediate = TRUE)

    # Replace the header in the UI
    insertUI(
      selector = 'body',
      where = 'afterBegin',
      ui = header
    )
  }
}

shinyApp(ui = ui, server = server)
