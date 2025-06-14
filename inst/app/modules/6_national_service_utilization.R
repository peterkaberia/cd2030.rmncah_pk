nationalServiceUtilizationUI <- function(id, i18n) {
  ns <- NS(id)

  contentDashboard(
    dashboardId = ns('service_utilization'),
    dashboardTitle = i18n$t("title_national_utilization"),
    i18n = i18n,

    optionsHeader = contentOptions(
      title = i18n$t('title_analysis_options'),
      column(3, selectizeInput(ns('years'), label = i18n$t("title_select_years"), choice = NULL, multiple = TRUE))
    ),

    include_report = TRUE,

    tabBox(
      title = i18n$t('title_national_utilization'),
      width = 12,

      tabPanel(title = i18n$t("opt_opd_visits"), downloadCoverageUI(ns('opd'))),
      tabPanel(title = i18n$t("opt_ipd_admissions"), downloadCoverageUI(ns('ipd'))),
      tabPanel(title = i18n$t("opt_under5"), downloadCoverageUI(ns('under5'))),
      tabPanel(title = i18n$t("opt_cfr"), downloadCoverageUI(ns('cfr'))),
      tabPanel(title = i18n$t("opt_deaths"), downloadCoverageUI(ns('deaths'))),
      tabPanel(title = i18n$t("opt_opd_map"), downloadCoverageUI(ns('opd_map'))),
      tabPanel(title = i18n$t("opt_ipd_map"), downloadCoverageUI(ns('ipd_map')))
    )
  )
}

nationalServiceUtilizationServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      utilization_data <- reactive({
        req(cache(), cache()$adjusted_data)
        cache()$compute_service_utilization('national')
      })

      admin1_utilization_data <- reactive({
        req(cache(), cache()$adjusted_data)
        cache()$compute_service_utilization('adminlevel_1')
      })

      opd <- reactive({
        req(utilization_data())
        utilization_data() %>%
          filter_service_utilization_map('opd')
      })

      ipd <- reactive({
        req(utilization_data())
        utilization_data() %>%
          filter_service_utilization_map('ipd')
      })

      under5 <- reactive({
        req(utilization_data())
        utilization_data() %>%
          filter_service_utilization_map('under5')
      })

      under5 <- reactive({
        req(utilization_data())
        utilization_data() %>%
          filter_service_utilization_map('under5')
      })

      cfr <- reactive({
        req(utilization_data())
        utilization_data() %>%
          filter_service_utilization_map('cfr')
      })

      deaths <- reactive({
        req(utilization_data())
        utilization_data() %>%
          filter_service_utilization_map('deaths')
      })

      opd_map <- reactive({
        req(admin1_utilization_data())
        admin1_utilization_data() %>%
          cache()$filter_service_utilization('opd')
      })

      ipd_map <- reactive({
        req(admin1_utilization_data())
        admin1_utilization_data() %>%
          cache()$filter_service_utilization('ipd')
      })

      observe({
        req(cache(), admin1_utilization_data())

        survey_years <- admin1_utilization_data() %>%
          distinct(year) %>%
          arrange(year) %>%
          pull(year)

        survey_years <- c('All years' = '', survey_years)
        updateSelectizeInput(session, 'years', choices = survey_years, selected = cache()$utilization_mapping_years)
      })

      observeEvent(input$years, {
        req(cache())
        cache()$set_utilization_mapping_years(as.integer(input$years))
      })

      downloadCoverageServer(
        id = 'opd',
        filename = reactive('opd_utilization'),
        data_fn = opd,
        sheet_name = reactive(i18n$t("opt_opd_visits")),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'ipd',
        filename = reactive('ipd_utilization'),
        data_fn = ipd,
        sheet_name = reactive(i18n$t("opt_ipd_admissions")),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'under5',
        filename = reactive('under5_utilization'),
        data_fn = under5,
        sheet_name = reactive(i18n$t("opt_under5")),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'cfr',
        filename = reactive('cfr_utilization'),
        data_fn = cfr,
        sheet_name = reactive(i18n$t("opt_cfr")),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'deaths',
        filename = reactive('deaths_utilization'),
        data_fn = deaths,
        sheet_name = reactive(i18n$t("opt_deaths")),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'opd_map',
        filename = reactive('opd_map'),
        data_fn = opd_map,
        sheet_name = reactive(i18n$t("opt_opd_map")),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'ipd_map',
        filename = reactive('ipd_map'),
        data_fn = ipd_map,
        sheet_name = reactive(i18n$t("opt_ipd_map")),
        i18n = i18n
      )

      contentHeaderServer(
        'service_utilization',
        cache = cache,
        path = 'maternal-mortality',
        i18n = i18n
      )
    }
  )
}
