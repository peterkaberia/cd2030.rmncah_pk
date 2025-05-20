lowReportingUI <- function(id, i18n) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('low_reporting'), i18n$t("title_global_coverage"), i18n = i18n),
    contentBody(
      box(
        title = i18n$t("title_analysis_options"),
        status = 'primary',
        width = 12,
        solidHeader = TRUE,
        fluidRow(
          column(3, adminLevelInputUI(ns('admin_level'), i18n)),
          column(3, denominatorInputUI(ns('denominator'), i18n))
        )
      ),

      tabBox(
        title = i18n$t("title_coverage"),
        width = 12,

        tabPanel(
          title = i18n$t("opt_maternal_coverage"),
          fluidRow(
            column(12, plotCustomOutput(ns('maternal'))),
            column(4, downloadButtonUI(ns('maternal_download')))
          )
        ),

        tabPanel(
          title = i18n$t("opt_vaccine_coverage"),
          fluidRow(
            column(12, plotCustomOutput(ns('vaccine'))),
            column(4, downloadButtonUI(ns('vaccine_download')))
          )
        )

      ),

      box(
        title = i18n$t('title_district_low_reporting'),
        status = 'primary',
        collapsible = TRUE,
        width = 6,
        fluidRow(
          column(3, selectizeInput(ns('indicator'), label = i18n$t('title_indicator'), choice = get_all_indicators())),
          # column(3, selectizeInput(ns('year'), label = i18n$t('title_year'), choice = NULL)),
          column(3, offset = 6, downloadButtonUI(ns('download_regions'))),
          column(12, withSpinner(reactableOutput(ns('district_low_reporting'))))
        )
      )
    )
  )
}

lowReportingServer <- function(id, cache, i18n) {

  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      denominator <- denominatorInputServer('denominator', cache)
      admin_level <- adminLevelInputServer('admin_level')
      region <- regionInputServer('region', cache, admin_level, i18n)

      data <- reactive({
        req(cache())
        cache()$adjusted_data
      })

      coverage <- reactive({
        req(cache(), cache()$survey_year, cache()$un_estimates, cache()$wuenic_estimates,
            cache()$regional_survey, all(!is.na(cache()$national_estimates)))

        rates <- cache()$national_estimates

        cache()$adjusted_data %>%
          calculate_coverage(
            admin_level = admin_level(),
            survey_data = cache()$regional_survey,
            wuenic_data = cache()$wuenic_estimates,
            sbr = rates$sbr,
            nmr = rates$nmr,
            pnmr = rates$pnmr,
            twin = rates$twin_rate,
            preg_loss = rates$preg_loss,
            anc1survey = rates$anc1,
            dpt1survey = rates$penta1,
            survey_year = cache()$survey_year,
            subnational_map = cache()$survey_mapping
          )
      })

      matternal_threshold <- reactive({
        req(data(), admin_level(), cache()$survey_year, all(!is.na(cache()$national_estimates)))
        rates <- cache()$national_estimates
        calculate_threshold(data(),
                            admin_level = admin_level(),
                            indicator = 'maternal',
                            sbr = rates$sbr,
                            nmr = rates$nmr,
                            pnmr = rates$pnmr,
                            anc1survey = rates$anc1,
                            dpt1survey = rates$penta1,
                            survey_year = cache()$survey_year,
                            twin = rates$twin_rate,
                            preg_loss = rates$preg_loss)
      })

      vaccine_threshold <- reactive({
        req(data(), admin_level(), cache()$survey_year, all(!is.na(cache()$national_estimates)))
        rates <- cache()$national_estimates
        calculate_threshold(data(),
                            admin_level = admin_level(),
                            indicator = 'vaccine',
                            sbr = rates$sbr,
                            nmr = rates$nmr,
                            pnmr = rates$pnmr,
                            anc1survey = rates$anc1,
                            dpt1survey = rates$penta1,
                            survey_year = cache()$survey_year,
                            twin = rates$twin_rate,
                            preg_loss = rates$preg_loss)
      })

      district_coverage_rate <- reactive({
        req(coverage(), cache()$denominator, input$indicator)

        indicator <- paste0('cov_', input$indicator, '_', cache()$denominator)

        coverage() %>%
          mutate(!!sym(indicator) := round(!!sym(indicator))) %>%
          filter(!!sym(indicator) >= 90) %>%
          select(any_of(c('adminlevel_1', 'district', 'year', indicator)))
      })

      output$maternal <- renderCustomPlot({
        req(matternal_threshold(), denominator())
        plot(matternal_threshold(), denominator = denominator())
      })

      output$vaccine <- renderCustomPlot({
        req(vaccine_threshold(), denominator())
        plot(vaccine_threshold(), denominator = denominator())
      })

      output$district_low_reporting <- renderReactable({
        req(district_coverage_rate())
        district_coverage_rate() %>%
          reactable()
      })

      downloadExcel(
        id = 'download_regions',
        filename = reactive(paste0('district_high_coverage_rate', input$year)),
        data = district_coverage_rate,
        i18n = i18n,
        excel_write_function = function(wb) {
          low_districts <- district_coverage_rate()

          sheet_name_1 <- i18n$t("title_districts_coverage_rate")
          addWorksheet(wb, sheet_name_1)
          writeData(wb, sheet = sheet_name_1, x = i18n$t("title_districts_coverage_rate"), startCol = 1, startRow = 1)
          writeData(wb, sheet = sheet_name_1, x = low_districts, startCol = 1, startRow = 3)
        }
      )

      contentHeaderServer(
        'low_reporting',
        cache = cache,
        objects = pageObjectsConfig(input),
        md_title = i18n$t("title_coverage"),
        md_file = '2_calculate_ratios.md',
        i18n = i18n
      )

    }
  )
}
