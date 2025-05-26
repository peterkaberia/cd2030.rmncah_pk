nationalCoverageUI <- function(id, i18n) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('national_coverage'), i18n$t("title_national_coverage"), i18n = i18n),
    contentBody(
      box(
        title = i18n$t("title_analysis_options"),
        status = 'primary',
        width = 12,
        solidHeader = TRUE,
        fluidRow(column(3, denominatorInputUI(ns('denominator'), i18n)))
      ),

      tabBox(
        title = i18n$t("title_national_coverage_trend"),
        id = 'national_trend',
        width = 12,

        tabPanel(
          title = i18n$t("opt_anc4"),
          fluidRow(
            column(12, plotCustomOutput(ns('anc4'))),
            downloadCoverageUI(ns('anc4_download'))
          )
        ),

        tabPanel(
          title = i18n$t("opt_ideliv"),
          fluidRow(
            column(12, plotCustomOutput(ns('ideliv'))),
            downloadCoverageUI(ns('ideliv_download'))
          )
        ),

        tabPanel(
          title = i18n$t("opt_lbw"),
          fluidRow(
            column(12, plotCustomOutput(ns('lbw'))),
            downloadCoverageUI(ns('lbw_download'))
          )
        ),

        tabPanel(
          title = i18n$t("opt_penta1"),
          fluidRow(
            column(12, plotCustomOutput(ns('penta1'))),
            downloadCoverageUI(ns('penta1_download'))
          )
        ),

        tabPanel(
          title = i18n$t("opt_mcv1"),
          fluidRow(
            column(12, plotCustomOutput(ns('measles1'))),
            downloadCoverageUI(ns('measles1_download'))
          )
        ),

        tabPanel(
          i18n$t("opt_custom_check"),
          fluidRow(
            column(3, selectizeInput(ns('indicator'), label = i18n$t("title_indicator"),
                                     choices = c('Select' = '', get_indicator_without_opd_ipd())))
          ),
          fluidRow(
            column(12, plotCustomOutput(ns('custom_check'))),
            downloadCoverageUI(ns('custom_download'))
          )
        )
      )
    )
  )
}

nationalCoverageServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      denominator <- denominatorInputServer('denominator', cache)

      survey_data <- reactive({
        req(cache())
        cache()$national_survey
      })

      coverage <- reactive({
        req(cache(), cache()$survey_year, cache()$un_estimates, survey_data(),
            cache()$wuenic_estimates, all(!is.na(cache()$national_estimates)))

        rates <- cache()$national_estimates

        cache()$adjusted_data %>%
          calculate_coverage(
            survey_data = survey_data(),
            wuenic_data = cache()$wuenic_estimates,
            un_estimates = cache()$un_estimates,
            sbr = rates$sbr,
            nmr = rates$nmr,
            pnmr = rates$pnmr,
            twin = rates$twin_rate,
            preg_loss = rates$preg_loss,
            anc1survey = rates$anc1,
            dpt1survey = rates$penta1,
            survey_year = cache()$survey_year
          )
      })

      output$anc4 <- renderCustomPlot({
        req(coverage(), denominator())
        plot(coverage(), indicator = 'anc4', denominator = denominator())
      })

      output$ideliv <- renderCustomPlot({
        req(coverage(), denominator())
        plot(coverage(), indicator = 'instdeliveries', denominator = denominator())
      })

      output$lbw <- renderCustomPlot({
        req(coverage(), denominator())
        plot(coverage(), indicator = 'low_bweight', denominator = denominator())
      })

      output$penta1 <- renderCustomPlot({
        req(coverage(), denominator())
        plot(coverage(), indicator = 'penta1', denominator = denominator())
      })

      output$measles1 <- renderCustomPlot({
        req(coverage(), denominator())
        plot(coverage(), indicator = 'measles1', denominator = denominator())
      })

      output$custom_check <- renderCustomPlot({
        req(coverage(), denominator(), input$indicator)
        plot(coverage(), indicator = input$indicator, denominator = denominator())
      })

      downloadCoverageServer(
        id = 'anc4_download',
        data = coverage,
        filename = reactive(paste0('anc4_survey_', denominator())),
        indicator = reactive('anc4'),
        denominator = denominator,
        data_fn = filter_coverage,
        sheet_name = reactive(i18n$t("title_anc4")),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'ideliv_download',
        data = coverage,
        filename = reactive(paste0('ideliv_survey_', denominator())),
        indicator = reactive('instdeliveries'),
        denominator = denominator,
        data_fn = filter_coverage,
        sheet_name = reactive(i18n$t("title_ideliv_coverage")),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'lbw_download',
        data = coverage,
        filename = reactive(paste0('lbw_survey_', denominator())),
        indicator = reactive('low_bweight'),
        denominator = denominator,
        data_fn = filter_coverage,
        sheet_name = reactive(i18n$t("title_lbw_coverage")),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'penta1_download',
        data = coverage,
        filename = reactive(paste0('penta1_survey_', denominator())),
        indicator = reactive('penta1'),
        denominator = denominator,
        data_fn = filter_coverage,
        sheet_name = reactive(i18n$t("title_penta1_coverage")),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'measles1_download',
        data = coverage,
        filename = reactive(paste0('measles1_survey_', denominator())),
        indicator = reactive('measles1'),
        denominator = denominator,
        data_fn = filter_coverage,
        sheet_name = reactive(i18n$t("title_mcv1_coverage")),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'custom_download',
        data = coverage,
        filename = reactive(paste0(input$indicator, '_survey_', denominator())),
        indicator = reactive(input$indicator),
        denominator = denominator,
        data_fn = filter_coverage,
        sheet_name = reactive(paste(input$indicator, i18n$t("title_coverage"))),
        i18n = i18n
      )

      contentHeaderServer(
        'national_coverage',
        cache = cache,
        objects = pageObjectsConfig(input),
        md_title = i18n$t("title_national_coverage"),
        md_file = '2_reporting_rate.md',
        i18n = i18n
      )
    }
  )
}
