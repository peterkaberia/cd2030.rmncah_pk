subnationalCoverageUI <- function(id, i18n) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('subnational_coverage'), i18n$t('title_subnational_coverage'), i18n = i18n),
    contentBody(
      box(
        title = i18n$t('title_analysis_options'),
        status = 'primary',
        width = 12,
        solidHeader = TRUE,
        fluidRow(
          column(3, adminLevelInputUI(ns('admin_level'), i18n)),
          column(3, regionInputUI(ns('region'), i18n)),
          column(3, denominatorInputUI(ns('denominator'), i18n))
        )
      ),

      tabBox(
        title = i18n$t('title_subnational_coverage_trend'),
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
          i18n$t('opt_custom_check'),
          fluidRow(
            column(3, selectizeInput(ns('indicator'),
                                     label = i18n$t('title_indicator'),
                                     choices = c('Select' = '', get_indicator_without_opd_ipd())))),
          fluidRow(column(12, plotCustomOutput(ns('custom_check'))), downloadCoverageUI(ns('custom_download')))
        )
      )
    )
  )
}

subnationalCoverageServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      admin_level <- adminLevelInputServer('admin_level')
      denominator <- denominatorInputServer('denominator', cache)
      region <- regionInputServer('region', cache, admin_level, i18n)

      survey_data <- reactive({
        req(cache())
        cache()$regional_survey
      })

      vacc_denom <- reactive({
        req(cache())
        cache()$denominator
      })

      mat_denom <- reactive({
        req(cache())
        cache()$maternal_denominator
      })

      coverage <- reactive({
        req(cache(), cache()$survey_year, cache()$un_estimates, cache()$wuenic_estimates,
            survey_data(), all(!is.na(cache()$national_estimates)))

        rates <- cache()$national_estimates

        cache()$adjusted_data %>%
          calculate_coverage(
            admin_level = admin_level(),
            survey_data = survey_data(),
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

      observeEvent(region(), {
        req(cache())
        if (admin_level() == 'adminlevel_1') {
          cache()$set_selected_admin_level_1(region())
        } else {
          cache()$set_selected_district(region())
        }
      })

      output$anc4 <- renderCustomPlot({
        req(coverage(), mat_denom())
        plot(coverage(), indicator = 'anc4', denominator = mat_denom(), region = region())
      })

      output$ideliv <- renderCustomPlot({
        req(coverage(), mat_denom())
        plot(coverage(), indicator = 'instdeliveries', denominator = mat_denom(), region = region())
      })

      output$lbw <- renderCustomPlot({
        req(coverage(), mat_denom())
        plot(coverage(), indicator = 'low_bweight', denominator = mat_denom(), region = region())
      })

      output$penta1 <- renderCustomPlot({
        req(coverage(), denominator())
        plot(coverage(), indicator = 'penta1', denominator = vacc_denom(), region = region())
      })

      output$measles1 <- renderCustomPlot({
        req(coverage(), vacc_denom())
        plot(coverage(), indicator = 'measles1', denominator = vacc_denom(), region = region())
      })

      output$custom_check <- renderCustomPlot({
        req(coverage(), region(), input$indicator)

        denom <- if (is_maternal_indicator(input$indicator)) {
          mat_denom()
        } else {
          vacc_denom()
        }
        plot(coverage(), indicator = input$indicator, denominator = denom(), region = region())
      })

      downloadCoverageServer(
        id = 'anc4_download',
        data = coverage,
        filename = reactive(paste0('anc4_', region(), '_survey_', mat_denom())),
        indicator = reactive('anc4'),
        denominator = mat_denom,
        data_fn = filter_coverage,
        region = region,
        sheet_name = reactive(i18n$t('title_anc4_coverage')),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'ideliv_download',
        data = coverage,
        filename = reactive(paste0('ideliv_', region(), '_survey_', mat_denom())),
        indicator = reactive('instdeliveries'),
        denominator = mat_denom,
        data_fn = filter_coverage,
        region = region,
        i18n = i18n,
        sheet_name = reactive(i18n$t('title_ideliv_coverage'))
      )

      downloadCoverageServer(
        id = 'lbw_download',
        data = coverage,
        filename = reactive(paste0('lbw_', region(), '_survey_', mat_denom())),
        indicator = reactive('low_bweight'),
        denominator = mat_denom,
        data_fn = filter_coverage,
        region = region,
        i18n = i18n,
        sheet_name = reactive(i18n$t('title_lbw_dropout'))
      )

      downloadCoverageServer(
        id = 'penta1_download',
        data = coverage,
        filename = reactive(paste0('penta1_', region(), '_survey_', vacc_denom())),
        indicator = reactive('penta1'),
        denominator = vacc_denom,
        data_fn = filter_coverage,
        region = region,
        i18n = i18n,
        sheet_name = reactive(i18n$t('title_penta1'))
      )

      downloadCoverageServer(
        id = 'measles1_download',
        data = coverage,
        filename = reactive(paste0('measles1_survey_', vacc_denom())),
        indicator = reactive('measles1'),
        denominator = vacc_denom,
        data_fn = filter_coverage,
        sheet_name = reactive(i18n$t("title_mcv1_coverage")),
        region = region,
        i18n = i18n
      )

      # downloadCoverageServer(
      #   id = 'custom_download',
      #   data = coverage,
      #   filename = reactive(paste0(input$indicator, '_', region(), '_survey_', if (is_maternal_indicator(input$indicator)) mat_denom() else vacc_denom())),
      #   indicator = reactive(input$indicator),
      #   denominator = if (is_maternal_indicator(input$indicator)) mat_denom else vacc_denom,
      #   data_fn = filter_coverage,
      #   region = region,
      #   i18n = i18n,
      #   sheet_name = reactive(paste(input$indicator, i18n$t('title_coverage')))
      # )

      contentHeaderServer(
        'subnational_coverage',
        cache = cache,
        objects = pageObjectsConfig(input),
        md_title = i18n$t('title_subnational_coverage'),
        md_file = '2_reporting_rate.md',
        i18n = i18n
      )
    }
  )
}
