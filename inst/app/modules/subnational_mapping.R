subnationalMappingUI <- function(id, i18n) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('subnational_mapping'), i18n$t("title_subnational_mapping"), i18n = i18n),
    contentBody(
      box(
        title = i18n$t("title_analysis_options"),
        status = 'primary',
        width = 12,
        solidHeader = TRUE,
        fluidRow(
          column(3, adminLevelInputUI(ns('admin_level'), i18n)),
                                               #'District' = 'district'))), # District is not support now
          column(3, denominatorInputUI(ns('denominator'), i18n)),
          column(3, selectizeInput(ns('years'), label = i18n$t("title_select_years"), choice = NULL, multiple = TRUE)),
          column(3, selectizeInput(ns('palette'), label = i18n$t("title_palette"), choices = NULL))
        )
      ),

      tabBox(
        id = ns('coverage'),
        title = i18n$t("title_coverage_level"),
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
                                     choices = c('Select' = '0', get_indicator_without_opd_ipd())))
          ),
          fluidRow(
            column(12, plotCustomOutput(ns('custom'))),
            column(3, downloadButtonUI(ns('custom_download'))),
          )
        )
      )
    )
  )
}

subnationalMappingServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      admin_level <- adminLevelInputServer('admin_level')
      denominator <- denominatorInputServer('denominator', cache)

      data <- reactive({
        req(cache())
        cache()$adjusted_data
      })

      country <- reactive({
        req(cache())
        cache()$country
      })

      un_estimates <- reactive({
        req(cache())
        cache()$un_estimates
      })

      dt <- reactive({
        req(data(), cache()$survey_year, un_estimates(), all(!is.na(cache()$national_estimates)))


        data() %>%
          get_mapping_data(un_estimates = un_estimates(),
                           rates = cache()$national_estimates,
                           survey_year = cache()$survey_year,
                           subnational_map = cache()$map_mapping)
      })

      years <- reactive({
        req(cache())
        cache()$mapping_years
      })

      observe({
        req(data())

        survey_years <- data() %>%
          distinct(year) %>%
          arrange(year) %>%
          pull(year)

        survey_years <- c('All years' = '', survey_years)
        updateSelectizeInput(session, 'years', choices = survey_years, selected = years())
      })

      observeEvent(input$years, {
        req(cache())
        cache()$set_mapping_years(as.integer(input$years))
      })

      observe({
        req(input$coverage, input$indicator)

        palette <- if (grepl('drop', input$coverage)) {
          c("Reds", "Purples")
        } else if (grepl('Coverage', input$coverage)) {
          c("Greens", "Blues")
        } else {
          if (grepl('drop|under|zero', input$indicator)) {
            c("Reds", "Purples")
          } else {
            c("Greens", "Blues")
          }
        }

        updateSelectizeInput(session, 'palette', choices = palette)
      })

      output$anc4 <- renderCustomPlot({
        req(dt())

        title <- str_glue(i18n$t("title_distribution_of_anc4"))
        plot(dt(), indicator = 'anc4',
             denominator = denominator(),
             palette = input$palette,
             title = title,
             plot_year = years())
      })

      output$ideliv <- renderCustomPlot({
        req(dt())

        title <- str_glue(i18n$t("title_distribution_of_ideliv"))
        plot(dt(), indicator = 'instdeliveries',
             denominator = denominator(),
             palette = input$palette,
             title = title,
             plot_year = years())
      })

      output$lbw <- renderCustomPlot({
        req(dt())

        title <- str_glue(i18n$t("title_distribution_of_lbw"))
        plot(dt(), indicator = 'low_bweight',
             denominator = denominator(),
             palette = input$palette,
             title = title,
             plot_year = years())
      })


      output$penta1 <- renderCustomPlot({
        req(dt())

        title <- str_glue(i18n$t("title_distribution_of_penta1"))
        plot(dt(), indicator = 'penta1',
             denominator = denominator(),
             palette = input$palette,
             title = title,
             plot_year = years())
      })

      output$measles1 <- renderCustomPlot({
        req(dt())

        title <- str_glue(i18n$t("title_distribution_of_measles1"))
        plot(dt(), indicator = 'measles1',
             denominator = denominator(),
             palette = input$palette,
             title = title,
             plot_year = years())
      })

      output$custom <- renderCustomPlot({
        req(dt(), input$indicator != '0')

        title <- str_glue(i18n$t("title_distribution_of_indicator"))
        plot(dt(), indicator = input$indicator,
             denominator = denominator(),
             palette = input$palette,
             title = title,
             plot_year = years())
      })

      downloadPlot(
        id = 'anc4_download',
        filename = reactive(paste0('anc4_', admin_level(), '_map_', denominator())),
        data = dt,
        i18n = i18n,
        plot_function = function() {
          plot(dt(), indicator = 'anc4',
               denominator = denominator(),
               palette = input$palette,
               title = str_glue(i18n$t("title_distribution_of_anc4")),
               plot_year = years())
        }
      )

      downloadPlot(
        id = 'ideliv_download',
        filename = reactive(paste0('ideliv_', admin_level(), '_map_', denominator())),
        data = dt,
        i18n = i18n,
        plot_function = function() {
          plot(dt(), indicator = 'instdeliveries',
               denominator = denominator(),
               palette = input$palette,
               title = str_glue(i18n$t("title_distribution_of_ideliv")),
               plot_year = years())
        }
      )

      downloadPlot(
        id = 'lbw_download',
        filename = reactive(paste0('lbw_', admin_level(), '_map_', denominator())),
        data = dt,
        i18n = i18n,
        plot_function = function() {
          plot(dt(), indicator = 'lbw',
               denominator = denominator(),
               palette = input$palette,
               title = str_glue(i18n$t("title_distribution_of_lbw")),
               plot_year = years())
        }
      )

      downloadPlot(
        id = 'penta3_download',
        filename = reactive(paste0('penta3_', admin_level(), '_map_', denominator())),
        data = dt,
        i18n = i18n,
        plot_function = function() {
          plot(dt(), indicator = 'penta3',
               denominator = denominator(),
               palette = input$palette,
               title = str_glue(i18n$t("title_distribution_of_penta3")),
               plot_year = years())
        }
      )

      downloadPlot(
        id = 'mcv1_download',
        filename = reactive(paste0('mcv1_', admin_level(), '_map_', denominator())),
        data = dt,
        i18n = i18n,
        plot_function = function() {
          plot(dt(), indicator = 'measles1',
               denominator = denominator(),
               palette = input$palette,
               title = str_glue(i18n$t("title_distribution_of_measles1")),
               plot_year = years())
        }
      )

      downloadPlot(
        id = 'custom_download',
        filename = reactive(paste0(input$indicator, '_', admin_level(), '_map_', denominator())),
        data = dt,
        i18n = i18n,
        plot_function = function() {
          plot(dt(), indicator = input$indicator,
               denominator = denominator(),
               palette = input$palette,
               title = str_glue(i18n$t("title_distribution_of_indicator")),
               plot_year = years())
        }
      )

      contentHeaderServer(
        'subnational_mapping',
        cache = cache,
        objects = pageObjectsConfig(input),
        md_title = i18n$t("title_subnational_mapping"),
        md_file = '2_reporting_rate.md',
        i18n = i18n
      )
    }
  )
}
