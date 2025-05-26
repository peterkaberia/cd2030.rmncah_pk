adjustmentChangesUI <- function(id, i18n) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('adjustment_changes'), i18n$t("title_adjustment_changes"), i18n = i18n),
    contentBody(
      tabBox(
        title = i18n$t("title_visualize_changes"),
        id = 'visualize_changes',
        width = 12,

        tabPanel(
          title = i18n$t("opt_live_births"),
          fluidRow(
            column(12, plotCustomOutput(ns('live_births'))),
            column(3, downloadButtonUI(ns('live_births_plot')))
          )
        ),

        tabPanel(
          title = i18n$t("opt_penta1"),
          fluidRow(
            column(12, plotCustomOutput(ns('penta1'))),
            column(3, downloadButtonUI(ns('penta1_plot')))
          )
        ),

        tabPanel(
          title = i18n$t("opt_anc1"),
          fluidRow(
            column(12, plotCustomOutput(ns('anc1'))),
            column(3, downloadButtonUI(ns('anc1_plot')))
          )
        ),

        tabPanel(
          title = i18n$t("opt_ideliv"),
          fluidRow(
            column(12, plotCustomOutput(ns('ideliv'))),
            column(3, downloadButtonUI(ns('ideliv_plot')))
          )
        ),

        tabPanel(
          title = i18n$t("opt_opd_under5"),
          fluidRow(
            column(12, plotCustomOutput(ns('opd_total'))),
            column(3, downloadButtonUI(ns('opd_total_plot')))
          )
        ),

        tabPanel(
          title = i18n$t("opt_custom_check"),
          fluidRow(
            column(3, selectizeInput(ns('indicator'),
                                     label = i18n$t("title_indicator"),
                                     choices = c('Select Indicator' = '', get_all_indicators())))
          ),
          fluidRow(
            column(12, plotCustomOutput(ns('custom_check'))),
            column(3, downloadButtonUI(ns('custom_check_plot')))
          )
        )
      )
    )
  )
}

adjustmentChangesServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      data <- reactive({
        req(cache())
        cache()$data_with_excluded_years
      })

      k_factors <- reactive({
        req(cache())

        if (cache()$adjusted_flag) {
          cache()$k_factors
        } else {
          c(anc = 0, ideliv = 0, vacc = 0)
        }
      })

      adjustments <- reactive({
        req(data())
        data() %>%
          generate_adjustment_values(adjustment = 'custom', k_factors = k_factors())
      })

      output$live_births <- renderCustomPlot({
        req(adjustments())
        plot(adjustments(),
             indicator = 'instlivebirths',
             title = i18n$t("figure_live_births_outlier"))
      })

      output$penta1 <- renderCustomPlot({
        req(adjustments())
        plot(adjustments(),
             indicator = 'penta1',
             title = i18n$t("figure_penta_outlier"))
      })

      output$anc1 <- renderCustomPlot({
        req(adjustments())
        plot(adjustments(),
             indicator = 'anc1',
             title = i18n$t("figure_anc1_outlier"))
      })

      output$ideliv <- renderCustomPlot({
        req(adjustments())
        plot(adjustments(),
             indicator = 'instdeliveries',
             title = i18n$t("figure_ideliv_outlier"))
      })

      output$opd_total <- renderCustomPlot({
        req(adjustments())
        plot(adjustments(),
             indicator = 'opd_total',
             title = i18n$t("figure_opd_total_outlier"))
      })

      output$custom_check <- renderCustomPlot({
        req(adjustments(), input$indicator)

        plot(adjustments(),
             indicator = input$indicator)
      })

      downloadPlot(
        id = 'live_births_plot',
        filename = reactive('live_births_plot'),
        data = adjustments,
        i18n = i18n,
        plot_function = function() {
          plot(adjustments(),
               indicator = 'instlivebirths',
               title = i18n$t("figure_live_births_outlier"))
        }
      )

      downloadPlot(
        id = 'penta1_plot',
        filename = reactive('penta1_plot'),
        data = adjustments,
        i18n = i18n,
        plot_function = function() {
          plot(adjustments(),
               indicator = 'penta1',
               title = i18n$t("figure_penta_outlier"))
        }
      )

      downloadPlot(
        id = 'anc1_plot',
        filename = reactive('anc1_plot'),
        data = adjustments,
        i18n = i18n,
        plot_function = function() {
          plot(adjustments(),
               indicator = 'bcg',
               title = i18n$t("figure_anc1_outlier"))
        }
      )

      downloadPlot(
        id = 'ideliv_plot',
        filename = reactive('ideliv_plot'),
        data = adjustments,
        i18n = i18n,
        plot_function = function() {
          plot(adjustments(),
               indicator = 'ideliv',
               title = i18n$t("figure_ideliv_outlier"))
        }
      )

      downloadPlot(
        id = 'opd_total_plot',
        filename = reactive('opd_total_plot'),
        data = adjustments,
        i18n = i18n,
        plot_function = function() {
          plot(adjustments(),
               indicator = 'opd_total',
               title = i18n$t("figure_opd_total_outlier"))
        }
      )

      downloadPlot(
        id = 'custom_check_plot',
        filename = reactive(paste0(input$indicator, '_plot')),
        data = adjustments,
        i18n = i18n,
        plot_function = function() {
          plot(adjustments(),
               indicator = input$indicator)
        }
      )

      contentHeaderServer(
        'adjustment_changes',
        cache = cache,
        objects = pageObjectsConfig(input),
        md_title = i18n$t("title_adjustment_changes"),
        md_file = '2_reporting_rate.md',
        i18n = i18n
      )
    }
  )
}
