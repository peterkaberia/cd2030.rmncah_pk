healthSystemComparisonUI <- function(id, i18n) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('health_sys_national'), i18n$t("title_health_system_comparison"), i18n = i18n),
    contentBody(
      tabBox(
        title = i18n$t("title_health_system_comparison"),
        width = 12,

        tabPanel(
          title = i18n$t("opt_ratio_fac_pop"),
          fluidRow(
            column(12, plotCustomOutput(ns('ratio_fac_pop'))),
            column(3, downloadButtonUI(ns('ratio_fac_pop_download')))
          )
        ),

        tabPanel(
          title = i18n$t("opt_ratio_hstaff_pop"),
          fluidRow(
            column(12, plotCustomOutput(ns('ratio_hstaff_pop'))),
            column(3, downloadButtonUI(ns('ratio_hstaff_pop_download')))
          )
        ),

        tabPanel(
          title = i18n$t("opt_ratio_bed_pop"),
          fluidRow(
            column(12, plotCustomOutput(ns('ratio_bed_pop'))),
            column(3, downloadButtonUI(ns('ratio_bed_pop_download')))
          )
        ),

        tabPanel(
          title = i18n$t("opt_ratio_opd_pop"),
          fluidRow(
            column(12, plotCustomOutput(ns('ratio_opd_pop'))),
            column(3, downloadButtonUI(ns('ratio_opd_pop_download')))
          )
        ),

        tabPanel(
          title = i18n$t("opt_ratio_ipd_pop"),
          fluidRow(
            column(12, plotCustomOutput(ns('ratio_ipd_pop'))),
            column(3, downloadButtonUI(ns('ratio_ipd_pop_download')))
          )
        ),

        tabPanel(
          title = i18n$t("opt_ratio_opd_u5_pop"),
          fluidRow(
            column(12, plotCustomOutput(ns('ratio_opd_u5_pop'))),
            column(3, downloadButtonUI(ns('ratio_opd_u5_pop_download')))
          )
        ),

        tabPanel(
          title = i18n$t("opt_ratio_ipd_u5_pop"),
          fluidRow(
            column(12, plotCustomOutput(ns('ratio_ipd_u5_pop'))),
            column(3, downloadButtonUI(ns('ratio_ipd_u5_pop_download')))
          )
        ),

        tabPanel(
          title = i18n$t("opt_cov_instdeliveries_hstaff"),
          fluidRow(
            column(12, plotCustomOutput(ns('cov_instdeliveries_hstaff'))),
            column(3, downloadButtonUI(ns('cov_instdeliveries_hstaff_download')))
          )
        ),

        tabPanel(
          title = i18n$t("opt_cov_instdeliveries_bed"),
          fluidRow(
            column(12, plotCustomOutput(ns('cov_instdeliveries_bed'))),
            column(3, downloadButtonUI(ns('cov_instdeliveries_bed_download')))
          )
        ),

        tabPanel(
          title = i18n$t("opt_cov_instdeliveries_fac"),
          fluidRow(
            column(12, plotCustomOutput(ns('cov_instdeliveries_fac'))),
            column(3, downloadButtonUI(ns('cov_instdeliveries_fac_download')))
          )
        )

      )
    )
  )
}

healthSystemComparisonServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      comparison <- reactive({
        req(cache())
        calculate_health_system_comparison(cache()$adjusted_data)
      })

      denominator <- reactive({
        req(cache())
        cache()$denominator
      })

      output$ratio_fac_pop <- renderCustomPlot({
        req(comparison())
        plot(comparison(), indicator = 'ratio_fac_pop')
      })

      output$ratio_hstaff_pop <- renderCustomPlot({
        req(comparison())
        plot(comparison(), indicator = 'ratio_hstaff_pop')
      })

      output$ratio_bed_pop <- renderCustomPlot({
        req(comparison())
        plot(comparison(), indicator = 'ratio_bed_pop')
      })

      output$ratio_opd_pop <- renderCustomPlot({
        req(comparison())
        plot(comparison(), indicator = 'ratio_opd_pop')
      })

      output$ratio_ipd_pop <- renderCustomPlot({
        req(comparison())
        plot(comparison(), indicator = 'ratio_ipd_pop')
      })

      output$ratio_opd_u5_pop <- renderCustomPlot({
        req(comparison())
        plot(comparison(), indicator = 'ratio_opd_u5_pop')
      })

      output$ratio_ipd_u5_pop <- renderCustomPlot({
        req(comparison())
        plot(comparison(), indicator = 'ratio_ipd_u5_pop')
      })

      output$cov_instdeliveries_hstaff <- renderCustomPlot({
        req(comparison(), denominator())
        plot(comparison(), indicator = 'cov_instdeliveries_hstaff', denominator = denominator())
      })

      output$cov_instdeliveries_bed <- renderCustomPlot({
        req(comparison())
        plot(comparison(), indicator = 'cov_instdeliveries_bed', denominator = denominator())
      })

      output$cov_instdeliveries_fac <- renderCustomPlot({
        req(comparison())
        plot(comparison(), indicator = 'cov_instdeliveries_fac', denominator = denominator())
      })
    }
  )
}
