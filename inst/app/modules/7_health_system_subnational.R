healthSystemSubnationalUI <- function(id, i18n) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('health_sys_national'), i18n$t("title_subnational_health_system"), i18n = i18n),
    contentBody(
      tabBox(
        title = i18n$t("title_subnational_health_system"),
        width = 12,

        tabPanel(
          title = i18n$t("opt_score_total"),
          fluidRow(
            column(12, plotCustomOutput(ns('score_total'))),
            column(3, downloadButtonUI(ns('score_total_download')))
          )
        ),

        tabPanel(
          title = i18n$t("opt_score_infrastructure"),
          fluidRow(
            column(12, plotCustomOutput(ns('score_infrastructure'))),
            column(3, downloadButtonUI(ns('score_infrastructure_download')))
          )
        ),

        tabPanel(
          title = i18n$t("opt_score_workforce"),
          fluidRow(
            column(12, plotCustomOutput(ns('score_workforce'))),
            column(3, downloadButtonUI(ns('score_workforce_download')))
          )
        ),

        tabPanel(
          title = i18n$t("opt_score_utilization"),
          fluidRow(
            column(12, plotCustomOutput(ns('score_utilization'))),
            column(3, downloadButtonUI(ns('score_utilization_download')))
          )
        ),

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
        )

      )
    )
  )
}

healthSystemSubnationalServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      nat_metrics <- reactive({
        req(cache())
        calculate_health_system_metrics(cache()$adjusted_data, 'national')
      })

      metric <- reactive({
        req(cache())
        calculate_health_system_metrics(cache()$adjusted_data, 'adminlevel_1')
      })

      output$score_total <- renderCustomPlot({
        req(metric())
        plot(metric(), 'score_total', nat_metrics()$score_total)
      })

      output$score_infrastructure <- renderCustomPlot({
        req(metric())
        plot(metric(), 'score_infrastructure', nat_metrics()$score_infrastructure)
      })

      output$score_workforce <- renderCustomPlot({
        req(metric())
        plot(metric(), 'score_workforce', nat_metrics()$score_workforce)
      })

      output$score_utilization <- renderCustomPlot({
        req(metric())
        plot(metric(), 'score_utilization', nat_metrics()$score_utilization)
      })

      output$ratio_fac_pop <- renderCustomPlot({
        req(metric())
        plot(metric(), 'ratio_fac_pop', nat_metrics()$ratio_fac_pop, 2)
      })

      output$ratio_hstaff_pop <- renderCustomPlot({
        req(metric())
        plot(metric(), 'ratio_hstaff_pop', nat_metrics()$ratio_hstaff_pop, 23)
      })

      output$ratio_bed_pop <- renderCustomPlot({
        req(metric())
        plot(metric(), 'ratio_bed_pop', nat_metrics()$ratio_bed_pop, 25)
      })

      output$ratio_opd_pop <- renderCustomPlot({
        req(metric())
        plot(metric(), 'ratio_opd_pop', nat_metrics()$ratio_opd_pop, 5)
      })

      output$ratio_ipd_pop <- renderCustomPlot({
        req(metric())
        plot(metric(), 'ratio_ipd_pop', nat_metrics()$ratio_ipd_pop, 10)
      })

      output$ratio_opd_u5_pop <- renderCustomPlot({
        req(metric())
        plot(metric(), 'ratio_opd_u5_pop', nat_metrics()$ratio_opd_u5_pop, 5)
      })

      output$ratio_ipd_u5_pop <- renderCustomPlot({
        req(metric())
        plot(metric(), 'ratio_ipd_u5_pop', nat_metrics()$ratio_ipd_u5_pop, 10)
      })

    }
  )
}
