healthSystemNationalUI <- function(id, i18n) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('health_sys_national'), i18n$t("title_national_health_system"), i18n = i18n),
    contentBody(
      tabBox(
        title = i18n$t("title_national_health_system"),
        width = 12,

        tabPanel(
          title = i18n$t("opt_health_system_performance"),
          fluidRow(
            column(12, plotCustomOutput(ns('performance'))),
            column(3, downloadButtonUI(ns('performance_download')))
          )
        ),

        tabPanel(
          title = i18n$t("opt_health_system_density"),
          fluidRow(
            column(12, plotCustomOutput(ns('density'))),
            column(3, downloadButtonUI(ns('density_download')))
          )
        )

      )
    )
  )
}

healthSystemNationalServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      national_metrics <- reactive({
        req(cache())
        calculate_health_system_metrics(cache()$adjusted_data, 'national')
      })

      output$performance <- renderCustomPlot({
        req(national_metrics())
        plot_national_health_metric(national_metrics(), 'performance')
      })

      output$density <- renderCustomPlot({
        req(national_metrics())
        plot_national_health_metric(national_metrics(), 'density')
      })

    }
  )
}
