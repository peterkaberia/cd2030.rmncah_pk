serviceUtilizationUI <- function(id, i18n) {
  ns <- NS(id)
  tagList(
    contentHeader(ns('service_utilization'), i18n$t("title_service_utilization"), i18n = i18n),
    contentBody(
      box(
        title = i18n$t("title_analysis_options"),
        status = 'primary',
        width = 12,
        solidHeader = TRUE,
        fluidRow(
          column(3, adminLevelInputUI(ns('admin_level'), i18n, include_national = TRUE)),
          column(3, uiOutput(ns('region_ui')))
        )
      ),

      tabBox(
        title = i18n$t("title_service_utilization"),
        width = 12,

        tabPanel(
          title = i18n$t("opt_opd_visits"),
          fluidRow(
            column(12, plotCustomOutput(ns('opd'))),
            column(12, tagList(
              column(4, downloadButtonUI(ns('opd_plot'))),
              column(4, downloadButtonUI(ns('opd_data')))
            ))
          )
        ),

        tabPanel(
          title = i18n$t("opt_ipd_admissions"),
          fluidRow(
            column(12, plotCustomOutput(ns('ipd'))),
            column(12, tagList(
              column(4, downloadButtonUI(ns('ipd_plot'))),
              column(4, downloadButtonUI(ns('ipd_data')))
            ))
          )
        ),

        tabPanel(
          title = i18n$t("opt_under5"),
          fluidRow(
            column(12, plotCustomOutput(ns('under5'))),
            column(12, tagList(
              column(4, downloadButtonUI(ns('under5_plot'))),
              column(4, downloadButtonUI(ns('under5_data')))
            ))
          )
        ),

        tabPanel(
          title = i18n$t("opt_cfr"),
          fluidRow(
            column(12, plotCustomOutput(ns('cfr'))),
            column(12, tagList(
              column(4, downloadButtonUI(ns('cfr_plot'))),
              column(4, downloadButtonUI(ns('cfr_data')))
            ))
          )
        ),

        tabPanel(
          title = i18n$t("opt_deaths"),
          fluidRow(
            column(12, plotCustomOutput(ns('deaths'))),
            column(12, tagList(
              column(4, downloadButtonUI(ns('deaths_plot'))),
              column(4, downloadButtonUI(ns('deaths_data')))
            ))
          )
        )
      )
    )
  )
}

serviceUtilizationServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      admin_level <- adminLevelInputServer('admin_level')
      region <- regionInputServer('region', cache, admin_level, i18n)

      data <- reactive({
        req(cache())
        cache()$adjusted_data
      })

      utilization_data <- reactive({
        req(data(), admin_level())
        compute_service_utilization(data(), admin_level())
      })

      output$region_ui <- renderUI({
        req(admin_level())
        if (admin_level() != 'national') {
          regionInputUI(ns('region'), i18n)
        }
      })

      output$opd <- renderCustomPlot({
        req(utilization_data())
        plot(utilization_data(), 'opd', region())
      })

      output$ipd <- renderCustomPlot({
        req(utilization_data())
        plot(utilization_data(), 'ipd', region())
      })

      output$under5 <- renderCustomPlot({
        req(utilization_data())
        plot(utilization_data(), 'under5', region())
      })

      output$cfr <- renderCustomPlot({
        req(utilization_data())
        plot(utilization_data(), 'cfr', region())
      })

      output$deaths <- renderCustomPlot({
        req(utilization_data())
        plot(utilization_data(), 'deaths', region())
      })
    }
  )
}
