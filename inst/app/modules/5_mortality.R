mortalityUI <- function(id, i18n) {
  ns <- NS(id)
  tagList(
    contentHeader(ns('mortality'), i18n$t("title_mortality"), i18n = i18n),
    contentBody(

      tabBox(
        title = i18n$t("title_mortality"),
        width = 12,

        tabPanel(
          title = i18n$t("opt_mmr_inst"),
          fluidRow(
            column(12, plotCustomOutput(ns('mmr_inst'))),
            column(12, tagList(
              column(4, downloadButtonUI(ns('mmr_inst_plot'))),
              column(4, downloadButtonUI(ns('mmr_inst_data')))
            ))
          )
        ),

        tabPanel(
          title = i18n$t("opt_ratio_md_sb"),
          fluidRow(
            column(12, plotCustomOutput(ns('ratio_md_sb'))),
            column(12, tagList(
              column(4, downloadButtonUI(ns('ratio_md_sb_plot'))),
              column(4, downloadButtonUI(ns('ratio_md_sb_data')))
            ))
          )
        ),

        tabPanel(
          title = i18n$t("opt_sbr_inst"),
          fluidRow(
            column(12, plotCustomOutput(ns('sbr_inst'))),
            column(12, tagList(
              column(4, downloadButtonUI(ns('sbr_inst_plot'))),
              column(4, downloadButtonUI(ns('sbr_inst_data')))
            ))
          )
        ),

        tabPanel(
          title = i18n$t("opt_nn_inst"),
          fluidRow(
            column(12, plotCustomOutput(ns('nn_inst'))),
            column(12, tagList(
              column(4, downloadButtonUI(ns('nn_inst_plot'))),
              column(4, downloadButtonUI(ns('nn_inst_data')))
            ))
          )
        )

      ),

      tabBox(
        title = i18n$t("title_mortality_completeness"),
        width = 12,

        tabPanel(
          title = i18n$t("opt_mmr_ratio"),
          fluidRow(
            column(12, plotCustomOutput(ns('mmr_ratio'))),
            column(12, tagList(
              column(4, downloadButtonUI(ns('mmr_ratio_plot'))),
              column(4, downloadButtonUI(ns('mmr_ratio_data')))
            ))
          )
        ),

        tabPanel(
          title = i18n$t("opt_sbr_ratio"),
          fluidRow(
            column(12, plotCustomOutput(ns('sbr_ratio'))),
            column(12, tagList(
              column(4, downloadButtonUI(ns('sbr_ratio_plot'))),
              column(4, downloadButtonUI(ns('sbr_ratio_data')))
            ))
          )
        )

      )
    )
  )
}

mortalityServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      data <- reactive({
        req(cache())
        cache()$adjusted_data
      })

      un_estimates <- reactive({
        req(cache)
        cache()$un_estimates
      })

      mortality_data <- reactive({
        req(cache)
        cache()$un_mortality_estimates
      })

      mortality_summary <- reactive({
        req(data())
        create_mortality_summary(data())
      })

      mortality_ratio <- reactive({
        req(data(), mortality_data())
        create_mortality_ratios(data(), mortality_data())
      })

      coverage <- reactive({
        req(data(), un_estimates(), cache()$survey_year, all(!is.na(cache()$national_estimates)))
        nat_est <- cache()$national_estimates
        calculate_indicator_coverage(data(), un_estimates = un_estimates(), admin_level = 'national',
                                     sbr = nat_est$sbr, nmr = nat_est$nmr, pnmr = nat_est$pnmr,
                                     twin = nat_est$twin_rate, preg_loss = nat_est$preg_loss,
                                     anc1survey = nat_est$anc1, dpt1survey = nat_est$penta1,
                                     survey_year = cache()$survey_year)
      })

      lbr_mean <- reactive({
        req(coverage())
        coverage() %>%
          select(year, cov_instlivebirths_dhis2) %>%
          summarise(lbr_mean = mean(cov_instlivebirths_dhis2)) %>%
          pull(lbr_mean)
      })

      output$mmr_inst <- renderCustomPlot({
        req(mortality_summary())
        plot(mortality_summary(), 'mmr_inst')
      })

      output$ratio_md_sb <- renderCustomPlot({
        req(mortality_summary())
        plot(mortality_summary(), 'ratio_md_sb')
      })

      output$sbr_inst <- renderCustomPlot({
        req(mortality_summary())
        plot(mortality_summary(), 'sbr_inst')
      })

      output$nn_inst <- renderCustomPlot({
        req(mortality_summary())
        plot(mortality_summary(), 'nn_inst')
      })

      output$mmr_ratio <- renderCustomPlot({
        req(mortality_ratio(), lbr_mean())
        plot(ratio_calc, 'mmr', lbr_mean())
      })

      output$sbr_ratio <- renderCustomPlot({
        req(mortality_ratio(), lbr_mean())
        plot(ratio_calc, 'sbr', lbr_mean())
      })
    }
  )
}
