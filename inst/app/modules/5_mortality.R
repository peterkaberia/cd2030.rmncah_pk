mortalityUI <- function(id, i18n) {
  ns <- NS(id)

  contentDashboard(
    dashboardId = ns('mortality'),
    dashboardTitle = i18n$t('title_mortality'),
    i18n = i18n,

    tabBox(
      title = i18n$t('title_mortality'),
      width = 12,

      tabPanel(title = i18n$t("opt_mmr_inst"), downloadCoverageUI(ns('mmr_inst'))),
      tabPanel(title = i18n$t("opt_sbr_inst"), downloadCoverageUI(ns('sbr_inst'))),
      tabPanel(title = i18n$t("opt_ratio_md_sb"), downloadCoverageUI(ns('ratio_md_sb'))),
      tabPanel(title = i18n$t("opt_nn_inst"), downloadCoverageUI(ns('nn_inst')))
    ),

    tabBox(
      title = i18n$t('title_mortality_completeness'),
      width = 12,

      tabPanel(title = i18n$t("opt_mmr_ratio"), downloadCoverageUI(ns('mmr_ratio'))),
      tabPanel(title = i18n$t("opt_sbr_ratio"), downloadCoverageUI(ns('sbr_ratio')))
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
        req(cache(), cache()$check_inequality_params)
        cache()$calculate_indicator_coverage('national')
      })

      lbr_mean <- reactive({
        req(coverage())
        coverage() %>%
          select(year, cov_instlivebirths_dhis2) %>%
          summarise(lbr_mean = mean(cov_instlivebirths_dhis2)) %>%
          pull(lbr_mean)
      })

      mmr_ratio <- reactive({
        req(mortality_ratio(), lbr_mean())
        mortality_ratio() %>%
          summarise_completeness_ratio('mmr', lbr_mean())
      })

      sbr_ratio <- reactive({
        req(mortality_ratio(), lbr_mean())
        mortality_ratio() %>%
          summarise_completeness_ratio('sbr', lbr_mean())
      })

      downloadCoverageServer(
        id = 'mmr_inst',
        filename = reactive(paste0('mmr_inst_')),
        data_fn = mortality_summary,
        indicator = 'mmr_inst',
        i18n = i18n,
        sheet_name = reactive(i18n$t('opt_mmr_inst'))
      )

      downloadCoverageServer(
        id = 'ratio_md_sb',
        filename = reactive(paste0('ratio_md_sb')),
        data_fn = mortality_summary,
        indicator = 'ratio_md_sb',
        i18n = i18n,
        sheet_name = reactive(i18n$t('opt_ratio_md_sb'))
      )

      downloadCoverageServer(
        id = 'sbr_inst',
        filename = reactive(paste0('sbr_inst')),
        data_fn = mortality_summary,
        indicator = 'sbr_inst',
        i18n = i18n,
        sheet_name = reactive(i18n$t('opt_sbr_inst'))
      )

      downloadCoverageServer(
        id = 'nn_inst',
        filename = reactive(paste0('nn_inst')),
        data_fn = mortality_summary,
        indicator = 'nn_inst',
        i18n = i18n,
        sheet_name = reactive(i18n$t('opt_nn_inst'))
      )

      downloadCoverageServer(
        id = 'mmr_ratio',
        filename = reactive(paste0('mmr_ratio')),
        data_fn = mmr_ratio,
        i18n = i18n,
        sheet_name = reactive(i18n$t('opt_mmr_ratio'))
      )

      downloadCoverageServer(
        id = 'sbr_ratio',
        filename = reactive(paste0('sbr_ratio')),
        data_fn = sbr_ratio,
        i18n = i18n,
        sheet_name = reactive(i18n$t('opt_sbr_ratio'))
      )
    }
  )
}
