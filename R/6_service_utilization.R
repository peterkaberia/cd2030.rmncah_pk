#' Compute Service Utilization Metrics
#'
#' Calculates annualized service utilization statistics including outpatient (OPD), inpatient (IPD),
#' case fatality rates, and under-5 service use, aggregated either at national or admin level 1.
#'
#' @param .data A data frame containing service and population indicators.
#' @param admin_level Either `"national"` or `"adminlevel_1"` to define the aggregation level.
#'
#' @return A `cd_service_utilization` object (tibble subclass) with summarized and derived indicators:
#' - OPD/IPD totals and rates
#' - Under-5 service utilization
#' - Case fatality rates
#' - Proportion of under-5 deaths
#'
#' @examples
#' \dontrun{
#'   compute_service_utilization(dat, admin_level = "national")
#' }
#'
#' @export
compute_service_utilization <- function(.data, admin_level = c('national', 'adminlevel_1', 'district')) {
  admin_level <- arg_match(admin_level)
  admin_level_cols <- get_admin_columns(admin_level)
  admin_level_cols <- c(admin_level_cols, 'year')

  pop_vars <- c('total_pop', 'under5_pop', 'under1_pop', 'live_births', 'total_births')
  vars <- c('opd_total', 'opd_under5', 'ipd_total', 'ipd_under5', 'under5_deaths', 'total_deaths')

  result <- .data %>%
    mutate(
      year_opd_rr = round(mean(opd_rr, na.rm = TRUE), 1),
      year_ipd_rr = round(mean(ipd_rr, na.rm = TRUE), 1),
      .by = all_of(admin_level_cols)
    ) %>%
    summarise(
      across(all_of(vars), sum, na.rm = TRUE),
      across(all_of(pop_vars), robust_max),
      across(starts_with('year_'), robust_max, .names = "{sub('^year_', '', .col)}"),
      .by = c(adminlevel_1, district, year),
    ) %>%
    summarise(
      across(all_of(c(vars, pop_vars)), if(admin_level == 'national') sum else mean, na.rm = TRUE),
      across(ends_with('_rr'), ~ if (admin_level == 'national') robust_max(.x) else mean(.x, na.rm = TRUE)),
      .by = all_of(admin_level_cols)
    ) %>%
    mutate(
      mean_opd_total = opd_total / total_pop,
      mean_ipd_total = ipd_total / total_pop,
      mean_opd_under5 = opd_under5 / under5_pop,
      mean_ipd_under5 = ipd_under5 / under5_pop,
      perc_opd_under5 = 100 * opd_under5 / opd_total,
      perc_ipd_under5 = 100 * ipd_under5 / ipd_total,
      cfr_under5 = 100 * under5_deaths / ipd_under5,
      cfr_total = 100 * total_deaths / ipd_total,
      prop_death = 100 * under5_deaths / total_deaths
    ) %>%
    select(-live_births, -under1_pop, -total_births)

  new_tibble(
    result,
    class = 'cd_service_utilization',
    admin_level = admin_level
  )
}

#' Format Service Utilization Data for Excel Export
#'
#' Transforms a `cd_service_utilization` object into a wide-format table
#' with labeled indicators, suitable for export to Excel.
#'
#' @param .data A `cd_service_utilization` object produced by [compute_service_utilization()].
#'
#' @return A tibble in wide format with:
#' - One row per indicator per admin unit
#' - One column per year
#' - A human-readable label for each indicator (`indiclabel`)
#'
#' @details
#' The output table includes population, OPD/IPD service utilization,
#' under-5 service metrics, reporting completeness, and case fatality rates,
#' reshaped to facilitate Excel review or reporting.
#'
#' @examples
#' \dontrun{
#'   x <- compute_service_utilization(dat, admin_level = "adminlevel_1")
#'   get_excel_version(x)
#' }
#'
#' @export

get_excel_version <- function(.data) {
  check_service_utilization(.data)

  admin_level <- attr_or_abort(.data, 'admin_level')
  admin_level_col <- get_admin_columns(admin_level)

  indicator_labels <- c(
    "total_pop"           = "Estimated population, all ages",
    "under5_pop"          = "Estimated population under-5",
    "mean_opd_total"      = "Mean OPD visits per person per year, all ages",
    "mean_ipd_total"      = "Mean IPD admissions per person per year, all ages",
    "opd_rr"              = "Completeness reporting OPD",
    "opd_total"           = "N of OPD visits per year, all ages",
    "opd_under5"          = "N of OPD visits per year, under-5",
    "mean_opd_under5"     = "Mean OPD visits per child per year, under-5",
    "perc_opd_under5"     = "Percent of OPD visits that are under-5",
    "ipd_rr"              = "Completeness reporting IPD",
    "ipd_total"           = "N of IPD admissions per year, all ages",
    "ipd_under5"          = "N of IPD admissions per year, under-5",
    "mean_ipd_under5"     = "Mean IPD admissions per child per year, under-5",
    "perc_ipd_under5"     = "Percent of IPD admissions that are under-5",
    "total_deaths"         = "N of deaths per year, all ages",
    "under5_deaths"        = "N of deaths in children under-5",
    "cfr_total"           = "CFR: deaths per 100 admissions per year, all ages",
    "cfr_under5"          = "CFR: deaths per 100 admissions per year, under-5",
    "prop_death"          = "Proportion of under-5 deaths (%): under-5 to total deaths"
  )

  .data %>%
    pivot_longer(
      cols = -any_of(c(admin_level_col, 'year')),
      names_to = "indic",
      values_to = "value"
    ) %>%
    pivot_wider( names_from = year, values_from = value) %>%
    mutate(indiclabel = factor(indicator_labels[indic], levels = unname(indicator_labels))) %>%
    arrange(!!!syms(admin_level_col)) %>%
    relocate(!!!syms(admin_level_col), indiclabel)
}
