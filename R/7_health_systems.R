#' Calculate Health System Metrics
#'
#' Aggregates data at national, admin level 1, or district level and computes
#' ratios and performance scores.
#'
#' @param .data A data frame with health system indicators and population data.
#' @param admin_level One of 'national', 'adminlevel_1', or 'district'.
#'
#' @return A summarized data frame with calculated metrics: ratios (facility,
#'   beds, workforce, service use) and performance scores.
#'
#' @export
calculate_health_system_metrics <- function(.data, admin_level = c('national', 'adminlevel_1', 'district')) {
  check_cd_data(.data)
  admin_level <- arg_match(admin_level)
  admin_level_cols <- get_admin_columns(admin_level)

  last_year <- robust_max(.data$year)

  allvars <- c('total_pop', 'total_facilities', 'total_hospitals', 'total_physicians', 'total_nonclinique_phys', 'total_nurses', 'total_beds', 'opd_total', 'ipd_total', 'under5_pop', 'opd_under5', 'ipd_under5')

  metrics <- .data %>%
    filter(year == last_year) %>%
    slice(1, .by = district) %>%
    select(adminlevel_1, district, year, -month, all_of(allvars)) %>%
    mutate(
      across(
        all_of(allvars),
        ~ if_else(is.na(.x), round(robust_max(median(.x, na.rm = TRUE)), 1), .x)
      ),
      .by = adminlevel_1
    ) %>%
    rename(
      total_nursemidwife = total_nurses,
      total_opd = opd_total,
      total_ipd = ipd_total,
      total_pop_u5 = under5_pop,
      total_opd_u5 = opd_under5,
      total_ipd_u5 = ipd_under5
    ) %>%
    mutate(total_healthstaff = rowSums(select(., total_physicians, total_nonclinique_phys, total_nursemidwife), na.rm = TRUE)) %>%
    summarise(across(-any_of(c('adminlevel_1', 'district', 'year')), sum, na.rm = TRUE), .by = all_of(c(admin_level_cols, 'year'))) %>%
    mutate(
      # Ratios
      ratio_fac_pop = (total_facilities / total_pop) * 10000,
      ratio_hos_pop = (total_hospitals / total_pop) * 100000,
      ratio_hstaff_pop = (total_healthstaff / total_pop) * 10000,
      ratio_phys_pop = (total_physicians / total_pop) * 10000,
      ratio_nursemidwife_pop = (total_nursemidwife / total_pop) * 10000,
      ratio_bed_pop = (total_beds / total_pop) * 10000,
      ratio_opd_pop = total_opd / total_pop,
      ratio_ipd_pop = (total_ipd / total_pop) * 100,
      ratio_opd_u5_pop = total_opd_u5 / total_pop_u5,
      ratio_ipd_u5_pop = (total_ipd_u5 / total_pop_u5) * 100,

      # Scores
      score_infrastructure = (((ratio_fac_pop / 2) + (ratio_bed_pop / 25)) / 2) * 100,
      score_workforce = (ratio_hstaff_pop / 23) * 100,
      score_utilization = (((ratio_opd_pop / 5) + (ratio_ipd_pop / 10)) / 2) * 100,

      score_infrastructure = if_else(score_infrastructure > 100, 100, score_infrastructure),
      score_workforce = if_else(score_workforce > 100, 100, score_workforce),
      score_utilization = if_else(score_utilization > 100, 100, score_utilization),
      score_total = (score_infrastructure + score_workforce + score_utilization) / 3,
      score_total = if_else(score_total > 100, 100, score_total)
    )

  new_tibble(
    metrics,
    class = 'cd_health_system_metric',
    admin_level = admin_level
  )
}

#' Compare Health System Metrics Across Levels
#'
#' Returns a merged dataset of health coverage and system metrics at district and
#' admin level 1 for the latest year.
#'
#' @param .data A data frame containing raw health system inputs including year,
#'   population, coverage, and facility indicators.
#'
#' @return A data frame with admin 1 and district metrics joined side by side for
#'   comparison.
#'
#' @export
calculate_health_system_comparison <- function(.data) {

  check_cd_data(.data)

  last_year <- robust_max(.data$year)

  # nat <- calculate_indicator_coverage(.data, un_estimates = un_est) %>%
  #   select(year, matches('^cov_(anc1|sba|instdeliveries|csection|pnc48|penta3)_(anc1|dhis2|penta1)$')) %>%
  #   rename_with(~ paste0('nat_', .x), .cols = starts_with('cov_'))

  ad1_cov <- calculate_indicator_coverage(.data, admin_level = 'adminlevel_1') %>%
    select(year, adminlevel_1, matches('^cov_(anc1|sba|instdeliveries|csection|pnc48|penta3)_(anc1|dhis2|penta1)$')) %>%
    rename_with(~ paste0('ad1_', .x), .cols = starts_with('cov_'))

  dis_cov <- calculate_indicator_coverage(.data, admin_level = 'district') %>%
    select(year, adminlevel_1, district, matches('^cov_(anc1|sba|instdeliveries|csection|pnc48|penta3)_(anc1|dhis2|penta1)$')) %>%
    rename_with(~ paste0('dis_', .x), .cols = starts_with('cov_'))

  ad1_metric <- calculate_health_system_metrics(.data, admin_level = 'adminlevel_1') %>%
    select(year, adminlevel_1, starts_with('ratio_')) %>%
    rename_with(~ paste0('ad1_', .x), .cols = starts_with('ratio_'))

  dis_metric <- calculate_health_system_metrics(.data, admin_level = 'district') %>%
    select(year, adminlevel_1, district, starts_with('ratio_')) %>%
    rename_with(~ paste0('dis_', .x), .cols = starts_with('ratio_'))

  metrics <- dis_cov %>%
    left_join(ad1_cov, join_by(adminlevel_1, year)) %>%
    # left_join(nat, join_by(year)) %>%
    left_join(ad1_metric, join_by(adminlevel_1, year)) %>%
    left_join(dis_metric, join_by(adminlevel_1, district, year)) %>%
    filter(year == last_year)

  new_tibble(
    metrics,
    class = 'cd_health_system_comparison'
  )
}
