#' Create Mortality Summary Object
#'
#' Combines national and admin1 summaries and formats as a custom class for plotting.
#'
#' @param .data An object of class `cd_data`.
#'
#' @return A `cd_mortality_rare` tibble.
#'
#' @export
create_mortality_summary <- function(.data) {
  check_cd_data(.data)

  scatter <- bind_rows(
    summarised_data(.data, 'adminlevel_1'),
    summarised_data(.data, 'national')
  ) %>%
    mutate(adminlevel_1 = if_else(is.na(adminlevel_1), 'National', adminlevel_1))

  new_tibble(
    scatter,
    class = 'cd_mortality_rate'
  )
}

#' Extract Latest Mortality Ratios for UN Comparison
#'
#' Joins computed national mortality indicators with UN estimates, returns latest value per indicator.
#'
#' @param .data The main mortality dataset.
#' @param mortality_data A `cd_un_mortality` object.
#'
#' @return A tibble of class `cd_mortality_ratio` with latest available values and UN means.
#'
#' @export
create_mortality_ratios <- function(.data, mortality_data) {
  check_un_mortality(mortality_data)

  un_national <- summarised_data(.data) %>%
    summarise(across(c('sbr_inst', 'mmr_inst'), mean, na.rm = TRUE), .by = year) %>%
    mutate(
      mean_mmr = mean(mmr_inst, na.rm = TRUE),
      mean_sbr = mean(sbr_inst, na.rm = TRUE)
    ) %>%
    left_join(mortality_data, join_by(year)) %>%
    select(-contains('inst'), -contains('nmr'))

  un_national %>%
    select(-contains('mean')) %>%
    pivot_longer(cols = -year, names_to = 'indicator', values_to = 'values') %>%
    group_by(indicator) %>%
    filter(!is.na(values)) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    left_join(un_national %>% select(year, contains('mean')), join_by(year)) %>%
    new_tibble(class = 'cd_mortality_ratio')
}

#' Summarise Mortality and Birth Data by Administrative Level
#'
#' Aggregates key mortality and birth statistics by year and admin level,
#' and computes key indicators like MMR, SBR, and neonatal death rate.
#'
#' @param .data A data frame containing `instlivebirths`, `total_stillbirth`, `maternal_deaths`, `neonatal_deaths`, and `year`.
#' @param admin_level One of `"national"`, `"adminlevel_1"`, or `"district"`.
#'
#' @return A summarised data frame with calculated rates.
#'
#' @noRd
summarised_data <- function(.data, admin_level = c('national', 'adminlevel_1', 'district')) {
  check_cd_data(.data)
  admin_level <- arg_match(admin_level)
  admin_level_col <- get_admin_columns(admin_level)

  .data %>%
    summarise(
      across(c(instlivebirths, total_stillbirth, maternal_deaths, neonatal_deaths), sum, na.rm = TRUE),
      .by = all_of(c(admin_level_col, 'year'))
    ) %>%
    mutate(
      sbr_inst = 1000 * total_stillbirth/(instlivebirths+total_stillbirth),
      mmr_inst = 100000 * maternal_deaths/instlivebirths,
      nn_inst = 1000 * neonatal_deaths/instlivebirths,
      ratio_md_sb = total_stillbirth/maternal_deaths,
      mmr_low = if_else(mmr_inst < 25, 1, 0),
      sbr_low = if_else(sbr_inst < 6, 1, 0)
    ) %>%
    relocate(all_of(c(admin_level_col, 'year')))
}

# Extract summary statistics from UN data using tidy logic
extract_un_summary <- function(un_data) {
  un_data %>%
    pivot_longer(cols = -year, names_to = 'indicator', values_to = 'values') %>%
    group_by(indicator) %>%
    filter(!is.na(values)) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    mutate(
      level = case_when(
        str_detect(indicator, 'lb') ~ '::Lower bound of the estimate',
        str_detect(indicator, 'ub') ~ '::Upper bound of the estimate',
        .default = 'UN best estimate, 2020/2022'
      ),
      indicator = str_to_upper(str_remove(indicator, "_(lb|ub)$"))
    ) %>%
    arrange(year) %>%
    pivot_wider(
      names_from = c(indicator, year),
      values_from = values,
      names_glue = '{indicator}({year})'
    )
}

aggregate_indicator <- function(.data, indicators) {
  .data %>%
    pivot_longer(cols = any_of(indicators), names_to = 'indicator', values_to = 'val') %>%
    summarise(val = sum(val, na.rm = TRUE), .by = c(year, indicator)) %>%
    pivot_wider(names_from = year, values_from = val) %>%
    relocate(indicator)
}



derive_ratio <- function(.data, admin_level = c('national', 'adminlevel_1', 'district')) {
  admin_level <- arg_match(admin_level)
  admin_level_col <- get_admin_columns(admin_level)

  summarised_data(dat, admin_level) %>%
    select(all_of(admin_level_col), year, mmr_inst, sbr_inst, nn_inst, ratio_md_sb) %>%
    pivot_longer(cols = -all_of(c(admin_level_col, 'year')), values_to = 'values', names_to = 'indicator') %>%
    pivot_wider(names_from = year, values_from = values)
}

threshold_metric <- function(.data, admin_level = c('adminlevel_1', 'district')) {
  admin_level <- arg_match(admin_level)

  .data %>%
    summarised_data(admin_level) %>%
    summarise(
      mmr_lowr = mean(mmr_low, na.rm = TRUE) * 100,
      sbr_lowr = mean(sbr_low, na.rm = TRUE) * 100,
      .by = year
    ) %>%
    pivot_longer(cols = -year, values_to = 'values', names_to = 'indicator') %>%
    pivot_wider(names_from = year, values_from = values)
}

calculate_summary <- function(.data) {
  check_cd_data(.data)

  bind_rows(
    # To add stillbirth rate for the latest year
    aggregate_indicator(dat, c('instlivebirths', 'total_stillbirth', 'maternal_deaths', 'neonatal_deaths')),
    derive_ratio(dat),
    threshold_metric(dat)
  )
}
