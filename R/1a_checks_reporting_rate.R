#' Reporting Rate Summary by Administrative Level
#'
#' `calculate_average_reporting_rate()` computes the average reporting rate for each indicator
#' across available years, disaggregated by the specified administrative level. It also
#' calculates an overall mean reporting rate per group.
#'
#' @param .data A tibble of class `cd_data`.
#' @param admin_level Character. The administrative level at which to calculate
#'   reporting rates. Must be one of `"national"`, `"adminlevel_1"` or `"district"`.
#'
#' @return A tibble of class `cd_average_reporting_rate`, containing:
#'   - One row per grouping (`year`, `adminlevel_1`, `district` as applicable).
#'   - One column per reporting indicator.
#'   - `mean_rr`: average of all indicators per group.
#'
#' @examples
#' \dontrun{
#' # Regional summary
#' calculate_average_reporting_rate(data, admin_level = "adminlevel_1")
#' }
#'
#' @export
calculate_average_reporting_rate <- function(.data,
                                             admin_level = c("national", "adminlevel_1", "district")) {
  . <- NULL

  check_cd_data(.data)

  admin_level <- arg_match(admin_level)
  admin_level_cols <- get_admin_columns(admin_level)
  allindicators <- get_indicator_group_names()
  indicators <- paste0(allindicators, "_rr")
  four_indicators <- paste0(allindicators[which(allindicators != 'ipd')], "_rr")

  reporting_rate <- .data %>%
    summarise(across(all_of(indicators), mean, na.rm = TRUE), .by = c(admin_level_cols, "year")) %>%
    mutate(
      mean_four_rr = rowMeans(select(., four_indicators), na.rm = TRUE),
      mean_rr = rowMeans(select(., indicators), na.rm = TRUE)
    ) %>%
    mutate(across(ends_with("_rr"), round, 0))

  new_tibble(
    reporting_rate,
    class = "cd_average_reporting_rate",
    admin_level = admin_level
  )
}

#' District-Level Reporting Rates by Year
#'
#' `calculate_district_reporting_rate` calculates the percentage of districts that
#' meet or exceed a specified reporting rate threshold for each indicator, by year.
#' Also computes an overall yearly mean.
#'
#' @param .data A tibble of class `cd_data`.
#' @param threshold Numeric. Minimum acceptable reporting rate (as a percentage).
#'   Default: 90.
#'
#' @return A tibble of class `cd_district_reporting_rate` with:
#'   - One row per year.
#'   - Columns `low_<indicator>`: percentage of districts meeting the threshold.
#'   - `low_mean_rr`: mean percentage of districts meeting the threshold across
#'      indicators.
#'
#' @examples
#' \dontrun{
#' # Calculate district-level reporting rates with a threshold of 90%
#' district_summary <- calculate_district_reporting_rate(data, threshold = 90)
#' }
#'
#' @export
calculate_district_reporting_rate <- function(.data, threshold = 90) {
  year <- district <- . <- NULL

  check_cd_data(.data)

  allindicators <- get_indicator_group_names()
  indicators <- paste0(allindicators, "_rr")
  four_indicators <- paste0('low_', allindicators[which(allindicators != 'ipd')], "_rr")

  reporting_rate <- .data %>%
    summarise(across(all_of(indicators), mean, na.rm = TRUE), .by = c(district, year)) %>%
    summarise(across(all_of(indicators), ~ mean(.x >= threshold, na.rm = TRUE) * 100, .names = "low_{.col}"), .by = year) %>%
    mutate(
      low_mean_rr = rowMeans(select(., starts_with("low_")), na.rm = TRUE),
      low_mean_four_rr = rowMeans(select(., any_of(four_indicators)), na.rm = TRUE)
    ) %>%
    mutate(across(starts_with("low_"), round, 0))

  new_tibble(
    reporting_rate,
    class = "cd_district_reporting_rate",
    threshold = threshold
  )
}
