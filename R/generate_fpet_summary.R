#' Load and Generate FPET Output
#'
#' Processes raw FPET data (e.g., from Track20 or UNFPA) into a structured tibble
#' for visualization and interpretation of modern contraceptive prevalence and
#' demand satisfied.
#'
#' @param .data An object of class `cd_fpet_data`.
#'
#' @return A `cd_fpet` object.
#'
#' @export
generate_fpet_summary <- function(.data) {
  check_cd_class(.data, 'cd_fpet_data')

  country <- .data %>%
    distinct(country) %>%
    pull(country)

  .data %>%
    filter(`Marital status` == "married") %>%
    select(
      Year, Percentile,
      `Prevalence of Modern Methods (mCPR) (%)`,
      `Demand Satisfied with a Modern Method (%)`
    ) %>%
    pivot_longer(
      cols = c(
        `Prevalence of Modern Methods (mCPR) (%)`,
        `Demand Satisfied with a Modern Method (%)`
      ),
      names_to = "indicator",
      values_to = "value"
    ) %>%
    pivot_wider(
      names_from = Percentile,
      values_from = value
    ) %>%
    filter(Year > 1999) %>%
    rename(
      year = Year,
      lower = `0.025`,
      median = `median`,
      upper = `0.975`
    ) %>%
    new_tibble(class = 'cd_fpet', country = country)
}


#' Plot FPET Estimates
#'
#' Creates a line plot for modern contraceptive prevalence rate (mCPR) over time,
#' optionally faceted by subnational region.
#'
#' @param x A `cd_fpet` object.
#' @param ... Additional arguments for customization (not used).
#'
#' @return A ggplot2 object.
#'
#' @export
plot.cd_fpet <- function(x, ...) {
  check_cd_fpet(x)

  country <- attr_or_abort(x, 'country')

  x %>%
    ggplot(aes(x = year, color = indicator, fill = indicator)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, colour = NA) +
      geom_line(aes(y = median), linewidth = 1.2) +
      geom_point(aes(y = median), size = 2) +
      labs(
        title = str_glue('Family planning among currently married women 15-49 years, {country}'),
        y = 'Percent of currently married women 15-49',
        x = 'Year',
        color = 'Indicator',
        fill = 'Indicator',
        caption = 'Lines show the median estimates. Shaded bands represent the 95% credible interval.\nSource: FPET Track20 modeling'
      ) +
      cd_plot_theme()
}

#' Generic Interpretation Method
#'
#' A generic method to extract narrative interpretation from structured data objects.
#'
#' @param x An object.
#' @param ... Additional arguments passed to methods.
#'
#' @export
interpret <- function(x, ...) {
  UseMethod("interpret")
}

#' Interpret FPET Results with Emphasis on Current Estimates
#'
#' Summarizes 2020 and 2024 estimates, and briefly mentions FPET projections to 2030.
#'
#' @param x A `cd_fpet` object.
#' @param ... Unused.
#'
#' @return A character string with interpretation summary.
#'
#' @export
interpret.cd_fpet <- function(x, ...) {
  check_cd_fpet(x)
  country <- attr_or_abort(x, "country")

  focus_years <- c(2020, 2024)

  summary_df <- x %>%
    filter(year %in% focus_years) %>%
    select(indicator, year, median) %>%
    pivot_wider(names_from = year, values_from = median, names_prefix = "y") %>%
    mutate(
      change = y2024 - y2020,
      direction = case_when(
        change > 0 ~ "increased",
        change < 0 ~ "decreased",
        TRUE ~ "remained stable"
      ),
      change = round(change, 3)
    )

  # Summary text
  trend_text <- paste0(
    summary_df$indicator, ": ", summary_df$direction,
    " by ", summary_df$change, " points"
  )

  str_glue(
    "In {country}, between 2020 and 2024:\n",
    "{paste(trend_text, collapse = '\n')}\n\n",
    "The graphs also include FPET projections with credible intervals through 2030."
  ) %>%
    as.character()
}
