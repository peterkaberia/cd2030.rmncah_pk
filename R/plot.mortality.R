#' Plot Mortality Rate Indicators
#'
#' Produces line plots for national mortality indicators with regional points for comparison.
#'
#' @param x A `cd_mortality_rate` object.
#' @param indicator One of `"mmr_inst"`, `"ratio_md_sb"`, `"sbr_inst"`, or `"nn_inst"`.
#' @param ... Additional arguments passed to methods.
#'
#' @return A ggplot object.
#'
#' @export
plot.cd_mortality_rate <- function(x, indicator = c('mmr_inst', 'ratio_md_sb', 'sbr_inst', 'nn_inst'), ...) {
  indicator <- arg_match(indicator)

  label <- switch(
    indicator,
    mmr_inst = 'National inst. MMR',
    nn_inst = 'National NN',
    sbr_inst = 'National inst. SBR',
    ratio_md_sb = 'National SB/MM ratio'
  )

  title <- switch(
    indicator,
    mmr_inst = 'Figure 5a: Maternal mortality per 100,000 live births in health facilities',
    ratio_md_sb = 'Figure 5b: Ratio number of stillbirths to maternal deaths in health facilities',
    sbr_inst = 'Figure 5c: Stillbirths per 1,000 births in health facilities',
    nn_inst = 'Figure 5d: Neonatal deaths before discharge per 1,000 live births in health facilities'
  )

  national <- x %>% filter(adminlevel_1 == 'National')
  regional <- x %>% filter(adminlevel_1 != 'National')
  max_y <- robust_max(x[[indicator]])
  max_y <- if (max_y < 100) 100 else max_y * 1.05

  ggplot() +
    geom_point(data = regional, aes(x = year, y = !!sym(indicator), color = 'Regions'), size = 2, alpha = 0.7) +
    geom_line(data = national, aes(x = year, y = !!sym(indicator), color = label), linewidth = 1.2) +
    geom_point(data = national, aes(x = year, y = !!sym(indicator), color = label), size = 2) +
    geom_text(
      data = national,
      aes(x = year, y = !!sym(indicator), label = round(!!sym(indicator), 1)),
      color = 'black', vjust = -0.5, hjust = -0.1, size = 3
    ) +
    labs(
      title = title,
      x = NULL,
      y = NULL
    ) +
    scale_y_continuous(limits = c(0, max_y), breaks = scales::pretty_breaks(n = 11), expand = expansion(mult = c(0, 0.05))) +
    scale_color_manual(values = set_names(c('forestgreen', 'orangered'), c('Regions', label))) +
    cd_plot_theme()
}


#' Plot Completeness of Facility Reporting Ratios
#'
#' Calculates and plots the estimated completeness of facility reporting
#' for maternal deaths or stillbirths based on UN estimates and assumed
#' community-to-institution ratios.
#'
#' @param x A `cd_mortality_ratio_summarised ` object from completeness estimation.
#' @param ... Additional arguments (not used).
#'
#' @return A ggplot object with ratio lines, labels, and reference points.
#'
#' @export
plot.cd_mortality_ratio_summarised <- function(x, ...) {

  plot_type <- attr_or_abort(x, 'plot_type')

  lower_bound <- paste0('UN ', str_to_upper(plot_type),' lower bound')
  upper_bound <- paste0('UN ', str_to_upper(plot_type),' upper bound')
  best_estimates <- paste0('UN ', str_to_upper(plot_type),' best estimate')

  labels <- list(
    sbr = list(
      title = 'Fig 5f: Completeness of facility stillbirth reporting (%), based on UN stillbirth estimates and community to institutional ratio',
      x = 'Ratio Community SBR to Institutional SBR',
      y = 'Completeness stillbirth reporting by facilities (%)'
    ),
    mmr = list(
      title = 'Fig 5e: Completeness of facility maternal death reporting (%), based on UN MMR estimates and community to institutional ratio',
      x = 'Ratio Community MMR to Institutional MMR',
      y = 'Completeness maternal deaths reporting by facilities (%)'
    )
  )

  label_values <- labels[[plot_type]]

  data <- x %>%
    pivot_longer(cols = -ciratio, values_to = 'rat', names_to = 'name') %>%
    mutate(name = factor(name, levels = c(lower_bound, best_estimates, upper_bound)))

  max_y <- robust_max(data$rat)
  max_y <- if (max_y < 100) 100 else max_y * 1.05

  data %>%
    ggplot(aes(x = ciratio, y = rat, colour = name)) +
    geom_line(size = 2) +
    geom_point(size = 13) +
    geom_text(aes(label = rat), color = "black", size = 4) +
    scale_y_continuous(limits = c(0, max_y), breaks = scales::pretty_breaks(n = 10), expand = expansion(mult = c(0,0.05))) +
    labs(
      title = label_values$title,
      x = label_values$x,
      y = label_values$y
    ) +
    cd_plot_theme()
}
