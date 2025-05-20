#' Plot Service Utilization Indicators
#'
#' Visualizes service utilization over time from a `cd_service_utilization` object.
#'
#' @param x A `cd_service_utilization` object created by [compute_service_utilization()].
#' @param plot_type One of:
#'   - `"opd"`: OPD visits per person
#'   - `"ipd"`: IPD admissions per person
#'   - `"under5"`: Under-5 share of OPD and IPD
#'   - `"cfr"`: Case fatality rate (under-5 and total)
#'   - `"deaths"`: Proportion of deaths that are under-5
#' @param region Character description
#'
#' @return A `ggplot2` plot object.
#'
#' @examples
#' \dontrun{
#' plot(compute_service_utilization(dat), plot_type = "opd")
#' }
#'
#' @export
plot.cd_service_utilization <- function(x, plot_type = c('opd', 'ipd', 'under5', 'cfr', 'deaths'), region = NULL) {

  plot_type <- arg_match(plot_type)
  admin_level <- attr_or_abort(x, 'admin_level')

  if (admin_level == 'national' && !is.null(region)) {
    cd_abort('x' = '{.arg region} cannot be specified in {.field national} data.')
  }

  if (admin_level != 'national' && (is.null(region) || !is_scalar_character(region))) {
    cd_abort('x' = '{.arg region} must be a scalar string.')
  }

  if (admin_level != 'national') {
    x <- x %>%
      filter(!!sym(admin_level) == region)
  }

  labels <- list(
    opd = list(
      y1 = 'mean_opd_under5',
      y2 = 'mean_opd_total',
      y1_label = 'Under-5',
      y2_label = 'All ages',
      title = 'OPD visits',
      y_label = 'Mean # of OPD visits per person'
    ),
    ipd = list(
      y1 = 'mean_ipd_under5',
      y2 = 'mean_ipd_total',
      y1_label = 'Under-5',
      y2_label = 'All ages',
      title = 'IPD admissions',
      y_label = 'Mean # of IPD admissions per person'
    ),
    under5 = list(
      y1 = 'perc_opd_under5',
      y2 = 'perc_ipd_under5',
      y1_label = 'OPD visits',
      y2_label = 'IPD admissions',
      title = 'Percentage of under-5 OPD visits and IPD admissions',
      y_label = 'Percentage'
    ),
    cfr = list(
      y1 = 'cfr_under5',
      y2 = 'cfr_total',
      y1_label = 'CFR: Under-5',
      y2_label = 'CFR: All ages',
      title = 'Case fatality rate (%)',
      y_label = 'Percentage'
    ),
    deaths = list(
      y1 = 'prop_death',
      y2 = 'prop_death',
      y1_label = NULL,
      y2_label = NULL,
      title = 'Proportion of deaths (%): under-5 to all-age',
      y_label = 'Percentage'
    )
  )

  labels_val <- labels[[plot_type]]

  x %>%
    ggplot(aes(x = year)) +
    geom_point(aes(y = !!sym(labels_val$y1), color = labels_val$y1_label), size = 2) +
    geom_line(aes(y = !!sym(labels_val$y1), color = labels_val$y1_label), size = 1) +
    geom_point(aes(y = !!sym(labels_val$y2), color = labels_val$y2_label), size = 2) +
    geom_line(aes(y = !!sym(labels_val$y2), color = labels_val$y2_label), size = 1) +
    labs(title = labels_val$title, y = labels_val$y_label, x = 'Year') +
    cd_plot_theme()
}
