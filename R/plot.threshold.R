#' @export
plot.cd_threshold <- function(x, ...) {

  indicator <- attr_or_abort(x, "indicator")
  admin_level <- attr_or_abort(x, "admin_level")
  region <- attr_or_null(x, 'region')
  admin_level <- switch(admin_level,
    adminlevel_1 = if (is.null(region)) "Admin Level 1" else 'Districts',
    district = "Districts"
  )

  title <- if (is.null(region)) {
    str_glue('Pecentage of {admin_level} with Coverage > 90%')
  } else {
    str_glue('Pecentage of {admin_level} in {region} with Coverage > 90%')
  }

  x %>%
    pivot_longer(
      cols = starts_with('cov_'),
      names_prefix = 'cov_',
      names_sep = '_(?=[^_]+$)',
      names_to = c('indicator', NA)
    ) %>%
    ggplot(aes(indicator, value, fill = factor(year))) +
    scale_y_continuous(breaks = scales::pretty_breaks(6), expand = c(0, 0)) +
    geom_col(position = "dodge") +
    geom_hline(yintercept = 80, colour = "red", size = 1.5) +
    labs(
      title = title,
      x = "",
      y = ""
    ) +
    scale_fill_manual(values = c("darkgreen", "darkgoldenrod3", "firebrick4", "springgreen3", "darkolivegreen3", "steelblue2")) +
    cd_plot_theme()
}
