#' Plot Missing Summary
#'
#' This method visualizes missing results for immunization indicators
#'
#' @param x A `cd_outlier` object containing pre-processed outlier data.
#' @param indicator Optional. One of the supported indicators (`'opv1'`, `'penta3'`, etc.)
#'   to visualize in the plot. If `NULL` all indicators will be shown.
#' @param ... Reserved for future use.
#'
#' @return A `ggplot` or `plotly` object depending on the selection type.
#'
#' @examples
#' \dontrun{
#' # Region-level summary
#' plot(missing_data, indicator = 'penta3')
#' }
#'
#' @export
plot.cd_completeness_summary <- function(x,
                                         indicator = NULL,
                                         ...) {
  admin_level <- attr_or_abort(x, 'admin_level')
  region <- attr_or_null(x, 'region')
  admin_level_col <- get_plot_admin_column(admin_level, region)

  indicator <- if (is.null(indicator) || indicator == '') {
    NULL
  } else {
    arg_match(indicator, get_all_indicators())
  }

  if (is.null(indicator)) {
    x <- x %>%
      summarise(across(starts_with('mis_'), ~ round(mean(.x, na.rm = TRUE))), .by = all_of(admin_level_col)) %>%
      pivot_longer(cols = starts_with('mis_'), names_to = 'indicator') %>%
      mutate(indicator = str_remove(indicator, 'mis_'))

    ggplot(x, aes(x = !!sym(admin_level_col), y = indicator, fill = value)) +
      geom_tile(color = 'white') +
      geom_text(aes(label = value), color = 'black', size = 3, vjust = 0.5) +
      scale_fill_gradient(low = 'red3', high = 'forestgreen', limits = c(0, 100)) +
      labs(x = admin_level_col, y = 'Indicator', fill = 'Value') +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))
  } else {
    column_name <- paste0('mis_', indicator)
    ggplot(x, aes(x = !!sym(admin_level_col), y = factor(year), fill = !!sym(column_name))) +
      geom_tile(color = 'white') +
      geom_text(aes(label = !!sym(column_name)), color = 'black', size = 3, vjust = 0.5) +
      scale_fill_gradient(low = 'red3', high = 'forestgreen', limits = c(0, 100)) +
      labs(x = admin_level_col, y = 'Year', fill = paste0(indicator, ' Value')) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))
  }
}
