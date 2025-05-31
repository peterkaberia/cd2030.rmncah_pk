#' Plot Subnational Coverage Maps by Indicator
#'
#' Creates a faceted `ggplot2` map showing subnational coverage levels of a health indicator,
#' colored using a gradient palette. Coverage values are drawn from a `cd_mapping_filtered`
#' object, typically filtered for a specific indicator and denominator.
#'
#' @param x A `cd_mapping_filtered` object. Created using [filter_mapping_data()], and must
#'   include spatial geometry and metadata attributes (`indicator`, `palette`, `column`).
#' @param ... Additional arguments (currently unused).
#'
#' @return A `ggplot` object visualizing the spatial distribution of the selected indicator by region and year.
#'
#' @examples
#' \dontrun{
#' # Assuming `map_data` is filtered with filter_mapping_data()
#' plot(map_data)
#' }
#'
#' @seealso [filter_mapping_data()], [get_mapping_data()]
#'
#' @export
plot.cd_mapping_filtered <- function(x, ...) {
  year <- NULL

  indicator <- attr_or_abort(x, 'indicator')
  palette <- attr_or_abort(x, 'palette')
  column <- attr_or_abort(x, 'column')

  title <- paste("Distribution of", indicator, " by Regions")

  x %>%
    st_set_geometry('geometry') %>%
    st_as_sf() %>%
    st_set_crs(4326) %>%
    st_transform(crs = 4326) %>%
    ggplot() +
      geom_sf(aes(fill = !!sym(column))) +
      coord_sf(default_crs = 4326, lims_method = "geometry_bbox") +
      facet_wrap(~ year, scales = "fixed", ncol = 5) +
      scale_fill_gradientn(colors = RColorBrewer::brewer.pal(7, palette)) +
      labs(
        title = title,
        fill = column,
        caption = "Data Source: DHIS-2 analysis"
      ) +
      cd_plot_theme() +
      theme(
        panel.border = element_blank(),
        panel.spacing = unit(1, "lines"),
        legend.key.size = unit(6, "mm"),
        legend.background = element_blank(),
        legend.title = element_text(size = 11),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        aspect.ratio = 1
      )
}
