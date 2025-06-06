load_public_private <- function(path, country_iso) {
  check_required(country_iso)

  data <- load_data_or_file(path)

  data %>%
    filter(iso == country_iso) %>%
    separate_wider_regex(
      level,
      patterns = c("[0-9]*", "\\s*", area = ".*")
    ) %>%
    mutate(
      sector = case_when(
        str_ends(indic, 'pub') ~ 'Public',
        str_ends(indic, 'priv') ~ 'Private',
        .ptype = factor(levels = c('Public', 'Private'))
      ),
      indic = str_remove(indic, 'pub|priv'),
      area = factor(str_to_title(area), levels = c('Urban', 'Rural'))
    ) #%>%
    # pivot_wider(
    #   names_from = sector,
    #   values_from = c(r_raw, se_raw, ll, ul, pop, N)
    # )
}
