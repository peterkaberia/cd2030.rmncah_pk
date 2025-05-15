#' Retrieve Attribute or Abort
#'
#' Safely retrieves an attribute from an object or aborts with an error.
#' @param .data An object
#' @param attr_name The name of the attribute to retrieve
#'
#' @return The attribute value
#'
#' @keywords internal
attr_or_abort <- function(.data, attr_name) {
  value <- attr(.data, attr_name)
  if (is.null(value)) {
    cd_abort(c("x" = str_glue("Missing attribute: {attr_name}")))
  }
  value
}

#' Get Country Name
#'
#' @param .data A `cd_data` object
#'
#' @return The country name as a character string
#'
#' @export
get_country_name <- function(.data) {
  check_cd_data(.data)
  attr_or_abort(.data, "country")
}

#' Get Country ISO3 Code
#'
#' @param .data A `cd_data` object
#'
#' @return The ISO3 country code as a character string
#'
#' @export
get_country_iso3 <- function(.data) {
  check_cd_data(.data)
  attr_or_abort(.data, "iso3")
}

#' Get Indicator Groups
#'
#' Default grouping of indicators used in CD2030 coverage framework
#'
#' @return A named list of grouped indicators
#'
#' @export
get_indicator_groups <- function() {
  list(
    anc = c("anc1", "anc_1trimester", "anc4", "ipt2", "ipt3", "syphilis_test", "ifa90", "hiv_test"),
    idelv = c("sba", "instdeliveries", "instlivebirths", "csection", "low_bweight", "pnc48h", "total_stillbirth", "stillbirth_f", "stillbirth_m", "maternal_deaths", "neonatal_deaths"),
    vacc = c("penta1", "penta3", "measles1", "measles2", "bcg"),
    opd = c("opd_total", "opd_under5"),
    ipd = c("ipd_total", "ipd_under5")
  )
}

#' Get Indicator Group Names
#' @return Character vector of indicator group names
#' @export
get_indicator_group_names <- function() names(get_indicator_groups())

#' @title Get All Indicators
#' @description Flatten all indicators from all groups
#' @return Character vector of all indicators
#' @export
get_all_indicators <- function() sort(list_c(get_indicator_groups()))

#' @title Get Indicators excluding indicators without denominator
#' @description Flatten all indicators from all groups
#' @return Character vector of all indicators
#' @export
get_indicator_without_opd_ipd <- function()  {
  groups <- get_indicator_groups()
  indicators <- sort(list_c(groups[!names(groups) %in% c("ipd", "opd")]))
  indicators[!indicators %in% c('sba', "total_stillbirth", "stillbirth_f", "stillbirth_m", "maternal_deaths", "neonatal_deaths")]
}

#' @title Get Named Indicator Vector
#' @description Each indicator is named by its group
#' @return Named character vector of indicators
#' @export
get_named_indicators <- function() {
  groups <- get_indicator_groups()
  out <- list_c(groups)
  names(out) <- rep(names(groups), lengths(groups))
  out
}

#' Get Population Denominator Column Based on Indicator Only
#'
#' @param indicator Character scalar. Indicator name (e.g., 'penta1', 'measles1')
#' @param denominator Character scalar. The denominator for the indicator
#'
#' @return A string naming the corresponding population column
#'
#' @export
get_population_column <- function(indicator, denominator) {
  indicator <- arg_match(indicator, get_all_indicators())
  denominator <- arg_match(denominator, c("dhis2", "anc1", "penta1", "penta1derived"))
  population <- case_match(
    indicator,
    c("anc1", "anc_1trimester", "anc4", "ipt2", "ipt3", "ifa90", "syphilis_test", "hiv_test") ~ "totpreg",
    c("bcg", "instlivebirths", "low_bweight", "pnc48h", "bcg") ~ ifelse(denominator == 'dhis2', 'totbirths', "totlbirths"),
    c("instdeliveries", "csection") ~ 'totdeliv',
    c("penta1", "penta3") ~ "totinftpenta",
    c("measles1", "measles2") ~ "totinftmeasles"
  )
  if (is.na(population)) {
    return(NA_character_)
  }
  return(paste(population, denominator, sep = "_"))
}

#' Get Grouping Columns by Administrative Level
#'
#' Returns the appropriate column(s) to use for grouping data based on the selected
#' administrative level. This utility is commonly used in functions that need to dynamically
#' group or filter data by administrative hierarchy.
#'
#' @param admin_level A character string specifying the administrative level.
#'   Must be one of `"national"`, `"adminlevel_1"`, or `"district"`.
#'
#' @return A character vector of column names to group by, or `NULL` if `admin_level = "national"`.
#'
#' @examples
#' get_admin_columns("national")
#' #> NULL
#'
#' get_admin_columns("adminlevel_1")
#' #> "adminlevel_1"
#'
#' get_admin_columns("district")
#' #> c("adminlevel_1", "district")
#'
#' @export
get_admin_columns <- function(admin_level) {
  admin_level <- arg_match(admin_level, c("national", "adminlevel_1", "district"))
  switch(admin_level,
    national = NULL,
    adminlevel_1 = "adminlevel_1",
    district = c("adminlevel_1", "district")
  )
}
