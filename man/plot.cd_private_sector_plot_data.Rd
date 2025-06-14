% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.private_sector.R
\name{plot.cd_private_sector_plot_data}
\alias{plot.cd_private_sector_plot_data}
\title{Plot Private Sector Prevalence by Sector and Area/National Level}
\usage{
\method{plot}{cd_private_sector_plot_data}(x, ..., country_name = NULL)
}
\arguments{
\item{x}{A data frame of class \code{"cd_private_sector_plot_data"} generated by
\code{\link[=prepare_private_sector_plot_data]{prepare_private_sector_plot_data()}}.}

\item{...}{Reserved for future use.}

\item{country_name}{Optional character string to override the default country name used
in the plot title. If \code{NULL}, uses the unique value in the \code{country} column if present.}
}
\value{
A \code{ggplot} object with:
\itemize{
\item Stacked bars for public/private sector share
\item Facets by indicator for area-level plots
\item Annotated private sector shares as text labels
}
}
\description{
Generates a stacked bar chart for RMNCAH indicators comparing public vs. private sector
share by either national or area-level (urban/rural) estimates.
}
\details{
The function automatically detects whether the data is at the \code{"national"} or \code{"area"}
level using the \code{"level"} attribute. It:
\itemize{
\item Maps indicator codes to descriptive labels
\item Stacks \code{r_raw_Public} and \code{r_raw_Private} values
\item Adds text annotations for private sector shares
}

For area-level data, bars are grouped by \code{area} and faceted by indicator.
For national-level data, each indicator has a single stacked bar.
}
\examples{
\dontrun{
dt <- load_private_sector_data("Area estimates.dta", country_iso = "TZA", level = "area")
cs <- load_csection_estimates("csection_area.dta", country_iso = "TZA", level = "area")
plot_data <- prepare_private_sector_plot_data(dt, cs)
plot(plot_data)
}

}
