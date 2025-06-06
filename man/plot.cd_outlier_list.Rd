% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.outlier.R
\name{plot.cd_outlier_list}
\alias{plot.cd_outlier_list}
\title{Plot Outlier Time Series for a Region}
\usage{
\method{plot}{cd_outlier_list}(x, region_name = NULL, ...)
}
\arguments{
\item{x}{A \code{cd_outlier_list} object from \code{list_outlier_units()}.}

\item{region_name}{The name of the unit to plot.}

\item{...}{Not used.}
}
\value{
A \code{ggplot} object.
}
\description{
Displays a time-series plot of one indicator for a single region or district,
with outlier highlights.
}
\details{
\itemize{
\item Plots observed values, median trend, and 5×MAD range.
\item Flags outliers in red.
}
}
\examples{
\dontrun{
list_outlier_units(cd_data, "penta1") \%>\%
  plot(region_name = "Nakuru")
}
}
