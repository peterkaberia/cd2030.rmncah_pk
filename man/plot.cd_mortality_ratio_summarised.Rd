% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.mortality.R
\name{plot.cd_mortality_ratio_summarised}
\alias{plot.cd_mortality_ratio_summarised}
\title{Plot Completeness of Facility Reporting Ratios}
\usage{
\method{plot}{cd_mortality_ratio_summarised}(x, ...)
}
\arguments{
\item{x}{A \code{cd_mortality_ratio_summarised } object from completeness estimation.}

\item{...}{Additional arguments (not used).}
}
\value{
A ggplot object with ratio lines, labels, and reference points.
}
\description{
Calculates and plots the estimated completeness of facility reporting
for maternal deaths or stillbirths based on UN estimates and assumed
community-to-institution ratios.
}
